{-# LANGUAGE OverloadedStrings #-}

-- | A 'Repl' is a persistent process handle with free dual ends.
--
-- 'replCommit' writes TO the process. 'replEmit' reads FROM it.
-- No request–response contract, no timeouts, no claim tokens.
-- Transport (FIFO, PTY, WebSocket, inject) is captured at construction.
module Circuit.Repl
  ( -- * Handle
    Repl (..),
    replCommit,
    replEmit,
    replClose,

    -- * Categorical dual
    endsRepl,
    Commit,
    Emit,

    -- * Configuration
    ReplConfig (..),
    defaultReplConfig,

    -- * Constructors
    replOpen,
    replOpenPty,
    replOpenHermes,
    replOpenCustom,
    replOpenInject,
    replAttach,
  )
where

import Circuit.Queue (Commit, Emit, WireK)
import Circuit.Trace (Trace (..))
import Control.Arrow (Kleisli (..))

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (guard, unless, void, forM_)
import Cursor qualified as Cur
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO
  ( BufferMode (NoBuffering),
    IOMode (AppendMode, ReadMode, WriteMode),
    SeekMode (AbsoluteSeek),
    hClose,
    hFlush,
    hSetBuffering,
    openFile,
    withFile,
  )
import System.IO.Error (userError)
import System.Posix.IO
  ( OpenFileFlags (..),
    OpenMode (ReadWrite),
    closeFd,
    defaultFileFlags,
    openFd,
    waitToSetLock,
  )
import System.Posix.IO qualified as PIO (LockRequest (..))
import System.Posix.Process (getProcessID)
import System.Posix.Pty (Pty, closePty, spawnWithPty, tryReadPty, writePty)
import System.Posix.Types (FileMode)
import System.Process
import System.Timeout (timeout)
import Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data ReplConfig = ReplConfig
  { replCommand    :: String,
    replArgs       :: [String],
    replStdinPath  :: FilePath,
    replStdoutPath :: FilePath,
    replStderrPath :: FilePath,
    replWorkingDir :: FilePath
  }
  deriving (Show, Eq)

defaultReplConfig :: ReplConfig
defaultReplConfig = ReplConfig
  { replCommand    = "cabal",
    replArgs       = ["repl"],
    replStdinPath  = "/tmp/repl-stdin",
    replStdoutPath = "/tmp/repl-stdout.md",
    replStderrPath = "/tmp/repl-stderr.md",
    replWorkingDir = "."
  }

-- ---------------------------------------------------------------------------
-- Handle — three closures, no dispatch
-- ---------------------------------------------------------------------------

data Repl = Repl
  { replCommit :: [Text] -> IO (),
    replEmit   :: IO [Text],
    replClose  :: IO ()
  }

-- | Expose the dual as 'Circuit.Queue.Commit' and 'Emit' wires.
--
-- 'Commit' is contravariant in its input; 'Emit' is covariant in its output.
-- Compose freely.  A turn boundary circuit (timeout, prompt detector) lives
-- outside this module.
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Circuit (run)
-- >>> import Circuit.Classes ((>>>))
-- >>> import Circuit.Queue (Commit, Emit)
-- >>> import Circuit.Trace (Trace(..))
-- >>> import Control.Arrow (Kleisli(..), runKleisli)
-- >>> let r = Repl { replCommit = \_ -> pure (), replEmit = pure ["emit: hello"], replClose = pure () }
-- >>> let (write, read) = endsRepl r
-- >>> let send = Arr (Kleisli (\() -> pure ["ping"]))
-- >>> let turn = send >>> write >>> read
-- >>> runKleisli (run turn) ()
-- ["emit: hello"]
endsRepl :: Repl -> (Commit IO [Text], Emit IO [Text])
endsRepl r =
  ( Arr (Kleisli (replCommit r)),
    Arr (Kleisli (\() -> replEmit r))
  )

-- ---------------------------------------------------------------------------
-- Constructor: FIFO
-- ---------------------------------------------------------------------------

replOpen :: ReplConfig -> IO Repl
replOpen cfg = do
  ensureFifo (replStdinPath cfg)
  stdoutH <- openFile (replStdoutPath cfg) AppendMode
  stderrH <- openFile (replStderrPath cfg) AppendMode
  hSetBuffering stdoutH NoBuffering
  hSetBuffering stderrH NoBuffering
  stdinH <- openFile (replStdinPath cfg) ReadMode
  let procSpec = (proc (replCommand cfg) (replArgs cfg))
        { cwd     = Just (replWorkingDir cfg),
          std_in  = UseHandle stdinH,
          std_out = UseHandle stdoutH,
          std_err = UseHandle stderrH
        }
  (_, _, _, ph) <- createProcess procSpec
  hClose stdinH; hClose stdoutH; hClose stderrH

  cursor <- Cur.newFile (cursorPath cfg)
  Cur.set cursor 0
  lastP  <- newIORef Nothing

  let commit ts = mapM_ (\t -> withFile (replStdinPath cfg) WriteMode $ \h -> do
        TIO.hPutStrLn h t; hFlush h) ts
      emit = logEmit (replStdoutPath cfg) cursor lastP
      close = terminateProcess ph

  pure Repl { replCommit = commit, replEmit = emit, replClose = close }

-- ---------------------------------------------------------------------------
-- Constructor: PTY
-- ---------------------------------------------------------------------------

replOpenPty :: ReplConfig -> IO Repl
replOpenPty cfg = do
  createDirectoryIfMissing True (takeDirectory (replStdoutPath cfg))
  appendFile (replStdoutPath cfg) ""
  (pty, ph) <- spawnWithPty Nothing True (replCommand cfg) (replArgs cfg) (100, 30)
  pumpTid  <- forkIO (pumpPtyToLog pty (replStdoutPath cfg))

  cursor <- Cur.newFile (cursorPath cfg)
  Cur.set cursor 0
  lastP  <- newIORef Nothing

  let commit ts = mapM_ (\t -> writePty pty (encodeUtf8 (t <> "\n"))) ts
      emit   = logEmit (replStdoutPath cfg) cursor lastP
      close  = do
        void $ try @IOException (terminateProcess ph)
        void $ timeout 500_000 $ do
          void $ try @IOException (closePty pty)
          killThread pumpTid

  pure Repl { replCommit = commit, replEmit = emit, replClose = close }

-- ---------------------------------------------------------------------------
-- Constructor: Hermes session JSON
-- ---------------------------------------------------------------------------

replOpenHermes :: FilePath -> IO Repl
replOpenHermes path = do
  exists <- doesFileExist path
  unless exists $ throwIO . userError $ "replOpenHermes: not found: " <> path
  n <- hermesMessageCount path
  idx <- newIORef n
  let commit ts = hermesCommitPath path ts
      emit   = hermesEmitPath path idx
      close  = pure ()
  pure Repl { replCommit = commit, replEmit = emit, replClose = close }

-- ---------------------------------------------------------------------------
-- Constructor: custom (MusterRepl, tests)
-- ---------------------------------------------------------------------------

replOpenCustom :: ([Text] -> IO ()) -> IO [Text] -> IO () -> IO Repl
replOpenCustom fcommit femit fclose =
  pure Repl { replCommit = fcommit, replEmit = femit, replClose = fclose }

-- ---------------------------------------------------------------------------
-- Constructor: inject (tests)
-- ---------------------------------------------------------------------------

replOpenInject :: ReplConfig -> (Text -> IO ()) -> IO Repl
replOpenInject cfg inject = do
  appendFile (replStdoutPath cfg) ""
  cursor <- Cur.newFile (cursorPath cfg)
  Cur.set cursor 0
  lastP  <- newIORef Nothing
  let commit ts = mapM_ inject ts
      emit   = logEmit (replStdoutPath cfg) cursor lastP
      close  = pure ()
  pure Repl { replCommit = commit, replEmit = emit, replClose = close }

-- ---------------------------------------------------------------------------
-- Attach (read-only, uses existing FIFO)
-- ---------------------------------------------------------------------------

replAttach :: ReplConfig -> IO Repl
replAttach cfg = do
  content <- readLogContent (replStdoutPath cfg)
  let (complete, _) = splitComplete content
  path    <- attachCursorPath cfg
  cursor  <- Cur.newFile path
  Cur.seekEnd cursor complete
  lastP   <- newIORef Nothing

  let commit ts = mapM_ (\t -> withFile (replStdinPath cfg) WriteMode $ \h -> do
        TIO.hPutStrLn h t; hFlush h) ts
      emit  = logEmit (replStdoutPath cfg) cursor lastP
      close = pure ()  -- attach doesn't own the process

  pure Repl { replCommit = commit, replEmit = emit, replClose = close }

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

ensureFifo :: FilePath -> IO ()
ensureFifo path = do
  exists <- doesFileExist path
  unless exists $ callProcess "mkfifo" [path]

cursorPath :: ReplConfig -> FilePath
cursorPath cfg = replStdoutPath cfg <> ".cursor"

attachCursorPath :: ReplConfig -> IO FilePath
attachCursorPath cfg = do
  pid <- getProcessID
  pure (replStdoutPath cfg <> ".cursor-attach-" <> show pid)

pumpPtyToLog :: Pty -> FilePath -> IO ()
pumpPtyToLog pty logPath = go
  where
    go = do
      r <- try @IOException (tryReadPty pty)
      case r of
        Left _            -> pure ()
        Right (Left _)    -> go
        Right (Right bs)
          | BS.null bs    -> go
          | otherwise     -> BS.appendFile logPath bs >> go

readLogContent :: FilePath -> IO Text
readLogContent fp = do
  exists <- doesFileExist fp
  if not exists then pure "" else decodeUtf8 <$> BS.readFile fp

splitComplete :: Text -> ([Text], Maybe Text)
splitComplete content
  | T.null content                = ([], Nothing)
  | T.isSuffixOf "\n" content    = (T.lines content, Nothing)
  | otherwise                     =
      let parts = T.splitOn "\n" content
       in case parts of
            [] -> ([], Nothing)
            _  -> (init parts, Just (last parts))

logEmit :: FilePath -> Cur.Cursor -> IORef (Maybe Text) -> IO [Text]
logEmit logPath cursor lastP = do
  content <- readLogContent logPath
  let (complete, mPartial) = splitComplete content
  news <- Cur.pollLines cursor complete
  prev <- readIORef lastP
  writeIORef lastP mPartial
  let partialNews = case (news, prev, mPartial) of
        (_, _, Nothing)          -> []
        (_ : _, _, Just p)       -> [p]
        ([], Just old, Just p)
          | old == p             -> []
        ([], _, Just p)          -> [p]
  pure (news <> partialNews)

-- ---------------------------------------------------------------------------
-- Hermes session-file backend (no Backend type — standalone functions)
-- ---------------------------------------------------------------------------

withSessionLock :: FilePath -> IO a -> IO a
withSessionLock path action = bracket acquire release (const action)
  where
    lockPath = path <> ".lock"
    mode :: Maybe FileMode
    mode = Just 0o644
    acquire = do
      fd <- openFd lockPath ReadWrite defaultFileFlags {creat = mode}
      waitToSetLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
      pure fd
    release fd = do
      waitToSetLock fd (PIO.Unlock, AbsoluteSeek, 0, 0)
      closeFd fd

readSessionObject :: FilePath -> IO (KM.KeyMap Value)
readSessionObject path = do
  bs <- BS.readFile path
  case eitherDecodeStrict' bs of
    Left err -> throwIO . userError $ "hermes session decode: " <> err <> " (" <> path <> ")"
    Right (Object o) -> pure o
    Right _ -> throwIO . userError $ "hermes session: expected object (" <> path <> ")"

writeSessionObject :: FilePath -> KM.KeyMap Value -> IO ()
writeSessionObject path o = do
  let tmp = path <> ".tmp"
  LBS.writeFile tmp (encode (Object o))
  renameFile tmp path

hermesMessageCount :: FilePath -> IO Int
hermesMessageCount path = withSessionLock path $ do
  o <- readSessionObject path
  pure $ case KM.lookup "messages" o of
    Just (Array arr) -> V.length arr
    _ -> 0

hermesCommitPath :: FilePath -> [Text] -> IO ()
hermesCommitPath path ts = withSessionLock path $ do
  o <- readSessionObject path
  let oldMsgs = case KM.lookup "messages" o of
        Just (Array arr) -> arr
        _ -> V.empty
      added = V.fromList
        [ object ["role" .= ("user" :: Text), "content" .= t] | t <- ts ]
      newMsgs = oldMsgs <> added
      o' = KM.insert "messages" (Array newMsgs) $
           KM.insert "message_count" (Number (fromIntegral (V.length newMsgs))) o
  writeSessionObject path o'

hermesEmitPath :: FilePath -> IORef Int -> IO [Text]
hermesEmitPath path idx = withSessionLock path $ do
  o <- readSessionObject path
  let msgs = case KM.lookup "messages" o of
        Just (Array arr) -> arr
        _ -> V.empty
  i <- readIORef idx
  let new  = V.drop i msgs
      asst = mapMaybe assistantContent (V.toList new)
  writeIORef idx (V.length msgs)
  pure asst

assistantContent :: Value -> Maybe Text
assistantContent (Object m) = do
  role <- case KM.lookup "role" m of
    Just (String t) -> Just t
    _ -> Nothing
  guard (role == "assistant")
  case KM.lookup "content" m of
    Just (String t) | not (T.null (T.strip t)) -> Just t
    _ -> Nothing
assistantContent _ = Nothing
