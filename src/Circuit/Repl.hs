{-# LANGUAGE OverloadedStrings #-}

-- | A 'Repl' is a persistent process /token/ with two free operational ends.
--
-- The handle is three closures (no dispatch table):
--
-- * 'replCommit' — write TO the process (harness feed)
-- * 'replEmit' — read FROM the process (harness harvest)
-- * 'replClose' — release the token
--
-- No request–response contract, no timeouts, no claim tokens.
-- Transport (FIFO, PTY, inject) is captured at construction.
--
-- = Free ends vs unit-grounded view
--
-- Free channel ends in @circuits@ are 'Circuit.Ends.In' / 'Circuit.Ends.Out'.
-- 'endsRepl' exposes a /Trace view/ of the handle as unit-grounded
-- 'Circuit.Queue.Commit' / 'Circuit.Queue.Emit' wires
-- (@[Text] → ()@ / @() → [Text]@) — the same shapes as
-- 'Circuit.Ends.asCommit' / 'Circuit.Ends.asEmit', specialised to IO.
-- That view is optional: most code should use the three closures directly.
-- Turn boundaries ('Circuit.Repl.Turn.turnUntil') live /outside/ this module.
module Circuit.Repl
  ( -- * Handle
    Repl (..),
    replCommit,
    replEmit,
    replClose,

    -- * Trace view of the dual (unit-grounded)
    endsRepl,
    Commit,
    Emit,

    -- * Configuration
    ReplConfig (..),
    defaultReplConfig,

    -- * Constructors
    replOpen,
    replOpenPty,
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
import Control.Monad (unless, void, forM_)
import Cursor qualified as Cur
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import System.IO
  ( BufferMode (NoBuffering),
    IOMode (AppendMode, ReadMode, WriteMode),
    hClose,
    hFlush,
    hSetBuffering,
    openFile,
    withFile,
  )
import System.IO.Error (userError)
import System.Posix.Process (getProcessID)
import System.Posix.Pty (Pty, closePty, spawnWithPty, tryReadPty, writePty)
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

-- | Process token: free operational ends + close.
--
-- Orientation is relative to the process: commit feeds it, emit harvests it.
-- A runner may re-seat who is \"the process\" (polarity flip); the three
-- closures stay the API — see 'Circuit.Ends' mock 3.
data Repl = Repl
  { replCommit :: [Text] -> IO (),
    -- ^ Write TO the process (stdin / bus post / inject).
    replEmit   :: IO [Text],
    -- ^ Read FROM the process (stdout log / bus watch).
    replClose  :: IO ()
    -- ^ Release handles / kill child / detach.
  }

-- | Trace /view/ of a 'Repl' as unit-grounded 'Commit' / 'Emit' wires.
--
-- @
--   endsRepl r = ( Arr (Kleisli (replCommit r))   -- Commit IO [Text]  ≅ [Text] → ()
--                , Arr (Kleisli (\\() -> replEmit r)) -- Emit   IO [Text]  ≅ () → [Text]
--                )
-- @
--
-- These are /not/ free 'Circuit.Ends.In'/'Out' ends — they are the same
-- unit-grounded shapes as 'Circuit.Ends.asCommit' / 'Circuit.Ends.asEmit',
-- specialised to @'Kleisli' IO@ and carrier @'[Text]'@.  Use when composing
-- with other 'Trace' circuits; prefer 'replCommit'/'replEmit' at the app edge.
--
-- 'Commit' is contravariant in its input; 'Emit' is covariant in its output.
-- Turn boundary (timeout, prompt detector) lives outside this module
-- ('Circuit.Repl.Turn').
--
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

