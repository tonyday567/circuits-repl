{-# LANGUAGE OverloadedStrings #-}

-- | A 'Repl' is a process token. Free ends of its channel are 'In' / 'Out'.
--
-- The handle is three closures (ops convenience):
--
-- * 'replCommit' — write TO the process (harness feed)
-- * 'replEmit' — read FROM the process (harness harvest)
-- * 'replClose' — release the token
--
-- = In ⊣ Out
--
-- 'openRepl' is 'open' for that channel: free 'Out' / 'In' over
-- @'Kleisli' IO@ at @'[Text]'@ (extrinsic, same pattern as 'openSTM').
-- Unit plug recovers feed/harvest:
--
-- @
--   let (outR, inR) = openRepl r
--       (outU, inU) = openK ()
--   in ( runOut inR outU   -- [Text] → ()
--      , runIn  outR inU   -- () → [Text]
--      )
-- @
--
-- Turn boundaries live outside this module.
module Circuit.Repl
  ( -- * Handle
    Repl (..),
    replCommit,
    replEmit,
    replClose,

    -- * Open (free ends)
    openRepl,
    endsRepl,
    replEnds,

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

import Circuit.Ends (openK)
import Circuit.Layer (run)
import Circuit.Monoidal (Tensor (..))
import Circuit.Queue (Commit, Emit)
import Circuit.Trace (In (..), Out (..), Trace (..), runIn, runOut)
import Control.Arrow (Kleisli (..), runKleisli)

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

-- | Process token. Free ends of its text channel: 'openRepl'.
data Repl = Repl
  { replCommit :: [Text] -> IO (),
    -- ^ Write TO the process (stdin / bus post / inject).
    replEmit   :: IO [Text],
    -- ^ Read FROM the process (stdout log / bus watch).
    replClose  :: IO ()
    -- ^ Release handles / kill child / detach.
  }

-- | Free dual store for a 'Repl': two end *handles* (Haskell pair of values).
--
-- Not a monoidal object. The @(,)@ in the return type is only how we hand
-- you two independent ends for async feed ‖ harvest. Dual *channels* enter
-- a circuit with 'par' (see 'replEnds'), not by treating this pair as tensor.
--
-- Same extrinsic pattern as 'open' / 'openSTM': both polarities share the
-- process. Unit-plug with 'openK' @()@ for boring ports ('endsRepl').
--
-- Clarity ladder (no extra structure):
--
-- * 'openRepl' — free dual store (handles)
-- * 'endsRepl' — unit plug each → Commit / Emit (still handles)
-- * 'replEnds' — 'par' of those → one Trace morphism (wiring)
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Circuit (run, par)
-- >>> import Circuit.Classes ((>>>))
-- >>> import Circuit.Ends (openK)
-- >>> import Circuit.Trace (Trace (..), runIn, runOut)
-- >>> import Control.Arrow (Kleisli (..), runKleisli)
-- >>> let r = Repl { replCommit = \_ -> pure (), replEmit = pure ["emit: hello"], replClose = pure () }
-- >>> let (outR, inR) = openRepl r
-- >>> let (outU, inU) = openK ()
-- >>> let send = Arr (Kleisli (\() -> pure ["ping"]))
-- >>> let turn = send >>> runOut inR outU >>> runIn outR inU
-- >>> runKleisli (run turn) ()
-- ["emit: hello"]
openRepl :: Repl -> (Out (Kleisli IO) (,) [Text], In (Kleisli IO) (,) [Text])
openRepl r = (outR, inR)
  where
    -- Out: harvest from the process (ignore opposing In for the read).
    outR = Out $ \_ -> Arr (Kleisli $ \_ -> replEmit r)
    -- In: feed the process, then continue through the opposing Out (openSTM shape).
    inR =
      In $ \o ->
        Arr
          ( Kleisli $ \ts -> do
              replCommit r ts
              runKleisli (run (runIn o inR)) ts
          )

-- | Unit-plug free ends: boring 'Commit' / 'Emit' handles (still a pair of values).
--
-- @Commit@ is @a → ()@, @Emit@ is @() → a@. Not monoidal packaging — for that
-- use 'replEnds' ('par'). Free dual remains 'openRepl'.
--
-- >>> let r = Repl { replCommit = \_ -> pure (), replEmit = pure ["hello"], replClose = pure () }
-- >>> :t openRepl r
-- openRepl r
--   :: (Out (Kleisli IO) (,) [Text], In (Kleisli IO) (,) [Text])
-- >>> let (commit, emit) = endsRepl r
-- >>> :t commit
-- commit :: Commit IO [Text]
-- >>> :t emit
-- emit :: Emit IO [Text]
-- >>> :t par commit emit
-- par commit emit :: Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
endsRepl :: Repl -> (Commit IO [Text], Emit IO [Text])
endsRepl r = (runOut inR outU, runIn outR inU)
  where
    (outR, inR) = openRepl r
    (outU, inU) = openK ()

-- | Wire the dual ports with 'par': one Trace morphism (Box / dual-port view).
--
-- Conjoint / companion: 'Out' companion, 'In' conjoint, 'open'/'close' = η/ε.
-- 'openRepl' holds free ends; 'endsRepl' unit-plugs; this packages with 'par':
--
-- @
--   replEnds r = par c e   where (c, e) = endsRepl r
--   :: Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
-- @
--
-- Prefer this over advertising @(Out, In)@ as a monoidal object. Multi-channel
-- (e.g. stdout ‖ stderr) is nested 'par' of unit-plugged halves, not a bigger
-- product type. No extra structure beyond free store + 'par'.
--
-- >>> let r = Repl { replCommit = \_ -> pure (), replEmit = pure ["hi"], replClose = pure () }
-- >>> :t replEnds r
-- replEnds r :: Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
replEnds :: Repl -> Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
replEnds r = par c e
  where
    (c, e) = endsRepl r

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

