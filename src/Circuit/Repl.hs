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

    -- * Process ports (stdin / stdout / stderr)
    ProcessPorts (..),
    openProcessPorts,
    attachProcessPorts,
    portsEnds,
    replFromPortsStdout,

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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Circuit (run, par)
-- >>> import Circuit.Classes ((>>>))
-- >>> import Circuit.Ends (openK)
-- >>> import Circuit.Queue (Commit, Emit)
-- >>> import Circuit.Trace (In (..), Out (..), Trace (..), runIn, runOut)
-- >>> import Control.Arrow (Kleisli (..), runKleisli)
-- >>> import Data.IORef
-- >>> import Data.Text (Text)

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
-- >>> import Circuit.Trace (In (..), Out (..), Trace (..), runIn, runOut)
-- >>> import Control.Arrow (Kleisli (..), runKleisli)
-- >>> import Data.Text (Text)
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
-- >>> let _openR = openRepl r :: (Out (Kleisli IO) (,) [Text], In (Kleisli IO) (,) [Text])
-- >>> let (commit, emit) = endsRepl r
-- >>> let _commit = commit :: Commit IO [Text]
-- >>> let _emit = emit :: Emit IO [Text]
-- >>> let _wire = par commit emit :: Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
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
-- >>> let _wire = replEnds r :: Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
replEnds :: Repl -> Trace (,) (Kleisli IO) ([Text], ()) ((), [Text])
replEnds r = par c e
  where
    (c, e) = endsRepl r

-- ---------------------------------------------------------------------------
-- Process ports: stdin / stdout / stderr
-- ---------------------------------------------------------------------------

-- | A process token with three free dual seats: stdin commit, stdout emit,
-- and stderr emit. This is the splayed / store view: @(peIn, (peOut, peErr))@
-- as independent ends, not a monoidal object.
--
-- The monoidal / wire view is 'portsEnds': @par peIn (par peOut peErr)@.
--
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Circuit (run, par)
-- >>> import Circuit.Classes ((>>>))
-- >>> import Circuit.Ends (openK)
-- >>> import Circuit.Trace (In (..), Out (..), Trace (..), runIn, runOut)
-- >>> import Control.Arrow (Kleisli (..), runKleisli)
-- >>> import Data.Text (Text)
data ProcessPorts a b c = ProcessPorts
  { peIn    :: In  (Kleisli IO) (,) a
  -- ^ Write TO the process (stdin / commit).
  , peOut   :: Out (Kleisli IO) (,) b
  -- ^ Read FROM the process stdout.
  , peErr   :: Out (Kleisli IO) (,) c
  -- ^ Read FROM the process stderr.
  , peClose :: IO ()
  -- ^ Release handles / kill child / detach.
  }

-- | Open a process with three line ports: stdin FIFO, stdout log, stderr log.
--
-- Spawns the configured command with stdin/stdout/stderr redirected, and
-- returns the three free ends plus a close action. Stdout and stderr each
-- have their own cursor on their respective log files.
openProcessPorts :: ReplConfig -> IO (ProcessPorts [Text] [Text] [Text])
openProcessPorts cfg = do
  ensureFifo (replStdinPath cfg)
  stdoutH <- openFile (replStdoutPath cfg) AppendMode
  stderrH <- openFile (replStderrPath cfg) AppendMode
  hSetBuffering stdoutH NoBuffering
  hSetBuffering stderrH NoBuffering
  stdinH <- openFile (replStdinPath cfg) ReadMode
  let procSpec =
        (proc (replCommand cfg) (replArgs cfg))
          { cwd = Just (replWorkingDir cfg),
            std_in = UseHandle stdinH,
            std_out = UseHandle stdoutH,
            std_err = UseHandle stderrH
          }
  (_, _, _, ph) <- createProcess procSpec
  hClose stdinH
  hClose stdoutH
  hClose stderrH

  stdoutCursor <- Cur.newFile (cursorPath cfg)
  Cur.set stdoutCursor 0
  stderrCursor <- Cur.newFile (stderrCursorPath cfg)
  Cur.set stderrCursor 0
  lastOutP <- newIORef Nothing
  lastErrP <- newIORef Nothing

  let commit ts = mapM_ (\t -> withFile (replStdinPath cfg) WriteMode $ \h -> do
        TIO.hPutStrLn h t
        hFlush h) ts
      peIn_ = In $ \o -> Arr (Kleisli $ \ts -> commit ts >> runKleisli (run (runIn o peIn_)) ts)
      peOut_ = Out $ \_ -> Arr (Kleisli $ \_ -> logEmit (replStdoutPath cfg) stdoutCursor lastOutP)
      peErr_ = Out $ \_ -> Arr (Kleisli $ \_ -> logEmit (replStderrPath cfg) stderrCursor lastErrP)
      close = terminateProcess ph

  pure ProcessPorts { peIn = peIn_, peOut = peOut_, peErr = peErr_, peClose = close }

-- | Attach to an existing process's logs without spawning.
--
-- Cursors start at the current end of both logs so the next poll only sees
-- future output. Each attachment gets its own cursor files (PID-suffix) so
-- multiple observers on the same logs do not interfere.
attachProcessPorts :: ReplConfig -> IO (ProcessPorts [Text] [Text] [Text])
attachProcessPorts cfg = do
  contentOut <- readLogContent (replStdoutPath cfg)
  contentErr <- readLogContent (replStderrPath cfg)
  stdoutCursorFile <- attachCursorPath (replStdoutPath cfg)
  stderrCursorFile <- attachCursorPath (replStderrPath cfg)
  stdoutCursor <- Cur.newFile stdoutCursorFile
  Cur.seekEnd stdoutCursor (fst (splitComplete contentOut))
  stderrCursor <- Cur.newFile stderrCursorFile
  Cur.seekEnd stderrCursor (fst (splitComplete contentErr))
  lastOutP <- newIORef Nothing
  lastErrP <- newIORef Nothing

  let commit ts = mapM_ (\t -> withFile (replStdinPath cfg) WriteMode $ \h -> do
        TIO.hPutStrLn h t
        hFlush h) ts
      peIn_ = In $ \o -> Arr (Kleisli $ \ts -> commit ts >> runKleisli (run (runIn o peIn_)) ts)
      peOut_ = Out $ \_ -> Arr (Kleisli $ \_ -> logEmit (replStdoutPath cfg) stdoutCursor lastOutP)
      peErr_ = Out $ \_ -> Arr (Kleisli $ \_ -> logEmit (replStderrPath cfg) stderrCursor lastErrP)
      close = pure () -- attach does not own the process

  pure ProcessPorts { peIn = peIn_, peOut = peOut_, peErr = peErr_, peClose = close }

-- | The wire view of 'ProcessPorts': one nested 'par' morphism.
--
-- Store @(peIn, (peOut, peErr))@ becomes
-- @par commit (par out err) :: Trace (,) (Kleisli IO) (a, ((), ())) ((), (b, c))@.
--
-- Each free end is unit-plugged with its own 'openK ()' pair; the three
-- unit plugs are independent channels.
--
-- >>> let pp = ProcessPorts { peIn = undefined, peOut = undefined, peErr = undefined, peClose = pure () }
-- >>> let _wire = portsEnds pp :: Trace (,) (Kleisli IO) ([Text], ((), ())) ((), ([Text], [Text]))
--
-- Round-trip doctest (no process spawn): commit writes a shared cell; the
-- nested par reads it back through both @Out@ seats.
--
-- >>> ref <- newIORef ([] :: [Text])
-- >>> let commit = In $ \o -> Arr (Kleisli $ \ts -> writeIORef ref ts >> runKleisli (run (runIn o commit)) ts)
-- >>> let emit   = Out $ \_ -> Arr (Kleisli $ \_ -> readIORef ref)
-- >>> let pp' = ProcessPorts { peIn = commit, peOut = emit, peErr = emit, peClose = pure () }
-- >>> runKleisli (run (portsEnds pp')) (["hello"], ((), ()))
-- ((),(["hello"],["hello"]))
portsEnds :: ProcessPorts a b c -> Trace (,) (Kleisli IO) (a, ((), ())) ((), (b, c))
portsEnds pp = par commit (par out err)
  where
    commit = runOut (peIn pp) outUIn
    out    = runIn (peOut pp) inUOut
    err    = runIn (peErr pp) inUErr
    (outUIn, _) = openK ()
    (_, inUOut) = openK ()
    (_, inUErr) = openK ()

-- | The old stdout-only 'Repl' view, extracted from 'ProcessPorts'.
--
-- This keeps 'replOpen' / 'replAttach' backward compatible: they build
-- 'ProcessPorts' internally and return this wrapper.
replFromPortsStdout :: ProcessPorts [Text] [Text] c -> Repl
replFromPortsStdout pp =
  Repl
    { replCommit = \ts -> runKleisli (run commit) ts,
      replEmit = runKleisli (run out) (),
      replClose = peClose pp
    }
  where
    commit = runOut (peIn pp) outU
    out    = runIn (peOut pp) inU
    (outU, inU) = openK ()

-- ---------------------------------------------------------------------------
-- Constructor: FIFO
-- ---------------------------------------------------------------------------

replOpen :: ReplConfig -> IO Repl
replOpen cfg = replFromPortsStdout <$> openProcessPorts cfg

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
replAttach cfg = replFromPortsStdout <$> attachProcessPorts cfg

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

ensureFifo :: FilePath -> IO ()
ensureFifo path = do
  exists <- doesFileExist path
  unless exists $ callProcess "mkfifo" [path]

cursorPath :: ReplConfig -> FilePath
cursorPath cfg = replStdoutPath cfg <> ".cursor"

stderrCursorPath :: ReplConfig -> FilePath
stderrCursorPath cfg = replStderrPath cfg <> ".cursor"

attachCursorPath :: FilePath -> IO FilePath
attachCursorPath logPath = do
  pid <- getProcessID
  pure (logPath <> ".cursor-attach-" <> show pid)

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

