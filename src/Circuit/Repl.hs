{-# LANGUAGE OverloadedStrings #-}

-- | A 'Repl' is a persistent agent process.
--
-- Agents / REPLs are the same object class: they read input, perform an
-- evaluation effect, print output however they like, and loop when they like.
-- There is no request–response contract in the type — no prompts, no timeouts,
-- no claim tokens.
--
-- The interface is the dual of 'Circuit.Queue.endsQueue': free commit and emit
-- ends sharing a process + append-only log.
--
-- @
--   open / attach / close     -- lifecycle
--   replCommit / replEmit     -- dual ends (write TO / read FROM the agent)
--   endsRepl                  -- same ends as 'Commit' / 'Emit' circuits
-- @
--
-- Timeout and turn boundaries live only in circuits that /tie/ the two ends
-- together.  Those circuits are not part of this module.
--
-- Backends (FIFO, PTY, inject, Hermes session file) are transport for the
-- same dual.  Read position uses the @cursor@ package for line logs; Hermes
-- uses a message index into the session JSON @messages@ array.
module Circuit.Repl
  ( -- * Configuration
    ReplConfig (..),
    defaultReplConfig,

    -- * Lifecycle
    Repl,
    replGetConfig,
    replOpen,
    replOpenInject,
    replOpenPty,
    replOpenHermes,
    replAttach,
    replClose,

    -- * Dual ends
    replCommit,
    replEmit,
    endsRepl,
    replWrite,
    replRead,
  )
where

import Circuit (Trace (..))
import Circuit.Queue (Commit, Emit)
import Control.Arrow (Kleisli (..))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (guard, unless, void)
import Cursor qualified as Cur
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
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
import System.Posix.Pty
  ( Pty,
    closePty,
    spawnWithPty,
    tryReadPty,
    writePty,
  )
import System.Posix.Types (FileMode)
import System.Process
import System.Timeout (timeout)
import Prelude
-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for a process-backed agent session.
data ReplConfig = ReplConfig
  { -- | Command to run (e.g. @\"cabal\"@, @\"hermes\"@, @\"cat\"@)
    replCommand :: String,
    -- | Arguments (e.g. @[\"repl\"]@, @[\"chat\", \"--cli\"]@)
    replArgs :: [String],
    -- | Path to stdin FIFO (FIFO backend; unused for pure PTY)
    replStdinPath :: FilePath,
    -- | Path to stdout log file (append-only, multi-reader)
    replStdoutPath :: FilePath,
    -- | Path to stderr log file
    replStderrPath :: FilePath,
    -- | Working directory
    replWorkingDir :: FilePath
  }
  deriving (Show, Eq)

-- | Sensible defaults (paths under @\/tmp@).
defaultReplConfig :: ReplConfig
defaultReplConfig =
  ReplConfig
    { replCommand = "cabal",
      replArgs = ["repl"],
      replStdinPath = "/tmp/repl-stdin",
      replStdoutPath = "/tmp/repl-stdout.md",
      replStderrPath = "/tmp/repl-stderr.md",
      replWorkingDir = "."
    }

-- ---------------------------------------------------------------------------
-- Repl handle
-- ---------------------------------------------------------------------------

-- | Transport backend.  Sum type (not a typeclass).
--
--   * 'BackendFifo' — child writes log via redirected fds; commit opens write-end.
--   * 'BackendPty'  — parent pumps master reads into log; commit uses 'writePty'.
--   * 'BackendInject' — tests / fakes: commit is a pure IO action (no OS process).
--   * 'BackendHermes' — attach to a Hermes session JSON file (not an agent runtime).
--     Commit RMW-appends @user@ messages; emit projects new @assistant@ contents.
--     Hermes itself (elsewhere) writes assistant replies into the same file.
data Backend
  = BackendFifo
      { beFifo :: FilePath,
        beFifoPh :: Maybe ProcessHandle
      }
  | BackendPty
      { bePty :: Pty,
        bePtyPh :: ProcessHandle,
        bePump :: ThreadId
      }
  | BackendInject
      { beInject :: Text -> IO ()
      }
  | BackendHermes
      { -- | Path to @session_*.json@ (full JSON document, not JSONL).
        beSessionPath :: FilePath,
        -- | Index into the @messages@ array — next message to consider on emit.
        beMsgIndex :: IORef Int
      }
-- | A live agent session: process (or inject) + log + cursor.
--
-- Transport is 'Backend' only.  Dual ends are 'replCommit' / 'replEmit'.
data Repl = Repl
  { replConfig :: ReplConfig,
    replBackend :: Backend,
    replCursor :: Cur.Cursor,
    -- | Last trailing partial line we already surfaced (hanging prompt).
    replLastPartial :: IORef (Maybe Text)
  }

-- | Config used to open / attach this handle.
replGetConfig :: Repl -> ReplConfig
replGetConfig = replConfig

-- | Build a 'Repl' over an existing log + backend (shared constructor).
mkRepl :: ReplConfig -> Backend -> Cur.Cursor -> IO Repl
mkRepl cfg backend cursor = do
  lastP <- newIORef Nothing
  pure $ Repl cfg backend cursor lastP

-- | Cursor file beside the stdout log.  Owner uses @.cursor@; attach uses
-- @.cursor-attach-\<pid\>@ so concurrent readers do not share position.
ownerCursorPath :: ReplConfig -> FilePath
ownerCursorPath cfg = replStdoutPath cfg <> ".cursor"

attachCursorPath :: ReplConfig -> IO FilePath
attachCursorPath cfg = do
  pid <- getProcessID
  pure (replStdoutPath cfg <> ".cursor-attach-" <> show pid)

-- | Ensure a FIFO exists, creating it if necessary.
ensureFifo :: FilePath -> IO ()
ensureFifo path = do
  exists <- doesFileExist path
  unless exists $ callProcess "mkfifo" [path]

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open an agent session (FIFO backend).
--
-- 1. Creates the stdin FIFO if missing.
-- 2. Spawns the process with stdin on the FIFO and stdout/stderr on the logs.
-- 3. Returns a 'Repl' with the cursor at the start of the log.
replOpen :: ReplConfig -> IO Repl
replOpen cfg = do
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

  cursor <- Cur.newFile (ownerCursorPath cfg)
  Cur.set cursor 0
  mkRepl cfg (BackendFifo (replStdinPath cfg) (Just ph)) cursor

-- | Attach to an already-running session (same FIFO/log paths).
--
-- Does not own process lifetime.  Cursor starts at log tail.
-- Commit still writes the FIFO.
replAttach :: ReplConfig -> IO Repl
replAttach cfg = do
  content <- readLogContent (replStdoutPath cfg)
  let (complete, _) = splitComplete content
  path <- attachCursorPath cfg
  cursor <- Cur.newFile path
  Cur.seekEnd cursor complete
  mkRepl cfg (BackendFifo (replStdinPath cfg) Nothing) cursor

-- | Open a log-only 'Repl' whose commit is a pure inject action.
-- Used by dual-mode backend mocks with no OS process.
replOpenInject :: ReplConfig -> (Text -> IO ()) -> IO Repl
replOpenInject cfg inject = do
  appendFile (replStdoutPath cfg) ""
  cursor <- Cur.newFile (ownerCursorPath cfg)
  Cur.set cursor 0
  mkRepl cfg (BackendInject inject) cursor

-- | Open a process connected via PTY. Parent pumps master → stdout log.
replOpenPty :: ReplConfig -> IO Repl
replOpenPty cfg = do
  createDirectoryIfMissing True (takeDirectory (replStdoutPath cfg))
  appendFile (replStdoutPath cfg) ""
  (pty, ph) <-
    spawnWithPty
      Nothing
      True
      (replCommand cfg)
      (replArgs cfg)
      (100, 30)
  pumpTid <- forkIO (pumpPtyToLog pty (replStdoutPath cfg))
  cursor <- Cur.newFile (ownerCursorPath cfg)
  Cur.set cursor 0
  mkRepl cfg (BackendPty pty ph pumpTid) cursor

-- | Attach to a Hermes session JSON file as a 'Repl'.
--
-- This is /not/ an agent runtime: Hermes (or another writer) must append
-- @assistant@ messages to the same file.  This backend only:
--
--   * __commit__ — RMW-append @{\"role\":\"user\",\"content\":...}@ messages
--   * __emit__ — project new non-empty @assistant@ contents since the last poll
--
-- The message index starts at the current length (tail attach): emit returns
-- only messages that appear after open (plus any written after our commits).
replOpenHermes :: FilePath -> IO Repl
replOpenHermes path = do
  exists <- doesFileExist path
  unless exists $
    throwIO . userError $
      "replOpenHermes: session file not found: " <> path
  n <- hermesMessageCount path
  idx <- newIORef n
  let cfg =
        defaultReplConfig
          { replCommand = "hermes-session",
            replArgs = [],
            replStdinPath = path,
            replStdoutPath = path,
            replStderrPath = path <> ".stderr",
            replWorkingDir = takeDirectory path
          }
  cursor <- Cur.newFile (path <> ".cursor-hermes")
  Cur.set cursor 0
  mkRepl cfg (BackendHermes path idx) cursor

-- | Pump PTY master reads into the append-only log (byte chunks).
pumpPtyToLog :: Pty -> FilePath -> IO ()
pumpPtyToLog pty logPath = go
  where
    go = do
      r <- try @IOException (tryReadPty pty)
      case r of
        Left _ -> pure ()
        Right (Left _) -> go
        Right (Right bs)
          | BS.null bs -> go
          | otherwise -> BS.appendFile logPath bs >> go

-- | Close a session (backend-specific teardown; must not hang).
replClose :: Repl -> IO ()
replClose r = case replBackend r of
  BackendFifo {beFifoPh} -> for_ beFifoPh terminateProcess
  BackendPty {bePty, bePtyPh, bePump} -> do
    void $ try @IOException (terminateProcess bePtyPh)
    void $
      timeout 500_000 $ do
        void $ try @IOException (closePty bePty)
        killThread bePump
  BackendInject {} -> pure ()
  BackendHermes {} -> pure ()
-- ---------------------------------------------------------------------------
-- Dual ends
-- ---------------------------------------------------------------------------

-- | Write lines into the agent (commit end).
--
-- Independent of emit: does not wait for output, prompts, or completion.
-- Empty list is a no-op. Each element is one line (backend appends @\\n@),
-- except 'BackendHermes' which RMW-appends one @user@ message per element
-- in a single rewrite.
-- Matches 'replEmit' object type so the dual is an @[Text] → [Text]@ agent.
replCommit :: Repl -> [Text] -> IO ()
replCommit _ [] = pure ()
replCommit r ts = case replBackend r of
  BackendHermes {} -> hermesCommit r ts
  _ -> mapM_ (replCommitLine r) ts

-- | Line-level backend write (private transport).
replCommitLine :: Repl -> Text -> IO ()
replCommitLine r t = case replBackend r of
  BackendFifo {beFifo} ->
    withFile beFifo WriteMode $ \h -> do
      TIO.hPutStrLn h t
      hFlush h
  BackendPty {bePty} ->
    writePty bePty (encodeUtf8 (t <> "\n"))
  BackendInject {beInject} ->
    beInject t
  BackendHermes {} ->
    hermesCommit r [t]

-- | Read all new lines from the agent (emit end).
--
-- Advances the cursor only over complete (newline-terminated) lines, and
-- surfaces a trailing partial line when it appears or changes (hanging prompts).
-- Independent of commit: never blocks waiting for the agent.
--
-- Hermes: projects new non-empty @assistant@ message contents (skips @tool@
-- and empty / tool-call-only assistants).
replEmit :: Repl -> IO [Text]
replEmit r = case replBackend r of
  BackendHermes {} -> hermesEmit r
  _ -> logEmit r

-- | Line-log emit (FIFO / PTY / inject).
logEmit :: Repl -> IO [Text]
logEmit r = do
  content <- readLogContent (replStdoutPath (replConfig r))
  let (complete, mPartial) = splitComplete content
  news <- Cur.pollLines (replCursor r) complete
  prev <- readIORef (replLastPartial r)
  writeIORef (replLastPartial r) mPartial
  let partialNews = case (news, prev, mPartial) of
        (_, _, Nothing) -> []
        -- After complete lines arrive, re-surface current partial (new prompt).
        (_ : _, _, Just p) -> [p]
        -- Idle: only emit partial if it is new or changed.
        ([], Just old, Just p) | old == p -> []
        ([], _, Just p) -> [p]
  pure (news <> partialNews)
-- | Free dual ends, same shape as 'Circuit.Queue.endsQueue'.
--
-- @
--   (commit, emit) = endsRepl r
--   -- commit :: Commit IO [Text]  --  [Text] → ()
--   -- emit   :: Emit   IO [Text]  --  () → [Text]
-- @
--
-- Compose freely.  A turn is an optional circuit that ties them
-- (boundary detector + timeout) — not provided here.
endsRepl :: Repl -> (Commit IO [Text], Emit IO [Text])
endsRepl r = (replWrite r, replRead r)

-- | Commit end as a 'Trace' wire (@[Text] → ()@).
replWrite :: Repl -> Trace t (Kleisli IO) [Text] ()
replWrite r = Arr $ Kleisli $ replCommit r

-- | Emit end as a 'Trace' wire (@() → [Text]@).
replRead :: Repl -> Trace t (Kleisli IO) () [Text]
replRead r = Arr $ Kleisli $ \() -> replEmit r

-- ---------------------------------------------------------------------------
-- Log helpers
-- ---------------------------------------------------------------------------

-- | Read raw log content (empty if missing).
--
-- Uses a strict byte read so concurrent appenders do not keep a lazy
-- read handle open on the append-only log.
readLogContent :: FilePath -> IO Text
readLogContent fp = do
  exists <- doesFileExist fp
  if not exists
    then pure ""
    else decodeUtf8 <$> BS.readFile fp

-- | Split into complete (newline-terminated) lines and optional trailing partial.
splitComplete :: Text -> ([Text], Maybe Text)
splitComplete content
  | T.null content = ([], Nothing)
  | T.isSuffixOf "\n" content = (T.lines content, Nothing)
  | otherwise =
      let parts = T.splitOn "\n" content
       in case parts of
            [] -> ([], Nothing)
            _ -> (init parts, Just (last parts))

-- ---------------------------------------------------------------------------
-- Hermes session-file backend
-- ---------------------------------------------------------------------------

-- | Exclusive flock around session RMW (Hermes may write concurrently).
withSessionLock :: FilePath -> IO a -> IO a
withSessionLock path action = bracket acquire release (const action)
  where
    lockPath = path <> ".lock"
    mode :: Maybe FileMode
    mode = Just 0o644
    acquire = do
      fd <-
        openFd
          lockPath
          ReadWrite
          defaultFileFlags {creat = mode}
      waitToSetLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
      pure fd
    release fd = do
      waitToSetLock fd (PIO.Unlock, AbsoluteSeek, 0, 0)
      closeFd fd

-- | Read session JSON object.
readSessionObject :: FilePath -> IO (KM.KeyMap Value)
readSessionObject path = do
  bs <- BS.readFile path
  case eitherDecodeStrict' bs of
    Left err ->
      throwIO . userError $
        "hermes session decode: " <> err <> " (" <> path <> ")"
    Right (Object o) -> pure o
    Right _ ->
      throwIO . userError $
        "hermes session: expected top-level object (" <> path <> ")"

-- | Atomic rewrite: write @path.tmp@ then rename.
writeSessionObject :: FilePath -> KM.KeyMap Value -> IO ()
writeSessionObject path o = do
  let tmp = path <> ".tmp"
  LBS.writeFile tmp (encode (Object o))
  renameFile tmp path

-- | @messages@ array length (0 if missing).
hermesMessageCount :: FilePath -> IO Int
hermesMessageCount path = withSessionLock path $ do
  o <- readSessionObject path
  pure $ case KM.lookup "messages" o of
    Just (Array arr) -> V.length arr
    _ -> 0

-- | RMW-append @user@ messages (one content string per 'Text').
hermesCommit :: Repl -> [Text] -> IO ()
hermesCommit r ts = case replBackend r of
  BackendHermes {beSessionPath} ->
    withSessionLock beSessionPath $ do
      o <- readSessionObject beSessionPath
      let oldMsgs = case KM.lookup "messages" o of
            Just (Array arr) -> arr
            _ -> V.empty
          added =
            V.fromList
              [ object ["role" .= ("user" :: Text), "content" .= t]
              | t <- ts
              ]
          newMsgs = oldMsgs <> added
          o' =
            KM.insert "messages" (Array newMsgs) $
              KM.insert "message_count" (Number (fromIntegral (V.length newMsgs))) o
      writeSessionObject beSessionPath o'
  _ -> pure ()

-- | Project new non-empty @assistant@ contents; advance message index to tip.
hermesEmit :: Repl -> IO [Text]
hermesEmit r = case replBackend r of
  BackendHermes {beSessionPath, beMsgIndex} ->
    withSessionLock beSessionPath $ do
      o <- readSessionObject beSessionPath
      let msgs = case KM.lookup "messages" o of
            Just (Array arr) -> arr
            _ -> V.empty
      i <- readIORef beMsgIndex
      let new = V.drop i msgs
          asst = mapMaybe assistantContent (V.toList new)
      writeIORef beMsgIndex (V.length msgs)
      pure asst
  _ -> pure []

-- | Extract non-empty assistant text content; skip tool / empty / structured.
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
