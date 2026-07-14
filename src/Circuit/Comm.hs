{-# LANGUAGE OverloadedStrings #-}

{- ORMOLU_DISABLE -}
-- $setup
-- >>> :set -XOverloadedStrings
{- ORMOLU_ENABLE -}
--
-- >>> frameMessage "hermes" "status check"
-- "[hermes] status check"
--
-- >>> parseMessage "[llm] found a type error in Foo.hs"
-- Just ("llm","found a type error in Foo.hs")
--
-- === Usage
--
-- Desktop agent (long-running):
--
-- @
--   ch <- channelOpen (defaultChannelConfig \"desktop-llm\")
--   channelSend ch \"scanning codebase for improvements...\"
--   -- ... work ...
--   channelSend ch \"found: simplify foldr in Bar.hs\"
--   channelSend ch \"question: should I also refactor Baz.hs?\"
-- @
--
-- Laptop cron job:
--
-- @
--   ch <- channelAttach (defaultChannelConfig \"hermes-cron\")
--   msgs <- channelRecv ch
--   for_ msgs $ \\(sender, body) ->
--     when (\"question:\" \`T.isPrefixOf\` body) $ do
--       answer <- think body
--       channelSend ch (sender <> \": \" <> answer)
-- @

-- | Multi-agent communication channel built on 'Circuit.Repl' primitives.
--
-- === Architecture
--
-- A channel is a shared FIFO + append-only log.  The \"bus\" is
-- literally @cat@ — it reads the FIFO and writes to the log.
-- Any number of agents can attach via 'channelAttach'; each
-- maintains its own read cursor so it only sees new messages.
--
-- The write-end of the FIFO is kept open for the lifetime of each
-- handle.  This prevents the bus from seeing EOF and exiting
-- between messages.  Multiple writers are serialized by the OS.
--
-- @
--   Agent A ──write──→ FIFO ──read──→ cat ──write──→ stdout log
--   Agent B ──write──→ FIFO                ↑ read ↙  (shared file)
--   Agent C ──write──→ FIFO           Agent B ──read──→ cursor B
--                                     Agent A ──read──→ cursor A
-- @
--
-- === Message format
--
-- One message per line: @[sender] body@
module Circuit.Comm
  ( -- * Configuration
    ChannelConfig (..),
    defaultChannelConfig,

    -- * Handle
    Channel,
    channelOpen,
    channelAttach,
    channelClose,

    -- * Operations
    channelSend,
    channelRecv,
    channelRecvBlocking,

    -- * Framing
    frameMessage,
    parseMessage,
  )
where

import Circuit.Repl
import Control.Concurrent (threadDelay)
import Control.Exception (onException)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO
import Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Channel configuration.
data ChannelConfig = ChannelConfig
  { -- | Shared stdin FIFO — agents write messages here.
    chStdinPath :: FilePath,
    -- | Shared stdout log — the bus appends here, agents read from here.
    chStdoutPath :: FilePath,
    -- | Bus process stderr.
    chStderrPath :: FilePath,
    -- | This agent's name (used as @[name]@ prefix on sent messages).
    chName :: Text,
    -- | Working directory for the bus process.
    chWorkingDir :: FilePath
  }
  deriving (Show, Eq)

-- | Sensible defaults in @\/tmp\/channel-*@.
--
-- @
--   ch <- channelOpen (defaultChannelConfig \"my-agent\")
-- @
defaultChannelConfig :: Text -> ChannelConfig
defaultChannelConfig name =
  ChannelConfig
    { chStdinPath = "/tmp/channel-stdin",
      chStdoutPath = "/tmp/channel-stdout.md",
      chStderrPath = "/tmp/channel-stderr.md",
      chName = name,
      chWorkingDir = "."
    }

-- | Convert to a 'ReplConfig' that uses @cat@ as the bus.
--
-- @cat@ is the simplest possible relay: it reads the FIFO and
-- writes to stdout (which 'replOpen' redirects to the log file).
-- We keep a persistent write handle open so @cat@ never sees EOF
-- and stays alive across messages.
toReplConfig :: ChannelConfig -> ReplConfig
toReplConfig cfg =
  ReplConfig
    { replCommand = "cat",
      replArgs = [],
      replStdinPath = chStdinPath cfg,
      replStdoutPath = chStdoutPath cfg,
      replStderrPath = chStderrPath cfg,
      replWorkingDir = chWorkingDir cfg
    }

-- ---------------------------------------------------------------------------
-- Handle
-- ---------------------------------------------------------------------------

-- | A connected channel handle.
--
-- Wraps a 'Repl' (for read cursor tracking) with a persistent write
-- handle to the FIFO.  Keeping the write-end open prevents the bus
-- from exiting on EOF between messages.
data Channel = Channel
  { chRepl :: Repl,
    chCfg :: ChannelConfig,
    chWriteH :: Handle
  }

-- | Open a new channel — spawns the @cat@ bus process, creates the FIFO
-- and log files, and opens a persistent write handle.
--
-- The returned handle OWNS the bus.  Call 'channelClose' to tear down.
channelOpen :: ChannelConfig -> IO Channel
channelOpen cfg = do
  repl <- replOpen (toReplConfig cfg)
  -- Open a persistent write-end so the bus never sees EOF.
  -- If openFile fails, clean up the spawned Repl process.
  writeH <-
    openFile (chStdinPath cfg) WriteMode
      `onException` replClose repl
  hSetBuffering writeH NoBuffering
  pure $ Channel repl cfg writeH

-- | Attach to an existing channel without spawning.
--
-- The bus must already be running (started by another agent via
-- 'channelOpen').  Creates a fresh read cursor and a persistent
-- write handle.
channelAttach :: ChannelConfig -> IO Channel
channelAttach cfg = do
  repl <- replAttach (toReplConfig cfg)
  writeH <- openFile (chStdinPath cfg) WriteMode
  hSetBuffering writeH NoBuffering
  pure $ Channel repl cfg writeH

-- | Close a channel handle.
--
-- Closes the persistent write handle.  If this handle owns the bus
-- (created via 'channelOpen'), sends SIGTERM to @cat@.  Attached
-- handles ('channelAttach') only close their write handle.
channelClose :: Channel -> IO ()
channelClose ch = do
  hClose (chWriteH ch)
  replClose (chRepl ch)

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Send a message to the channel.
--
-- Frames as @[name] body@ and writes to the shared FIFO.
-- The bus relays it to the log where all attached agents can read it.
--
-- Non-blocking — returns as soon as the write is flushed.
channelSend :: Channel -> Text -> IO ()
channelSend ch body = do
  TIO.hPutStrLn (chWriteH ch) (frameMessage (chName (chCfg ch)) body)
  hFlush (chWriteH ch)

-- | Receive all new messages since the last poll.
--
-- Reads the shared log and returns @(sender, body)@ pairs for each
-- new line that parses as a framed message.  Lines that don't match
-- the @[sender] body@ format are silently dropped.
--
-- Non-blocking — returns @[]@ if nothing new.
channelRecv :: Channel -> IO [(Text, Text)]
channelRecv ch = do
  ls <- replEmit (chRepl ch)
  pure $ mapMaybe parseMessage ls

-- | Block until new messages arrive, or the timeout fires.
--
-- Polls the log with exponential backoff (10ms → 500ms cap).
-- Returns @Just msgs@ if any messages arrived, @Nothing@ on timeout.
--
-- Timeout is in microseconds (1 second = 1,000,000).
channelRecvBlocking :: Channel -> Int -> IO (Maybe [(Text, Text)])
channelRecvBlocking ch timeoutUs = go 0 10000
  where
    go elapsed delay = do
      msgs <- channelRecv ch
      if not (null msgs)
        then pure (Just msgs)
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= timeoutUs
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' delay'

-- ---------------------------------------------------------------------------
-- Framing
-- ---------------------------------------------------------------------------

-- | Frame a message with sender prefix.
--
-- >>> frameMessage "hermes" "status check"
-- "[hermes] status check"
frameMessage :: Text -> Text -> Text
frameMessage sender body = "[" <> sender <> "] " <> body

-- | Parse a framed message into @(sender, body)@.
--
-- >>> parseMessage "[llm] found a type error"
-- Just ("llm","found a type error")
--
-- >>> parseMessage "unframed text"
-- Nothing
parseMessage :: Text -> Maybe (Text, Text)
parseMessage t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      case T.breakOn "] " rest of
        (sender, body)
          | T.null sender -> Nothing
          | T.null body -> Nothing -- no "] " found
          | otherwise ->
              let body' = T.drop 2 body -- drop "] "
               in if T.null body'
                    then Nothing
                    else Just (sender, body')
