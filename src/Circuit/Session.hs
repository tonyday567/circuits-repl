{-# LANGUAGE OverloadedStrings #-}

-- | Session protocol for agent-to-agent conversation.
--
-- Built on 'Circuit.Comm', adds:
--
--   * Directed questions with blocking answers (@ask@ / @answer@)
--   * Message tagging (@?@ question, @!@ answer, plain broadcast)
--   * Non-blocking receive with typed 'Msg' parsing
--
-- === Architecture
--
-- Only the opening session runs a dispatcher thread.  Attached
-- sessions share the same message buffer.  This avoids cursor
-- races when multiple sessions read from the same 'Channel'.
--
-- @
--   Channel → dispatcher (sole reader) → shared buffer (MVar [Msg])
--                                       → recv (all sessions)
--                                       → pending MVars (ask unblocks)
-- @
--
-- === Example
--
-- Desktop agent:
--
-- @
--   sess <- sessionOpen (defaultSessionConfig \"desktop-llm\")
--   tell sess \"scanning codebase...\"
--   reply <- ask sess \"should I refactor Baz.hs?\"
--   -- blocks until Hermes answers
-- @
--
-- Hermes cron:
--
-- @
--   sess <- sessionAttach (defaultSessionConfig \"hermes\") opener
--   msgs <- recv sess
--   for_ msgs $ \\case
--     Question sender qid body ->
--       answer sess qid \"yes, go ahead\"
--     _ -> pure ()
-- @
module Circuit.Session
  ( -- * Configuration
    SessionConfig (..),
    defaultSessionConfig,

    -- * Handle
    Session,
    sessionOpen,
    sessionAttach,
    sessionClose,

    -- * Messages
    Msg (..),
    parseMsg,
    recv,

    -- * Operations
    tell,
    ask,
    answer,
    rawSend,
  )
where

import Circuit.Comm
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Session configuration — extends 'ChannelConfig' with session identity.
data SessionConfig = SessionConfig
  { sessChannel :: ChannelConfig,
    sessName :: Text
  }
  deriving (Show, Eq)

-- | Sensible defaults using 'defaultChannelConfig'.
defaultSessionConfig :: Text -> SessionConfig
defaultSessionConfig name =
  SessionConfig
    { sessChannel = defaultChannelConfig name,
      sessName = name
    }

-- ---------------------------------------------------------------------------
-- Session handle
-- ---------------------------------------------------------------------------

-- | A session wraps a 'Channel' with protocol state.
--
-- Multiple sessions on the same channel share a single message buffer
-- and dispatcher.  Only the opening session owns the dispatcher.
data Session = Session
  { sessChan :: Channel,
    sessCfg :: SessionConfig,
    -- | Message buffer shared across sessions on this channel
    sessBuffer :: MVar [Msg],
    -- | Pending questions: msgId → response MVar (shared across sessions)
    sessPending :: IORef (Map.Map Text (MVar Text)),
    -- | Monotonically increasing message ID counter (per-session)
    sessCounter :: IORef Int,
    -- | True if this session owns the dispatcher
    sessOwnsBus :: Bool
  }

-- | Open a new session — creates the channel, shared buffer, and dispatcher.
--
-- The returned session OWNS the channel and dispatcher.
-- Other agents attach via 'sessionAttach'.
sessionOpen :: SessionConfig -> IO Session
sessionOpen cfg = do
  ch <- channelOpen (sessChannel cfg)
  buf <- newMVar []
  pending <- newIORef Map.empty
  counter <- newIORef 0
  let sess = Session ch cfg buf pending counter True
  _ <- forkIO (dispatcher sess)
  pure sess

-- | Attach to an existing session, sharing its channel, buffer,
-- and pending-questions map.
--
-- The opener's dispatcher is the sole channel reader.  Attached
-- sessions get their own message counter but share the buffer
-- and pending map so 'ask' works correctly from any session.
sessionAttach :: SessionConfig -> Session -> IO Session
sessionAttach cfg opener = do
  ch <- channelAttach (sessChannel cfg)
  counter <- newIORef 0
  pure $ Session ch cfg (sessBuffer opener) (sessPending opener) counter False

-- | Close a session handle.
--
-- If this session owns the channel, kills the dispatcher and closes
-- the channel.  Attached sessions just release their handle.
sessionClose :: Session -> IO ()
sessionClose s = do
  when (sessOwnsBus s) $
    channelClose (sessChan s)

-- ---------------------------------------------------------------------------
-- Background dispatcher
-- ---------------------------------------------------------------------------

-- | Continuously reads the channel and routes messages.
--
-- Sole reader of the underlying Channel — avoids cursor races.
-- Runs forever (terminated by 'sessionClose' on the owning session).
dispatcher :: Session -> IO ()
dispatcher s = forever $ do
  raw <- channelRecv (sessChan s)
  forM_ raw $ \(sender, body) ->
    case parseMsg sender body of
      Just msg -> do
        -- Append to shared buffer for recv (all sessions)
        modifyMVar_ (sessBuffer s) $ \msgs ->
          pure (msgs ++ [msg])
        -- Route answers to waiting askers (per-session)
        case msg of
          Answer _sender qid ansBody -> do
            p <- readIORef (sessPending s)
            case Map.lookup qid p of
              Just mv -> do
                -- tryPutMVar: if MVar already full (double-answer),
                -- don't block — just drop the duplicate silently.
                _ <- tryPutMVar mv ansBody
                atomicModifyIORef' (sessPending s) $ \m ->
                  (Map.delete qid m, ())
              Nothing -> pure ()
          _ -> pure ()
      Nothing -> pure ()
  threadDelay 100_000 -- 100ms poll interval

-- | Simple forever helper.
forever :: IO () -> IO ()
forever a = a >> forever a

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

-- | A parsed message with protocol semantics.
data Msg
  = -- | @sender body@ — plain broadcast
    Broadcast Text Text
  | -- | @sender msgId body@ — question expecting an answer
    Question Text Text Text
  | -- | @sender msgId body@ — answer to a question
    Answer Text Text Text
  deriving (Show, Eq)

-- | Receive all new messages since the last poll (non-blocking).
--
-- Drains the shared buffer atomically.  Returns @[]@ if nothing new.
recv :: Session -> IO [Msg]
recv s = modifyMVar (sessBuffer s) $ \msgs -> pure ([], reverse msgs)

-- | Parse a framed @[sender] body@ into a 'Msg'.
parseMsg :: Text -> Text -> Maybe Msg
parseMsg sender body =
  case T.stripPrefix "? " body of
    Just rest ->
      case T.breakOn " " rest of
        (qid, qbody)
          | T.null qid -> Nothing
          | otherwise ->
              let qbody' = T.strip qbody
               in if T.null qbody'
                    then Nothing
                    else Just (Question sender qid qbody')
    Nothing ->
      case T.stripPrefix "! " body of
        Just rest ->
          case T.breakOn " " rest of
            (qid, abody)
              | T.null qid -> Nothing
              | otherwise ->
                  let abody' = T.strip abody
                   in if T.null abody'
                        then Nothing
                        else Just (Answer sender qid abody')
        Nothing ->
          Just (Broadcast sender body)

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Send a broadcast message (no reply expected).
tell :: Session -> Text -> IO ()
tell s = channelSend (sessChan s)

-- | Send a raw text through the underlying channel (bypasses framing).
--
-- Useful for testing and for agents that handle their own protocol.
rawSend :: Session -> Text -> IO ()
rawSend s = channelSend (sessChan s)

-- | Generate a fresh message ID unique to this session.
freshId :: Session -> IO Text
freshId s = do
  n <- atomicModifyIORef' (sessCounter s) (\c -> (c + 1, c))
  pure $ sessName (sessCfg s) <> "." <> T.pack (show n)

-- | Ask a question and block until an answer arrives.
--
-- Sends @? msgId body@ and waits for a matching @! msgId answer@.
-- The answer comes from any agent — there's no sender filtering.
--
-- Blocks indefinitely.  The dispatcher thread routes incoming
-- answers to the correct 'MVar'.
ask :: Session -> Text -> IO Text
ask s body = do
  qid <- freshId s
  mv <- newEmptyMVar

  -- Register the pending question BEFORE sending, to avoid a race
  -- where the answer arrives before we register.
  atomicModifyIORef' (sessPending s) $ \p ->
    (Map.insert qid mv p, ())

  channelSend (sessChan s) ("? " <> qid <> " " <> body)

  -- Block until the dispatcher fills our MVar
  takeMVar mv

-- | Answer a question.
--
-- Sends @! msgId body@.  The dispatcher in the asking agent's session
-- will route it to the blocked 'ask' call.
answer :: Session -> Text -> Text -> IO ()
answer s qid body =
  channelSend (sessChan s) ("! " <> qid <> " " <> body)
