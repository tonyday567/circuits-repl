{-# LANGUAGE OverloadedStrings #-}

-- | A persistent cabal REPL and a free-agent channel owner.
--
-- Input and output are independent: callers may send several commands before
-- receiving output, and receive any number of output events before the prompt
-- boundary. Standard output and standard error share one pipe, so their order
-- is preserved. A unique prompt marks the end of a command.
--
-- The process may also write without being asked, for example from a thread it
-- started earlier. 'receiveReplEvent' reports that text as ordinary output; the
-- channel owner posts it as an uncorrelated observation rather than failing,
-- because the prompt boundary is the only correlation available.
module Circuit.Repl.Process
  ( -- * persistent REPL
    ReplProcess,
    replPid,
    replStarts,
    ReplEvent (..),
    ReplOutcome (..),
    startRepl,
    sendReplCommand,
    receiveReplEvent,
    runReplCommand,
    stopRepl,

    -- * bus posts
    BusPost (..),
    postBus,
    readBus,

    -- * channel owner
    ChannelConfig (..),
    ChannelExit (..),
    runChannel,
  )
where

import Control.Concurrent (Chan, forkIO, killThread, newChan, readChan, writeChan, writeList2Chan)
import Control.Exception (IOException, bracket, catch, finally, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrictText, withObject, (.:))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBuffering, hSetEncoding, utf8)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getPid,
    getProcessExitCode,
    proc,
    readProcess,
    shell,
    terminateProcess,
    waitForProcess,
  )
import Prelude

-- | A process handle and its independent input and output channels.
data ReplProcess = ReplProcess
  { processInput :: Handle,
    processOutput :: Handle,
    processHandle :: ProcessHandle,
    processEvents :: Chan ReplEvent,
    processPid :: Maybe Int,
    processStarts :: Int
  }

-- | The real operating-system process id. @exec@ in the launcher keeps the id
-- stable as the shell becomes cabal and cabal enters the REPL.
replPid :: ReplProcess -> Maybe Int
replPid = processPid

-- | Number of REPL starts owned by this value. This is always one.
replStarts :: ReplProcess -> Int
replStarts = processStarts

-- | One output observation: available text, a prompt boundary, or process exit.
data ReplEvent
  = ReplOutput Text
  | ReplDone
  | ReplEventExited ExitCode
  deriving (Eq, Show)

-- | A complete command outcome, used by the compatibility runner.
data ReplOutcome
  = ReplCompleted Text
  | ReplExited Text ExitCode
  deriving (Eq, Show)

promptMarker :: Text
promptMarker = "__CIRCUITS_REPL_PROCESS_DONE_7F3A91__"

-- | Start one real @cabal repl@ in a named project. The optional target is
-- passed to cabal unchanged. The returned startup outcome includes cabal and
-- GHCi output through installation of the private prompt.
startRepl :: FilePath -> Maybe String -> IO (ReplProcess, ReplOutcome)
startRepl project target = do
  let args = "repl" : maybe [] pure target
      command = "exec cabal " ++ unwords (shellQuote <$> args) ++ " 2>&1"
  (Just input, Just output, _, handle) <-
    createProcess
      (shell command)
        { cwd = Just project,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = Inherit
        }
  mapM_ (\h -> hSetEncoding h utf8 >> hSetBuffering h NoBuffering) [input, output]
  pid <- fmap fromIntegral <$> getPid handle
  events <- newChan
  let repl = ReplProcess input output handle events pid 1
  _ <- forkIO (outputReader output handle events)
  promptOutcome <-
    runReplCommand repl $
      ":set prompt \"\\n"
        <> promptMarker
        <> "\\n\""
  case promptOutcome of
    ReplExited {} -> pure (repl, promptOutcome)
    ReplCompleted promptOutput -> do
      continuationOutcome <- runReplCommand repl ":set prompt-cont \"\""
      pure
        ( repl,
          case continuationOutcome of
            ReplCompleted continuationOutput ->
              ReplCompleted (promptOutput <> continuationOutput)
            ReplExited continuationOutput code ->
              ReplExited (promptOutput <> continuationOutput) code
        )

-- | Send one command without waiting for any output.
sendReplCommand :: ReplProcess -> Text -> IO (Maybe ReplOutcome)
sendReplCommand repl command = do
  existing <- getProcessExitCode (processHandle repl)
  case existing of
    Just code -> pure (Just (ReplExited "" code))
    Nothing -> do
      writeResult <-
        try $
          TIO.hPutStrLn (processInput repl) command
            >> hFlush (processInput repl)
      case writeResult of
        Left err -> do
          code <- waitForProcess (processHandle repl)
          pure (Just (ReplExited (T.pack (show (err :: IOException))) code))
        Right () -> pure Nothing

-- | Receive the next output observation, independently of sending commands.
receiveReplEvent :: ReplProcess -> IO ReplEvent
receiveReplEvent = readChan . processEvents

-- | Compatibility: send one command and accumulate its output through 'ReplDone'.
-- The caller must have drained every earlier independent command first; prompt
-- boundaries, not this wrapper, supply command correlation.
runReplCommand :: ReplProcess -> Text -> IO ReplOutcome
runReplCommand repl command = do
  sent <- sendReplCommand repl command
  case sent of
    Just outcome -> pure outcome
    Nothing -> readOutcome repl ""

-- | Stop the REPL if it is still running. Normal @:quit@ has already reaped it.
stopRepl :: ReplProcess -> IO ()
stopRepl repl = do
  existing <- getProcessExitCode (processHandle repl)
  case existing of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess (processHandle repl)
      void (waitForProcess (processHandle repl))
  closeQuietly (processInput repl)
  closeQuietly (processOutput repl)

-- | A channel post. The thread array is retained verbatim for correlation.
data BusPost = BusPost
  { busPostId :: Int,
    busPostFrom :: Text,
    busPostChannel :: Maybe Text,
    busPostThread :: [Int],
    busPostBody :: Text
  }
  deriving (Eq, Show)

instance FromJSON BusPost where
  parseJSON = withObject "BusPost" $ \o ->
    BusPost
      <$> o .: "id"
      <*> o .: "from"
      <*> o .: "channel"
      <*> o .: "thread"
      <*> o .: "body"

-- | Post to a channel. The body travels on stdin, so multiline REPL output is
-- not subject to shell quoting or argument-size limits.
postBus :: FilePath -> Text -> Text -> Maybe Int -> Text -> IO BusPost
postBus root channel from thread body = do
  output <-
    readProcess
      "free-agent"
      ( [ "bus",
          "post",
          "--root",
          root,
          "--from",
          T.unpack from,
          "--channel",
          T.unpack channel
        ]
          ++ maybe [] (\i -> ["--thread", show i]) thread
      )
      (T.unpack body)
  decodePost (T.pack output)

-- | Read every post visible to the named subscriptions since id zero.
readBus :: FilePath -> [Text] -> IO [BusPost]
readBus root names = do
  output <-
    readProcess
      "free-agent"
      (["bus", "read", "--root", root] ++ map T.unpack names ++ ["--since", "0"])
      ""
  traverse decodePost (filter (not . T.null . T.strip) (T.lines (T.pack output)))

-- | Configuration for one persistent channel owner.
data ChannelConfig = ChannelConfig
  { channelRoot :: FilePath,
    channelProject :: FilePath,
    channelName :: Text,
    channelAddress :: Text,
    channelTarget :: Maybe String,
    channelImports :: [Text]
  }

-- | The owner stopped because the REPL stopped.
data ChannelExit = ChannelExit
  { channelExitCode :: ExitCode,
    channelExitPid :: Maybe Int,
    channelExitRoot :: FilePath
  }
  deriving (Eq, Show)

-- | Own one REPL and one channel subscription until the REPL exits. Valid
-- commands are committed strictly in log order, with their prompt-delimited
-- outputs correlated by the same order. Malformed posts are rejected on
-- arrival. Owner posts are ignored, preventing a result from becoming another
-- command. Output that arrives while no command is pending is posted as an
-- unsolicited observation, so a process that writes on its own neither stops
-- the owner nor has its text counted against another requester.
runChannel :: ChannelConfig -> IO ChannelExit
runChannel config =
  bracket
    (startBusWatch (channelRoot config) (channelAddress config))
    stopBusWatch
    $ \watch ->
      bracket
        (startRepl (channelProject config) (channelTarget config))
        (stopRepl . fst)
        $ \(repl, startup) -> case startup of
          ReplExited output code -> do
            _ <-
              postBus
                (channelRoot config)
                (channelAddress config)
                (channelName config)
                Nothing
                (startupBody "startup_exit" repl output code)
            pure (ChannelExit code (replPid repl) (channelRoot config))
          ReplCompleted startupOutput -> do
            importOutcomes <- mapM (runReplCommand repl . (":module + " <>)) (channelImports config)
            case firstExit importOutcomes of
              Just (output, code) -> do
                _ <-
                  postBus
                    (channelRoot config)
                    (channelAddress config)
                    (channelName config)
                    Nothing
                    (startupBody "import_exit" repl output code)
                pure (ChannelExit code (replPid repl) (channelRoot config))
              Nothing -> do
                TIO.putStrLn startupOutput
                mapM_ printOutcome importOutcomes
                _ <-
                  postBus
                    (channelRoot config)
                    (channelAddress config)
                    (channelName config)
                    Nothing
                    ( "REPL_READY pid="
                        <> pidText repl
                        <> " starts=1 project="
                        <> T.pack (channelProject config)
                        <> " channel="
                        <> channelAddress config
                        <> "\n"
                        <> T.concat (map outcomeText importOutcomes)
                    )
                requestLoop config watch repl

data PendingRequest = PendingRequest
  { pendingPost :: BusPost,
    pendingOutput :: Text
  }

data ChannelAction
  = ChannelWatchLine Text
  | ChannelWatchError IOException
  | ChannelProcessEvent ReplEvent

-- | Merge channel input and process output into one blocking runner. Commands
-- can be committed while earlier output is pending; each 'ReplDone' retires the
-- oldest pending request. Output accumulation is a channel-runner policy, not
-- part of the independent process interface.
requestLoop :: ChannelConfig -> (Handle, ProcessHandle) -> ReplProcess -> IO ChannelExit
requestLoop config watch repl = do
  let (watchOutput, _) = watch
  actions <- newChan
  watchThread <- forkIO (watchActions watchOutput actions)
  eventThread <- forkIO (eventActions repl actions)
  finally
    (loop actions [] "")
    (mapM_ killThread [watchThread, eventThread])
  where
    loop actions pending unsolicited = do
      action <- readChan actions
      case action of
        ChannelWatchError err -> throwIO err
        ChannelWatchLine line ->
          case eitherDecodeStrictText line of
            Left err -> do
              putStrLn ("repl-channel: ignored non-JSON watch output: " ++ err)
              loop actions pending unsolicited
            Right post
              | busPostFrom post == channelName config ->
                  loop actions pending unsolicited
              | otherwise -> do
                  let command = T.stripEnd (busPostBody post)
                  if T.null command || T.any (== '\n') command
                    then do
                      postOutcome config repl post "protocol_error" "expected one non-empty GHCi command per post"
                      loop actions pending unsolicited
                    else do
                      sent <- sendReplCommand repl command
                      case sent of
                        Nothing -> loop actions (pending <> [PendingRequest post ""]) unsolicited
                        Just (ReplCompleted output) -> do
                          postOutcome config repl post "completed" output
                          loop actions pending unsolicited
                        Just (ReplExited output code) -> do
                          let pending' = pending <> [PendingRequest post output]
                          flushUnsolicited config repl unsolicited
                          postExitOutcomes config repl code pending'
                          pure (ChannelExit code (replPid repl) (channelRoot config))
        ChannelProcessEvent (ReplOutput output) ->
          case pending of
            [] -> do
              let (complete, remaining) = splitLines (unsolicited <> output)
              postUnsolicited config repl complete
              loop actions pending remaining
            current : rest ->
              loop actions (current {pendingOutput = pendingOutput current <> output} : rest) unsolicited
        ChannelProcessEvent ReplDone ->
          case pending of
            [] -> do
              flushUnsolicited config repl unsolicited
              unsolicitedOutcome config repl "unsolicited_boundary" ""
              loop actions pending ""
            current : rest -> do
              postOutcome config repl (pendingPost current) "completed" (pendingOutput current)
              loop actions rest unsolicited
        ChannelProcessEvent (ReplEventExited code) -> do
          flushUnsolicited config repl unsolicited
          postExitOutcomes config repl code pending
          pure (ChannelExit code (replPid repl) (channelRoot config))

-- | Split unprompted text into its complete lines and the partial remainder. A
-- line split across reads is therefore reported once, not once per read.
splitLines :: Text -> (Text, Text)
splitLines text =
  let (complete, rest) = T.breakOnEnd "\n" text
   in if T.null complete then ("", rest) else (complete, rest)

-- | Post unprompted output lines. Whitespace alone is not an observation.
postUnsolicited :: ChannelConfig -> ReplProcess -> Text -> IO ()
postUnsolicited config repl output =
  unless (T.null (T.strip output)) (unsolicitedOutcome config repl "unsolicited_output" output)

-- | Post a held partial line before the prompt returns or the runner stops.
flushUnsolicited :: ChannelConfig -> ReplProcess -> Text -> IO ()
flushUnsolicited = postUnsolicited

watchActions :: Handle -> Chan ChannelAction -> IO ()
watchActions output actions = do
  next <- try (TIO.hGetLine output) :: IO (Either IOException Text)
  case next of
    Left err -> writeChan actions (ChannelWatchError err)
    Right line -> do
      writeChan actions (ChannelWatchLine line)
      watchActions output actions

eventActions :: ReplProcess -> Chan ChannelAction -> IO ()
eventActions repl actions = do
  event <- receiveReplEvent repl
  writeChan actions (ChannelProcessEvent event)
  eventActions repl actions

postExitOutcomes :: ChannelConfig -> ReplProcess -> ExitCode -> [PendingRequest] -> IO ()
postExitOutcomes config repl code =
  mapM_ (\pending -> postOutcomeWithCode config repl (pendingPost pending) "exit" code (pendingOutput pending))

postOutcome :: ChannelConfig -> ReplProcess -> BusPost -> Text -> Text -> IO ()
postOutcome config repl request status output = do
  _ <-
    postBus
      (channelRoot config)
      (channelAddress config)
      (channelName config)
      (Just (busPostId request))
      (resultHeader repl request status Nothing <> "\n" <> output)
  pure ()

postOutcomeWithCode :: ChannelConfig -> ReplProcess -> BusPost -> Text -> ExitCode -> Text -> IO ()
postOutcomeWithCode config repl request status code output = do
  _ <-
    postBus
      (channelRoot config)
      (channelAddress config)
      (channelName config)
      (Just (busPostId request))
      (resultHeader repl request status (Just code) <> "\n" <> output)
  pure ()

-- | Post process text that no request asked for. There is no request to
-- correlate, so the post carries no request id and no thread.
unsolicitedOutcome :: ChannelConfig -> ReplProcess -> Text -> Text -> IO ()
unsolicitedOutcome config repl status output = do
  _ <-
    postBus
      (channelRoot config)
      (channelAddress config)
      (channelName config)
      Nothing
      ( "REPL_UNSOLICITED status="
          <> status
          <> " pid="
          <> pidText repl
          <> " starts=1\n"
          <> output
      )
  pure ()

resultHeader :: ReplProcess -> BusPost -> Text -> Maybe ExitCode -> Text
resultHeader repl request status code =
  "REPL_RESULT request="
    <> T.pack (show (busPostId request))
    <> " requester="
    <> busPostFrom request
    <> " thread="
    <> T.pack (show (fromMaybe (busPostId request) (safeHead (busPostThread request))))
    <> " status="
    <> status
    <> " pid="
    <> pidText repl
    <> " starts=1"
    <> maybe "" (\c -> " exit_code=" <> exitCodeText c) code

startupBody :: Text -> ReplProcess -> Text -> ExitCode -> Text
startupBody status repl output code =
  "REPL_RESULT status="
    <> status
    <> " pid="
    <> pidText repl
    <> " starts=1 exit_code="
    <> exitCodeText code
    <> "\n"
    <> output

firstExit :: [ReplOutcome] -> Maybe (Text, ExitCode)
firstExit [] = Nothing
firstExit (ReplCompleted _ : rest) = firstExit rest
firstExit (ReplExited output code : _) = Just (output, code)

outcomeText :: ReplOutcome -> Text
outcomeText (ReplCompleted output) = output
outcomeText (ReplExited output code) = output <> T.pack (show code)

printOutcome :: ReplOutcome -> IO ()
printOutcome (ReplCompleted output) = TIO.putStrLn output
printOutcome (ReplExited output code) =
  TIO.putStrLn output >> putStrLn ("REPL exited " ++ show code)

startBusWatch :: FilePath -> Text -> IO (Handle, ProcessHandle)
startBusWatch root channel = do
  (Nothing, Just output, _, handle) <-
    createProcess
      (proc "free-agent" ["bus", "watch", "--root", root, T.unpack channel])
        { std_out = CreatePipe,
          std_err = Inherit
        }
  hSetEncoding output utf8
  pure (output, handle)

stopBusWatch :: (Handle, ProcessHandle) -> IO ()
stopBusWatch (output, handle) = do
  existing <- getProcessExitCode handle
  when (isNothing existing) (terminateProcess handle)
  closeQuietly output

outputReader :: Handle -> ProcessHandle -> Chan ReplEvent -> IO ()
outputReader handle process events = do
  remaining <- loop initialMarker
  emit (markerPending remaining)
  code <- waitForProcess process
  writeChan events (ReplEventExited code)
  where
    loop marker = do
      next <- try (TIO.hGetChunk handle) :: IO (Either IOException Text)
      case next of
        Left _ -> pure marker
        Right chunk
          | T.null chunk -> pure marker
          | otherwise -> do
              let (observations, marker') = stepChunk marker chunk
              writeList2Chan events observations
              loop marker'
    emit text = unless (T.null text) (writeChan events (ReplOutput text))

-- | Classify one read from the pipe into at most one output observation per
-- contiguous run of text, so event count follows reads rather than characters.
stepChunk :: MarkerWait -> Text -> ([ReplEvent], MarkerWait)
stepChunk marker chunk =
  let (observations, marker') = T.foldl' step ([], marker) chunk
   in (coalesce (reverse observations), marker')
  where
    step (observations, marker') c =
      let (new, marker'') = stepMarker marker' c
       in (reverse new <> observations, marker'')

-- | Merge adjacent output observations. Nothing can arrive between them except
-- a boundary, so the merged text is the same stream text.
coalesce :: [ReplEvent] -> [ReplEvent]
coalesce = foldr merge []
  where
    merge (ReplOutput a) (ReplOutput b : rest) = ReplOutput (a <> b) : rest
    merge event rest = event : rest

-- | Prompt detection state: received text not yet classified, and whether the
-- last classified event was a prompt boundary.
data MarkerWait = MarkerWait
  { markerPending :: Text,
    markerBoundary :: Bool
  }

initialMarker :: MarkerWait
initialMarker = MarkerWait "" False

-- | Classify one received character. The newline that ends the prompt line
-- belongs to the boundary, not to any command's output, so it is consumed
-- before anything else is classified.
stepMarker :: MarkerWait -> Char -> ([ReplEvent], MarkerWait)
stepMarker marker c =
  classify (absorb (markerPending marker <> T.singleton c) (markerBoundary marker))
  where
    absorb pending True = case T.uncons pending of
      Just ('\n', rest) -> MarkerWait rest False
      _ -> MarkerWait pending False
    absorb pending False = MarkerWait pending False

classify :: MarkerWait -> ([ReplEvent], MarkerWait)
classify marker =
  case T.breakOn promptMarker pending of
    (before, rest)
      | not (T.null rest) ->
          ( [ReplOutput before | not (T.null before)] <> [ReplDone],
            MarkerWait (T.drop (T.length promptMarker) rest) True
          )
      | otherwise ->
          let keep = markerSuffixLength pending
              (output, remaining) = T.splitAt (T.length pending - keep) pending
              observations = [ReplOutput output | not (T.null output)]
           in (observations, MarkerWait remaining False)
  where
    pending = markerPending marker

-- | Length of the longest suffix of the received text that is a proper prefix
-- of the prompt marker. Only that much has to be withheld: any other text
-- cannot become part of the marker, so holding it back would delay output that
-- is already complete.
markerSuffixLength :: Text -> Int
markerSuffixLength pending = go (min (T.length pending) (T.length promptMarker - 1))
  where
    go k
      | k <= 0 = 0
      | T.takeEnd k pending == T.take k promptMarker = k
      | otherwise = go (k - 1)

readOutcome :: ReplProcess -> Text -> IO ReplOutcome
readOutcome repl accumulated = do
  event <- receiveReplEvent repl
  case event of
    ReplOutput chunk -> readOutcome repl (accumulated <> chunk)
    ReplDone -> pure (ReplCompleted accumulated)
    ReplEventExited code -> pure (ReplExited accumulated code)

decodePost :: Text -> IO BusPost
decodePost input = case eitherDecodeStrictText (T.strip input) of
  Left err -> ioError (userError ("cannot decode free-agent post: " ++ err ++ ": " ++ T.unpack input))
  Right post -> pure post

pidText :: ReplProcess -> Text
pidText = maybe "unknown" (T.pack . show) . replPid

exitCodeText :: ExitCode -> Text
exitCodeText ExitSuccess = "0"
exitCodeText (ExitFailure n) = T.pack (show n)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

shellQuote :: String -> String
shellQuote value = "'" ++ concatMap escape value ++ "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]

closeQuietly :: Handle -> IO ()
closeQuietly handle = hClose handle `catch` \(_ :: IOException) -> pure ()
