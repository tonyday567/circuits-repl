{-# LANGUAGE OverloadedStrings #-}

-- | A persistent cabal REPL and a free-agent channel owner.
--
-- One owner reads channel posts in order and sends one command at a time to one
-- real @cabal repl@ process. Standard output and standard error share one pipe,
-- so a result cannot lose or reorder the error stream. A unique prompt marks
-- the end of a command; output is accumulated through the complete prompt,
-- rather than through a newline approximation.
module Circuit.Repl.Process
  ( -- * persistent REPL
    ReplProcess,
    replPid,
    replStarts,
    ReplOutcome (..),
    startRepl,
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

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Exception (IOException, bracket, catch, try)
import Control.Monad (void, when)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrictText, withObject, (.:))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hGetChar, hSetBuffering, hSetEncoding, utf8)
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

-- | A process handle, its merged output stream, and the prompt boundary state.
data ReplProcess = ReplProcess
  { processInput :: Handle,
    processOutput :: Handle,
    processHandle :: ProcessHandle,
    processEvents :: Chan ReplEvent,
    processBuffer :: IORef Text,
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

-- | A command either reached the next prompt or ended with the process.
data ReplOutcome
  = ReplCompleted Text
  | ReplExited Text ExitCode
  deriving (Eq, Show)

data ReplEvent
  = ReplOutput Text
  | ReplEOF

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
  buffer <- newIORef ""
  let repl = ReplProcess input output handle events buffer pid 1
  _ <- forkIO (outputReader output events)
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

-- | Send one command and capture all merged output through the next private
-- prompt. On @:quit@ or a crash, the process exit code is explicit.
runReplCommand :: ReplProcess -> Text -> IO ReplOutcome
runReplCommand repl command = do
  existing <- getProcessExitCode (processHandle repl)
  case existing of
    Just code -> pure (ReplExited "" code)
    Nothing -> do
      writeResult <-
        try $
          TIO.hPutStrLn (processInput repl) command
            >> hFlush (processInput repl)
      case writeResult of
        Left err -> do
          code <- waitForProcess (processHandle repl)
          pure (ReplExited (T.pack (show (err :: IOException))) code)
        Right () -> do
          pending <- readIORef (processBuffer repl)
          writeIORef (processBuffer repl) ""
          readOutcome repl pending

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

-- | Own one REPL and one channel subscription until the REPL exits. Requests
-- are handled strictly in log order. Owner posts are ignored, preventing a
-- result from becoming another command.
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

requestLoop :: ChannelConfig -> (Handle, ProcessHandle) -> ReplProcess -> IO ChannelExit
requestLoop config watch repl = do
  let (watchOutput, _) = watch
  line <- TIO.hGetLine watchOutput
  case eitherDecodeStrictText line of
    Left err -> do
      putStrLn ("repl-channel: ignored non-JSON watch output: " ++ err)
      requestLoop config watch repl
    Right post
      | busPostFrom post == channelName config ->
          requestLoop config watch repl
      | otherwise -> do
          let command = T.stripEnd (busPostBody post)
          if T.null command || T.any (== '\n') command
            then do
              postOutcome config repl post "protocol_error" "expected one non-empty GHCi command per post"
              requestLoop config watch repl
            else do
              outcome <- runReplCommand repl command
              case outcome of
                ReplCompleted output -> do
                  postOutcome config repl post "completed" output
                  requestLoop config watch repl
                ReplExited output code -> do
                  postOutcomeWithCode config repl post "exit" code output
                  pure (ChannelExit code (replPid repl) (channelRoot config))

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

outputReader :: Handle -> Chan ReplEvent -> IO ()
outputReader handle events = do
  _ <- try loop :: IO (Either IOException ())
  writeChan events ReplEOF
  where
    loop = do
      c <- hGetChar handle
      writeChan events (ReplOutput (T.singleton c))
      loop

readOutcome :: ReplProcess -> Text -> IO ReplOutcome
readOutcome repl accumulated =
  case splitPrompt accumulated of
    Just (output, leftover) -> do
      writeIORef (processBuffer repl) leftover
      pure (ReplCompleted output)
    Nothing -> do
      event <- readChan (processEvents repl)
      case event of
        ReplOutput chunk -> readOutcome repl (accumulated <> chunk)
        ReplEOF -> do
          code <- waitForProcess (processHandle repl)
          pure (ReplExited accumulated code)

splitPrompt :: Text -> Maybe (Text, Text)
splitPrompt input =
  let (before, rest) = T.breakOn promptMarker input
   in if T.null rest
        then Nothing
        else Just (before, T.drop (T.length promptMarker) rest)

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
