{-# LANGUAGE OverloadedStrings #-}

-- | Real independent-I/O, process, and channel oracles. The direct witness
-- pipelines commands and output events; the channel witness starts another
-- cabal REPL in circuits-net, addresses it as two identities, and retains the
-- complete scratch bus conversation.
module Axioma.Process
  ( processTopic,
    processTopicAt,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Repl.Process
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (when)
import Data.List (find, isPrefixOf, nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Prelude

-- | Run against circuits-net in a fresh directory below the claimed scratch
-- parent. The parent is configurable through the process environment only by
-- changing the explicit call to 'processTopicAt'.
processTopic :: Verbosity -> IO [Bool]
processTopic verbosity = do
  home <- getEnv "HOME"
  processTopicAt
    (home </> ".hermes/cache/scratch/free2-repl-channel")
    "../circuits-net"
    verbosity

-- | Run the complete witness with an explicit scratch parent and project.
processTopicAt :: FilePath -> FilePath -> Verbosity -> IO [Bool]
processTopicAt scratchParent project0 verbosity = do
  when (verbosity == Axioms) $ putStrLn "Process oracles"
  project <- canonicalizePath project0
  directChecks <- independentIOOracles project verbosity
  root <- freshRoot scratchParent 0
  putStrLn ("WITNESS_PROJECT " ++ project)
  putStrLn ("WITNESS_LOG " ++ root </> "log.jsonl")
  done <- newEmptyMVar
  owner <-
    forkIO $
      try
        ( runChannel
            ChannelConfig
              { channelRoot = root,
                channelProject = project,
                channelName = "cabal-repl",
                channelAddress = "repl",
                channelTarget = Just "circuits-net",
                channelImports = ["NetCoalgebra", "Control.Concurrent"]
              }
        )
        >>= putMVar done
  let cleanup = killThread owner
  flip finally cleanup $ do
    ready <- waitFor root (isReady . busPostBody)
    pid <- maybe (ioError (userError "missing REPL pid")) pure (headerInt "pid=" (busPostBody ready))
    processEvidence <- readProcess "ps" ["-p", show pid, "-o", "pid=,ppid=,comm=,args="] ""
    putStrLn "PROCESS_IDENTITY"
    putStr processEvidence

    slowRequest <- postBus root "repl" "alice" Nothing "threadDelay 3000000 >> print (\"slow-finished\" :: String)"
    badRequest <- postBus root "repl" "bob" Nothing "let bad = 1\nlet bad = 2"
    badResult <- waitFor root (isResultFor badRequest)
    postsAtBadResult <- readBus root ["repl"]
    qLet <- postBus root "repl" "alice" Nothing "let x = 41"
    qEval <- postBus root "repl" "bob" Nothing "x + 1"
    postsBeforeSlowResult <- readBus root ["repl"]
    slowResult <- waitFor root (isResultFor slowRequest)
    rLet <- waitFor root (isResultFor qLet)
    rEval <- waitFor root (isResultFor qEval)
    (qType, rType) <- submit root "alice" ":t NetCoalgebra.trainIdentity"
    (qFail, rFail) <- submit root "bob" "doesNotExist"
    (qRecover, rRecover) <- submit root "alice" "x + 2"
    (qRepeat, rRepeat) <- submit root "bob" "x + 1"
    qAsync <- postBus root "repl" "alice" Nothing "forkIO (threadDelay 400000 >> putStrLn \"UNSOLICITED-MARKER\")"
    rAsync <- waitFor root (isResultFor qAsync)
    rUnprompted <- waitFor root (isUnprompted "UNSOLICITED-MARKER")
    qAfter <- postBus root "repl" "bob" Nothing "length [1, 2, 3]"
    rAfter <- waitFor root (isResultFor qAfter)
    (qQuit, rQuit) <- submit root "alice" ":quit"

    channelExit <- takeMVar done >>= either (throwIO :: SomeException -> IO ChannelExit) pure
    aliceView <- readBus root ["alice", "repl"]
    bobView <- readBus root ["bob", "repl"]
    let requests = [qLet, qEval, qType, qFail, qRecover, qRepeat, qAfter, qQuit]
        results = [rLet, rEval, rType, rFail, rRecover, rRepeat, rAfter, rQuit]
        resultIds = map busPostId results
        pids = map (headerInt "pid=" . busPostBody) results
        bodyHas text post = text `T.isInfixOf` busPostBody post
        correlated request result =
          busPostId request `elem` busPostThread result
            && bodyHas ("request=" <> showText (busPostId request)) result
            && bodyHas ("requester=" <> busPostFrom request) result
            && bodyHas ("thread=" <> showText (busPostId request)) result
        completed result = bodyHas "status=completed" result && bodyHas "starts=1" result
    sequence
      ( map pure directChecks
          ++ [ checkV verbosity "channel input is accepted while an earlier command is still running" $
                 correlated badRequest badResult
                   && bodyHas "status=protocol_error" badResult
                   && not (any (isResultFor slowRequest) postsAtBadResult)
                   && correlated slowRequest slowResult
                   && completed slowResult
                   && bodyHas "slow-finished" slowResult,
               checkV verbosity "valid channel commands queue before earlier output completes" $
                 all (`elem` postsBeforeSlowResult) [qLet, qEval]
                   && not (any (isResultFor slowRequest) postsBeforeSlowResult)
                   && not (any (isResultFor qLet) postsBeforeSlowResult)
                   && not (any (isResultFor qEval) postsBeforeSlowResult),
               checkV verbosity "the startup module import completes without a GHCi error" $
                 not (bodyHas "error:" ready),
               checkV verbosity "two identities share one ordered channel" $
                 nub (map busPostFrom requests) == ["alice", "bob"]
                   && length (nub resultIds) == length resultIds,
               checkV verbosity "every result carries its requester and thread" $
                 all (uncurry correlated) (zip requests results),
               checkV verbosity "let and x+1 retain state across identities in one REPL" $
                 completed rLet
                   && completed rEval
                   && bodyHas "42" rEval
                   && all (== Just pid) (take 2 pids),
               checkV verbosity "the real NetCoalgebra.trainIdentity type is complete" $
                 completed rType
                   && bodyHas "trainIdentity" rType
                   && bodyHas "Double" rType
                   && bodyHas "NetParams" rType,
               checkV verbosity "a failing command keeps full error output and the next command succeeds" $
                 completed rFail
                   && bodyHas "doesNotExist" rFail
                   && bodyHas "not in scope" rFail
                   && completed rRecover
                   && bodyHas "43" rRecover,
               checkV verbosity "repeated commands neither restart nor cross-attribute" $
                 completed rRepeat
                   && bodyHas "42" rRepeat
                   && all (== Just pid) pids
                   && all (uncurry correlated) (zip requests results),
               checkV verbosity "both identities can read every shared result" $
                 all (\r -> r `elem` aliceView && r `elem` bobView) results,
               checkV verbosity "output with no pending command is posted uncorrelated" $
                 null (busPostThread rUnprompted)
                   && not (bodyHas "request=" rUnprompted)
                   && bodyHas "status=unsolicited_output" rUnprompted
                   && headerInt "pid=" (busPostBody rUnprompted) == Just pid
                   && correlated qAsync rAsync,
               checkV verbosity "unprompted output is not attributed to the next request" $
                 completed rAfter
                   && bodyHas "3" rAfter
                   && not (bodyHas "UNSOLICITED-MARKER" rAfter)
                   && not (bodyHas "UNSOLICITED-MARKER" rQuit),
               checkV verbosity "process identity is visible in ps and constant" $
                 show pid `isPrefixOf` dropWhile (== ' ') processEvidence
                   && all (== Just pid) pids,
               checkV verbosity ":quit reports the real process exit explicitly" $
                 bodyHas "status=exit" rQuit
                   && bodyHas "exit_code=0" rQuit
                   && channelExitCode channelExit == ExitSuccess
                   && channelExitPid channelExit == Just pid
                   && channelExitRoot channelExit == root
             ]
      )
  where
    submit root from body = do
      request <- postBus root "repl" from Nothing body
      result <- waitFor root (isResultFor request)
      pure (request, result)

independentIOOracles :: FilePath -> Verbosity -> IO [Bool]
independentIOOracles project verbosity =
  bracket
    (startRepl project (Just "circuits-net"))
    (stopRepl . fst)
    $ \(repl, startup) -> case startup of
      ReplExited output code ->
        ioError (userError ("independent I/O REPL startup failed: " ++ show code ++ " " ++ T.unpack output))
      ReplCompleted _ -> do
        sendCommand repl "let pipelined = 40"
        sendCommand repl "pipelined + 2"
        _ <- readCommandEvents repl
        pipelinedEvents <- readCommandEvents repl
        sendCommand repl "mapM_ print [1 .. 100 :: Int]"
        manyEvents <- readCommandEvents repl
        sendCommand repl "print (\"final-before-done\" :: String)"
        finalEvents <- readCommandEvents repl
        compatibility <- runReplCommand repl "mapM_ print [200 .. 202 :: Int]"
        sendCommand repl "import Control.Concurrent"
        _ <- readCommandEvents repl
        sendCommand repl "forkIO (threadDelay 300000 >> putStrLn \"ASYNC-EARLY\")"
        _ <- readCommandEvents repl
        unprompted <- readUnprompted repl "ASYNC-EARLY"
        sendCommand repl "6 * 7"
        afterUnprompted <- readCommandEvents repl
        let pipelinedOutput = eventsOutput pipelinedEvents
            manyOutput = eventsOutput manyEvents
            manyChunks = length [() | ReplOutput _ <- manyEvents]
        sequence
          [ checkV verbosity "two commands can be sent before either output is received" $
              "42" `T.isInfixOf` pipelinedOutput,
            checkV verbosity "one command can produce multiple output events before ReplDone" $
              manyChunks > 1
                && "1\n2\n" `T.isInfixOf` manyOutput
                && "99\n100\n" `T.isInfixOf` manyOutput
                && lastEventIsDone manyEvents,
            checkV verbosity "the final output events are retained before ReplDone" $
              case reverse finalEvents of
                ReplDone : ReplOutput {} : _ ->
                  "final-before-done" `T.isInfixOf` eventsOutput finalEvents
                _ -> False,
            checkV verbosity "compatibility draining retains ordered multi-event output" $
              case compatibility of
                ReplCompleted output -> "200\n201\n202\n" `T.isInfixOf` output
                ReplExited {} -> False,
            checkV verbosity "output the caller never requested arrives before the next command" $
              "ASYNC-EARLY" `T.isInfixOf` eventsOutput unprompted
                && not (any isBoundary unprompted),
            checkV verbosity "unrequested output is not carried into the next command's events" $
              not ("ASYNC-EARLY" `T.isInfixOf` eventsOutput afterUnprompted)
                && "42" `T.isInfixOf` eventsOutput afterUnprompted
                && lastEventIsDone afterUnprompted
          ]

-- | Collect output until the text appears, sending nothing. A budget of 100ms
-- per step bounds the wait for a process that never writes.
readUnprompted :: ReplProcess -> Text -> IO [ReplEvent]
readUnprompted repl wanted = go [] (60 :: Int)
  where
    go accumulated 0 = pure (reverse accumulated)
    go accumulated n = do
      next <- timeout 100000 (receiveReplEvent repl)
      case next of
        Nothing
          | wanted `T.isInfixOf` eventsOutput accumulated -> pure (reverse accumulated)
          | otherwise -> go accumulated (n - 1)
        Just event -> case event of
          ReplOutput {} -> go (event : accumulated) n
          ReplDone -> pure (reverse (event : accumulated))
          ReplEventExited code ->
            ioError (userError ("REPL exited while receiving unprompted output: " ++ show code))

isBoundary :: ReplEvent -> Bool
isBoundary ReplDone = True
isBoundary _ = False

sendCommand :: ReplProcess -> Text -> IO ()
sendCommand repl command = do
  result <- sendReplCommand repl command
  case result of
    Nothing -> pure ()
    Just outcome -> ioError (userError ("REPL send failed: " ++ show outcome))

readCommandEvents :: ReplProcess -> IO [ReplEvent]
readCommandEvents repl = go []
  where
    go accumulated = do
      event <- receiveReplEvent repl
      case event of
        ReplOutput {} -> go (event : accumulated)
        ReplDone -> pure (reverse (ReplDone : accumulated))
        ReplEventExited code -> ioError (userError ("REPL exited while receiving: " ++ show code))

eventsOutput :: [ReplEvent] -> Text
eventsOutput events = T.concat [output | ReplOutput output <- events]

lastEventIsDone :: [ReplEvent] -> Bool
lastEventIsDone events =
  case reverse events of
    ReplDone : _ -> True
    _ -> False

-- | Create an empty bus log in a fresh scratch directory.
freshRoot :: FilePath -> Int -> IO FilePath
freshRoot parent n = do
  createDirectoryIfMissing True parent
  let root = parent </> ("axioma-bus-" ++ show n)
  exists <- doesPathExist root
  if exists
    then freshRoot parent (n + 1)
    else do
      createDirectoryIfMissing False root
      writeFile (root </> "log.jsonl") ""
      pure root

waitFor :: FilePath -> (BusPost -> Bool) -> IO BusPost
waitFor root predicate = go (1200 :: Int)
  where
    go 0 = ioError (userError ("timed out reading " ++ root </> "log.jsonl"))
    go remaining = do
      posts <- readBus root ["repl"]
      case find predicate posts of
        Just post -> pure post
        Nothing -> threadDelay 100000 >> go (remaining - 1)

isReady :: Text -> Bool
isReady = T.isPrefixOf "REPL_READY "

isResultFor :: BusPost -> BusPost -> Bool
isResultFor request post =
  busPostFrom post == "cabal-repl"
    && busPostId request `elem` busPostThread post
    && ("request=" <> showText (busPostId request)) `T.isInfixOf` busPostBody post

-- | A post the owner published without any request to answer.
isUnprompted :: Text -> BusPost -> Bool
isUnprompted wanted post =
  busPostFrom post == "cabal-repl"
    && "REPL_UNSOLICITED" `T.isInfixOf` busPostBody post
    && wanted `T.isInfixOf` busPostBody post

headerInt :: Text -> Text -> Maybe Int
headerInt key body =
  listToMaybe
    [ value
    | word <- T.words body,
      key `T.isPrefixOf` word,
      Just value <- [readMaybe (T.unpack (T.drop (T.length key) word))]
    ]

showText :: (Show a) => a -> Text
showText = T.pack . show
