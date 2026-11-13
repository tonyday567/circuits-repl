{-# LANGUAGE OverloadedStrings #-}

-- | Real process and channel oracles. These witnesses start one cabal REPL in
-- circuits-net, address it as two identities, and retain the complete scratch
-- bus conversation.
module Axioma.Process
  ( processTopic,
    processTopicAt,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Repl.Process
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, throwIO, try)
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
                channelImports = ["NetCoalgebra"]
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

    (qLet, rLet) <- submit root "alice" "let x = 41"
    (qEval, rEval) <- submit root "bob" "x + 1"
    (qType, rType) <- submit root "alice" ":t NetCoalgebra.trainIdentity"
    (qFail, rFail) <- submit root "bob" "doesNotExist"
    (qRecover, rRecover) <- submit root "alice" "x + 2"
    (qRepeat, rRepeat) <- submit root "bob" "x + 1"
    (qQuit, rQuit) <- submit root "alice" ":quit"

    channelExit <- takeMVar done >>= either (throwIO :: SomeException -> IO ChannelExit) pure
    aliceView <- readBus root ["alice", "repl"]
    bobView <- readBus root ["bob", "repl"]
    let requests = [qLet, qEval, qType, qFail, qRecover, qRepeat, qQuit]
        results = [rLet, rEval, rType, rFail, rRecover, rRepeat, rQuit]
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
      [ checkV verbosity "the startup module import completes without a GHCi error" $
          not (bodyHas "error:" ready),
        checkV verbosity "two identities share one sequential channel" $
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
  where
    submit root from body = do
      request <- postBus root "repl" from Nothing body
      result <- waitFor root (isResultFor request)
      pure (request, result)

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
