{-# LANGUAGE OverloadedStrings #-}

module Main where

import Circuit (Trace (..), run)
import Circuit.Comm
import Circuit.Int (IntMorph (..), causal, comp)
import Circuit.Poly (Mono, Morphism, applyLens, lens)
import Circuit.Repl
import Circuit.Repl.Agent (AgentVerb (..), agentRoster, openAgentRosterRepl, verbDelta)
import Circuit.Repl.PingPong (openPingPongRepl, pingPongLens)
import Circuit.Repl.Turn (TurnConfig (..), defaultTurnConfig, turnUntil)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import MockBackend (MockMode (..), openMockRepl)
import System.Directory (doesFileExist, removeFile)
import System.IO (hPutStrLn, stderr)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "circuits-repl"
      [ replTests,
        backendTests,
        musterReplTests,
        channelTests,
        agentIntTests,
        pingPongIntTests
      ]

-- ---------------------------------------------------------------------------
-- Local tie helper (tests only — not part of Circuit.Repl)
--
-- Timeout lives here, on the circuit that joins free emit to a boundary.
-- ---------------------------------------------------------------------------

-- | Poll 'replEmit' until a line satisfies @isBoundary@ or timeout (µs).
emitUntil :: (Text -> Bool) -> Int -> Repl -> IO (Maybe [Text])
emitUntil isBoundary timeoutUs r = go 0 [] 10000
  where
    go elapsed acc delay = do
      news <- replEmit r
      let acc' = acc <> news
      if any isBoundary news
        then pure (Just acc')
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= timeoutUs
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' acc' delay'

-- ---------------------------------------------------------------------------
-- Repl dual-ends tests (mock)
-- ---------------------------------------------------------------------------

replTests :: TestTree
replTests =
  testGroup
    "Repl dual ends (mock)"
    [ testCase "commit then emitUntil sees response" $ do
        let cfg =
              (baseCfg ["--prompt=mock> ", "--delay=20", "--no-extra-noise"])
                { replStdinPath = "/tmp/circuits-io-mock-in-1",
                  replStdoutPath = "/tmp/circuits-io-mock-out-1.md",
                  replStderrPath = "/tmp/circuits-io-mock-err-1.md"
                }

        cleanLogs cfg

        repl <- replOpen cfg
        threadDelay 500_000

        _ <- emitUntil (T.isSuffixOf "mock> ") 5_000_000 repl -- drain welcome
        replCommit repl ["hello"]
        mResp <- emitUntil (T.isSuffixOf "mock> ") 10_000_000 repl

        -- After consuming via emitUntil, free emit should see nothing new
        extra <- replEmit repl
        assertBool "emit after drain should be empty until next output" (null extra)

        replClose repl
        threadDelay 100_000

        case mResp of
          Nothing -> assertFailure "Timed out waiting for boundary from mock"
          Just lines -> do
            let combined = T.unlines lines
            assertBool "should contain our input echo" ("received: hello" `T.isInfixOf` combined)
            assertBool "should contain a response line" ("echo: hello" `T.isInfixOf` combined),
      testCase "multiple commits with free emit" $ do
        let cfg =
              (baseCfg ["--prompt=mock> ", "--delay=15"])
                { replStdinPath = "/tmp/circuits-io-mock-in-2",
                  replStdoutPath = "/tmp/circuits-io-mock-out-2.md",
                  replStderrPath = "/tmp/circuits-io-mock-err-2.md"
                }

        cleanLogs cfg

        repl <- replOpen cfg
        threadDelay 500_000

        _ <- emitUntil (T.isSuffixOf "mock> ") 5_000_000 repl

        replCommit repl ["add 3"]
        m1 <- emitUntil (T.isSuffixOf "mock> ") 10_000_000 repl
        case m1 of
          Nothing -> assertFailure "timeout on first command"
          Just ls -> assertBool "first result has 3" ("result: 3" `T.isInfixOf` T.unlines ls)

        replCommit repl ["get"]
        m2 <- emitUntil (T.isSuffixOf "mock> ") 10_000_000 repl
        case m2 of
          Nothing -> assertFailure "timeout on second command"
          Just ls -> do
            let combined = T.unlines ls
            assertBool
              "second should see updated counter"
              ("counter: 1" `T.isInfixOf` combined || "counter: 2" `T.isInfixOf` combined)
        replClose repl
        threadDelay 100_000,
      testCase "hanging prompt (no trailing newline) still surfaces via emit" $ do
        let cfg =
              (baseCfg ["--prompt=mock-hang> ", "--delay=10", "--hanging-prompt", "--no-extra-noise"])
                { replStdinPath = "/tmp/circuits-io-mock-in-3",
                  replStdoutPath = "/tmp/circuits-io-mock-out-3.md",
                  replStderrPath = "/tmp/circuits-io-mock-err-3.md"
                }

        cleanLogs cfg

        repl <- replOpen cfg
        threadDelay 500_000

        _ <- emitUntil (T.isInfixOf "mock-hang>") 5_000_000 repl
        replCommit repl ["hello"]
        mResp <- emitUntil (T.isInfixOf "mock-hang>") 10_000_000 repl

        replClose repl
        threadDelay 100_000

        case mResp of
          Nothing -> assertFailure "timeout on hanging prompt test"
          Just lines -> do
            let combined = T.unlines lines
            assertBool "response captured even with hanging prompt" ("echo: hello" `T.isInfixOf` combined),
      testCase "attach starts at log tail; commit still shared" $ do
        let cfg =
              (baseCfg ["--prompt=mock> ", "--delay=10", "--no-extra-noise"])
                { replStdinPath = "/tmp/circuits-io-mock-in-attach",
                  replStdoutPath = "/tmp/circuits-io-mock-out-attach.md",
                  replStderrPath = "/tmp/circuits-io-mock-err-attach.md"
                }
        cleanLogs cfg
        owner <- replOpen cfg
        threadDelay 400_000
        _ <- emitUntil (T.isSuffixOf "mock> ") 5_000_000 owner

        attacher <- replAttach cfg
        -- attacher at tail: should not re-see welcome
        stale <- replEmit attacher
        assertBool "attach cursor at tail sees nothing yet" (null stale)

        replCommit attacher ["hello"]
        mResp <- emitUntil (T.isSuffixOf "mock> ") 10_000_000 owner
        case mResp of
          Nothing -> assertFailure "owner did not see attach commit"
          Just ls -> assertBool "echo" ("echo: hello" `T.isInfixOf` T.unlines ls)

        replClose owner
        threadDelay 100_000
    ]
  where
    baseCfg args =
      ReplConfig
        { replCommand = "./dist-newstyle/build/aarch64-osx/ghc-9.14.1/circuits-repl-0.1.0.0/x/mock-repl/build/mock-repl/mock-repl",
          replArgs = args,
          replStdinPath = "/tmp/circuits-io-mock-in",
          replStdoutPath = "/tmp/circuits-io-mock-out.md",
          replStderrPath = "/tmp/circuits-io-mock-err.md",
          replWorkingDir = "."
        }

    cleanLogs cfg =
      mapM_
        (\p -> whenM (doesFileExist p) (removeFile p))
        [ replStdinPath cfg,
          replStdoutPath cfg,
          replStderrPath cfg,
          replStdoutPath cfg <> ".cursor"
        ]

-- ---------------------------------------------------------------------------
-- Backend abstraction (FakeFifo vs FakePty, same free dual)
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- BackendCustom ↔ causal/IN (agent roster)
-- ---------------------------------------------------------------------------

agentIntTests :: TestTree
agentIntTests =
  testGroup
    "BackendCustom agentRoster ↔ causal/IN"
    [ testCase "commit/emit tracks pure Int morph" $ do
        r <- openAgentRosterRepl
        let pureStep n v = fst (runIntMorph (causal agentRoster) (n, verbDelta v))
        -- join
        pureStep 0 Join @?= 1
        replCommit r ["join"]
        e1 <- replEmit r
        e1 @?= ["1"]
        -- ack
        pureStep 1 Ack @?= 1
        replCommit r ["ack"]
        e2 <- replEmit r
        e2 @?= ["1"]
        -- quit
        pureStep 1 Quit @?= 0
        replCommit r ["quit"]
        e3 <- replEmit r
        e3 @?= ["0"]
        -- endsRepl dual still well-typed and live
        let (write, read_) = endsRepl r
        _ <- pure (write, read_)
        replClose r
    , testCase "unknown commit is a no-op" $ do
        r <- openAgentRosterRepl
        replCommit r ["nope"]
        e <- replEmit r
        e @?= ["0"]
        replClose r
    , testCase "agentRoster lens semantics" $ do
        let (out, put) = applyLens agentRoster 0
        out @?= (0 :: Int)
        put 1 @?= (1 :: Int)
        put (-1) @?= (0 :: Int)
    , testCase "causal agentRoster join/ack/quit" $ do
        runIntMorph (causal agentRoster) (0 :: Int, verbDelta Join) @?= (1 :: Int, 0 :: Int)
        runIntMorph (causal agentRoster) (2 :: Int, verbDelta Ack) @?= (2 :: Int, 2 :: Int)
        runIntMorph (causal agentRoster) (1 :: Int, verbDelta Quit) @?= (0 :: Int, 1 :: Int)
    , testCase "Trace comp equals lens Compose (trivial knot)" $ do
        let cz m = IntMorph (Arr (\(a, db) -> let (b, put) = applyLens m a in (put db, b)))
            composed = comp (cz agentRoster) (cz agentRoster)
        run (runIntMorph composed) (0 :: Int, verbDelta Quit) @?= (0 :: Int, 0 :: Int)
    , testCase "multi-verb path join→ack→quit restores zero" $ do
        let step n v = fst (runIntMorph (causal agentRoster) (n, verbDelta v))
        step (step (step 0 Join) Ack) Quit @?= (0 :: Int)
    ]

-- ---------------------------------------------------------------------------
-- BackendCustom ↔ causal/IN (concrete ping/pong turn)
-- ---------------------------------------------------------------------------

pingPongIntTests :: TestTree
pingPongIntTests =
  testGroup
    "BackendCustom ping/pong ↔ causal/IN"
    [ testCase "causal lens predicts immediate emit" $ do
        runIntMorph (causal pingPongLens) (["ping"], []) @?= ([], ["pong"])
        runIntMorph (causal pingPongLens) (["foo"], []) @?= ([], []),
      testCase "BackendCustom Repl agrees with causal lens" $ do
        r <- openPingPongRepl
        replCommit r ["ping"]
        e1 <- replEmit r
        e1 @?= ["pong"]
        replCommit r ["foo"]
        e2 <- replEmit r
        e2 @?= []
        replClose r,
      testCase "turnUntil ties the dual ends" $ do
        r <- openPingPongRepl
        m <- runKleisli (run (turnUntil defaultTurnConfig (T.isInfixOf "pong") r)) ["ping"]
        m @?= Just ["pong"]
        replClose r,
      testCase "endsRepl exposes the box dual (Commit/Emit wires)" $ do
        r <- openPingPongRepl
        let (write, read_) = endsRepl r
        _ <- pure (write, read_)
        replClose r,
      testCase "IntMorph composition via trace equals direct semantics" $ do
        let ackLens = lens (const ["ack"] :: [Text] -> [Text]) (const (const []))
            cz ::
              forall x xd y yd.
              Morphism (Mono x xd) (Mono y yd) ->
              IntMorph (,) (Trace (,) (->)) x xd y yd
            cz m = IntMorph (Arr (\(a, db) -> let (b, put) = applyLens m a in (put db, b)))
            f = cz pingPongLens
            g = cz ackLens
            composed = g `comp` f
        run (runIntMorph composed) (["ping"], []) @?= ([], ["ack"])
    ]

backendTests :: TestTree
backendTests =
  testGroup
    "Backend dual-mode mocks"
    [ testCase "FakeFifo: commit/emit" $ dualMode FakeFifo "fifo",
      testCase "FakePty: commit/emit" $ dualMode FakePty "pty"
    ]
  where
    dualMode mode tag = do
      r <- openMockRepl mode tag
      _ <- emitUntil (T.isSuffixOf "mock> ") 2_000_000 r
      replCommit r ["hello"]
      m <- emitUntil (T.isSuffixOf "mock> ") 2_000_000 r
      case m of
        Nothing -> assertFailure (tag <> ": no boundary")
        Just ls -> do
          let combined = T.unlines ls
          assertBool (tag <> ": has echo") ("echo: hello" `T.isInfixOf` combined)
      extra <- replEmit r
      assertBool (tag <> ": emit empty after drain") (null extra)
      replClose r

-- ---------------------------------------------------------------------------
-- MusterRepl — Comm channel as free dual
-- ---------------------------------------------------------------------------

musterReplTests :: TestTree
musterReplTests =
  testGroup
    "MusterRepl (Comm dual)"
    [ testCase "commit/emit with self-echo filtered" $ do
        let tag = "muster-repl-1"
            cfgA =
              (defaultChannelConfig "alice")
                { chStdinPath = "/tmp/muster-repl-" <> tag <> "-in",
                  chStdoutPath = "/tmp/muster-repl-" <> tag <> "-out.md",
                  chStderrPath = "/tmp/muster-repl-" <> tag <> "-err.md"
                }
            cfgB =
              cfgA {chName = "bob"}
        mapM_
          (\p -> whenM (doesFileExist p) (removeFile p))
          [ chStdinPath cfgA,
            chStdoutPath cfgA,
            chStderrPath cfgA,
            chStdoutPath cfgA <> ".cursor",
            chStdoutPath cfgA <> ".cursor-custom"
          ]

        alice <- openMusterRepl cfgA
        threadDelay 200_000
        bob <- attachMusterRepl cfgB
        threadDelay 100_000

        -- drain any bus noise
        _ <- replEmit alice
        _ <- replEmit bob

        replCommit alice ["hello from alice"]
        threadDelay 150_000

        fromBob <- replEmit bob
        assertEqual "bob sees alice body" ["hello from alice"] fromBob

        fromAlice <- replEmit alice
        assertBool "alice does not self-echo" (null fromAlice)

        replCommit bob ["reply from bob"]
        threadDelay 150_000

        toAlice <- replEmit alice
        assertEqual "alice sees bob" ["reply from bob"] toAlice

        toBob <- replEmit bob
        assertBool "bob does not self-echo" (null toBob)

        replClose bob
        replClose alice
        threadDelay 100_000
    ]

-- ---------------------------------------------------------------------------
-- Channel tests (multi-agent comms using cat bus)
-- ---------------------------------------------------------------------------

channelTests :: TestTree
channelTests =
  testGroup
    "Channel (multi-agent comms)"
    [ testCase "framing roundtrip" $ do
        let name = "test-agent"
        let body = "hello world"
        assertEqual
          "roundtrip"
          (Just (name, body))
          (parseMessage (frameMessage name body)),
      testCase "parseMessage rejects unframed text" $ do
        assertBool "unframed" (isNothing (parseMessage "hello world")),
      testCase "parseMessage rejects empty sender" $ do
        assertBool "empty sender" (isNothing (parseMessage "[] body")),
      testCase "parseMessage rejects empty body after bracket" $ do
        assertBool "empty body" (isNothing (parseMessage "[agent] ")),
      testCase "parseMessage handles bracket in body" $ do
        assertEqual
          "bracket in body"
          (Just ("agent", "[nested] text"))
          (parseMessage "[agent] [nested] text"),
      testCase "single-agent send and recv with cat bus" $ do
        let cfg = mkChCfg "agent-a" "ch-test-1"
        cleanChLogs cfg

        ch <- channelOpen cfg
        threadDelay 200_000

        channelSend ch "hello from agent-a"
        threadDelay 100_000

        msgs <- channelRecv ch
        channelClose ch
        threadDelay 100_000

        assertBool "should have at least one message" (not (null msgs))
        let (sender, body) = head msgs
        assertEqual "sender" "agent-a" sender
        assertEqual "body" "hello from agent-a" body,
      testCase "multi-agent: attach sees messages from opener" $ do
        let cfgA = mkChCfg "agent-a" "ch-test-2"
            cfgB = mkChCfg "agent-b" "ch-test-2"
        cleanChLogs cfgA

        chA <- channelOpen cfgA
        threadDelay 200_000

        chB <- channelAttach cfgB

        channelSend chA "message from A"

        mMsgs <- channelRecvBlocking chB 5_000_000
        channelClose chA
        threadDelay 100_000

        case mMsgs of
          Nothing -> assertFailure "agent B timed out waiting for A's message"
          Just msgsB -> do
            assertBool "agent B should see A's message" (not (null msgsB))
            let (sender, body) = head msgsB
            assertEqual "sender seen by B" "agent-a" sender
            assertEqual "body seen by B" "message from A" body,
      testCase "multi-agent: both can send and see each other" $ do
        let cfgA = mkChCfg "agent-a" "ch-test-3"
            cfgB = mkChCfg "agent-b" "ch-test-3"
        cleanChLogs cfgA

        chA <- channelOpen cfgA
        threadDelay 200_000

        chB <- channelAttach cfgB

        channelSend chA "ping from A"
        channelSend chB "pong from B"

        mMsgsA <- channelRecvBlocking chA 5_000_000
        mMsgsB <- channelRecvBlocking chB 5_000_000
        channelClose chA
        threadDelay 100_000

        case (mMsgsA, mMsgsB) of
          (Nothing, _) -> assertFailure "agent A timed out"
          (_, Nothing) -> assertFailure "agent B timed out"
          (Just msgsA, Just msgsB) -> do
            let sendersA = map fst msgsA
                sendersB = map fst msgsB
            assertBool "A sees B" ("agent-b" `elem` sendersA)
            assertBool "A sees itself" ("agent-a" `elem` sendersA)
            assertBool "B sees A" ("agent-a" `elem` sendersB)
            assertBool "B sees itself" ("agent-b" `elem` sendersB),
      testCase "blocking recv times out when no messages" $ do
        let cfg = mkChCfg "agent-x" "ch-test-4"
        cleanChLogs cfg

        ch <- channelOpen cfg
        threadDelay 200_000

        mMsgs <- channelRecvBlocking ch 1_000_000
        channelClose ch
        threadDelay 100_000

        assertBool "should time out with Nothing" (isNothing mMsgs),
      testCase "blocking recv returns messages when they arrive" $ do
        let cfg = mkChCfg "agent-sender" "ch-test-5"
        cleanChLogs cfg

        ch <- channelOpen cfg
        threadDelay 200_000

        channelSend ch "arriving message"
        threadDelay 100_000

        mMsgs <- channelRecvBlocking ch 5_000_000
        channelClose ch
        threadDelay 100_000

        case mMsgs of
          Nothing -> assertFailure "expected messages but timed out"
          Just msgs -> do
            assertBool "should have messages" (not (null msgs))
            let (sender, body) = head msgs
            assertEqual "sender" "agent-sender" sender
            assertEqual "body" "arriving message" body
    ]
  where
    mkChCfg name suffix =
      ChannelConfig
        { chStdinPath = "/tmp/ch-test-stdin-" <> suffix,
          chStdoutPath = "/tmp/ch-test-stdout-" <> suffix <> ".md",
          chStderrPath = "/tmp/ch-test-stderr-" <> suffix <> ".md",
          chName = name,
          chWorkingDir = "."
        }

    cleanChLogs cfg = do
      mapM_
        (\p -> whenM (doesFileExist p) (removeFile p))
        [chStdinPath cfg, chStdoutPath cfg, chStderrPath cfg]
      whenM (doesFileExist (chStdinPath cfg)) (removeFile (chStdinPath cfg))


-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb action = do
  b <- mb
  when b action
