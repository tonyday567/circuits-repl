{-# LANGUAGE OverloadedStrings #-}

module Main where

import Circuit.Comm
import Circuit.Repl
import Circuit.Session
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_, when)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import MockBackend (MockMode (..), openMockRepl)
import System.Directory (doesFileExist, removeFile, removePathForcibly)
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
        hermesTests,
        channelTests,
        sessionTests
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
-- Hermes session-file backend
-- ---------------------------------------------------------------------------

hermesTests :: TestTree
hermesTests =
  testGroup
    "BackendHermes (session JSON)"
    [ testCase "commit user then emit assistant" $ do
        let path = "/tmp/circuits-io-hermes-session.json"
        removePathForcibly path
        removePathForcibly (path <> ".lock")
        removePathForcibly (path <> ".cursor-hermes")
        writeMinimalSession path
          [ object ["role" .= ("user" :: Text), "content" .= ("hi" :: Text)]
          , object ["role" .= ("assistant" :: Text), "content" .= ("hello" :: Text)]
          ]

        r <- replOpenHermes path
        -- tail attach: history not re-emitted
        early <- replEmit r
        assertBool "no history on open" (null early)

        replCommit r ["what is 2+2?"]
        -- still no assistant reply yet
        mid <- replEmit r
        assertBool "no assistant yet" (null mid)

        -- simulate Hermes writing an assistant reply (+ empty tool-call skip)
        appendAssistant path "4"
        appendAssistantEmpty path

        out <- replEmit r
        assertEqual "assistant content" ["4"] out

        again <- replEmit r
        assertBool "idempotent emit" (null again)

        replClose r
    ]

writeMinimalSession :: FilePath -> [Value] -> IO ()
writeMinimalSession path msgs = do
  let o =
        object
          [ "session_id" .= ("test" :: Text)
          , "message_count" .= length msgs
          , "messages" .= msgs
          ]
  LBS.writeFile path (encode o)

appendAssistant :: FilePath -> Text -> IO ()
appendAssistant path content = do
  bs <- LBS.readFile path
  case eitherDecode bs of
    Left err -> assertFailure ("session decode: " <> err)
    Right (Object o) -> do
      let old = case KM.lookup "messages" o of
            Just (Array arr) -> arr
            _ -> V.empty
          msg = object ["role" .= ("assistant" :: Text), "content" .= content]
          new = old <> V.singleton msg
          o' =
            KM.insert "messages" (Array new) $
              KM.insert "message_count" (Number (fromIntegral (V.length new))) o
      LBS.writeFile path (encode (Object o'))
    Right _ -> assertFailure "expected object"

appendAssistantEmpty :: FilePath -> IO ()
appendAssistantEmpty path = appendAssistant path ""

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
-- Session tests (protocol: ask/answer, tell/recv)
-- ---------------------------------------------------------------------------

sessionTests :: TestTree
sessionTests =
  testGroup
    "Session (ask/answer protocol)"
    [ testCase "parseMsg broadcast" $ do
        assertEqual
          "broadcast"
          (Just (Broadcast "sender" "hello world"))
          (parseMsg "sender" "hello world"),
      testCase "parseMsg question" $ do
        assertEqual
          "question"
          (Just (Question "agent" "agent.0" "should I refactor?"))
          (parseMsg "agent" "? agent.0 should I refactor?"),
      testCase "parseMsg answer" $ do
        assertEqual
          "answer"
          (Just (Answer "agent" "agent.0" "yes go ahead"))
          (parseMsg "agent" "! agent.0 yes go ahead"),
      testCase "parseMsg rejects malformed question (no id)" $ do
        assertBool "no id" (isNothing (parseMsg "agent" "? ")),
      testCase "parseMsg rejects malformed question (no body)" $ do
        assertBool "no body" (isNothing (parseMsg "agent" "? x ")),
      testCase "tell and recv" $ do
        let cfgA = mkSessCfg "agent-a" "sess-bcast"
        cleanSessLogs cfgA

        sessA <- sessionOpen cfgA
        threadDelay 200_000

        tell sessA "hello from session A"
        threadDelay 500_000

        msgs <- recv sessA
        sessionClose sessA
        threadDelay 100_000

        assertBool "should have at least one message" (not (null msgs))
        case head msgs of
          Broadcast sender body -> do
            assertEqual "sender" "agent-a" sender
            assertEqual "body" "hello from session A" body
          _ -> assertFailure "expected Broadcast",
      testCase "ask and answer across two sessions" $ do
        let cfgA = mkSessCfg "agent-a" "sess-ask"
            cfgB = mkSessCfg "agent-b" "sess-ask"
        cleanSessLogs cfgA

        sessA <- sessionOpen cfgA
        threadDelay 200_000

        sessB <- sessionAttach cfgB sessA
        threadDelay 200_000

        resultMVar <- newEmptyMVar
        _ <- forkIO $ do
          reply <- ask sessA "should I refactor Baz.hs?"
          putMVar resultMVar reply

        qMsgs <- waitForMessages sessB 5_000_000
        case qMsgs of
          Nothing -> assertFailure "B timed out waiting for question"
          Just msgs -> do
            assertBool "B should see a question" (any isQuestion msgs)
            case findQuestion msgs of
              Nothing -> assertFailure "no Question in messages"
              Just (Question _sender qid _body) -> do
                answer sessB qid "yes, definitely refactor"

        reply <- takeMVar resultMVar
        assertEqual "answer body" "yes, definitely refactor" reply

        sessionClose sessA
        threadDelay 100_000,
      testCase "two questions, interleaved answers" $ do
        let cfgA = mkSessCfg "agent-a" "sess-multi"
            cfgB = mkSessCfg "agent-b" "sess-multi"
        cleanSessLogs cfgA

        sessA <- sessionOpen cfgA
        threadDelay 200_000

        sessB <- sessionAttach cfgB sessA
        threadDelay 200_000

        rawSend sessA "? a.q1 question one"
        rawSend sessA "? a.q2 question two"
        threadDelay 300_000

        bMsgs <- waitForMessagesN sessB 2 5_000_000
        case bMsgs of
          Nothing -> assertFailure "B timed out waiting for questions"
          Just msgs -> do
            let qs = filter isQuestion msgs
            assertBool "should see at least two questions" (length qs >= 2)
            forM_ qs $ \case
              Question _ qid _ -> answer sessB qid "done"
              _ -> pure ()

        threadDelay 300_000
        aMsgs <- recv sessA
        let answers = filter isAnswer aMsgs
        assertBool "A should see at least two answers" (length answers >= 2)

        sessionClose sessA
        threadDelay 100_000
    ]
  where
    mkSessCfg name suffix =
      SessionConfig
        { sessChannel =
            ChannelConfig
              { chStdinPath = "/tmp/sess-test-stdin-" <> suffix,
                chStdoutPath = "/tmp/sess-test-stdout-" <> suffix <> ".md",
                chStderrPath = "/tmp/sess-test-stderr-" <> suffix <> ".md",
                chName = name,
                chWorkingDir = "."
              },
          sessName = name
        }

    cleanSessLogs cfg = do
      let ch = sessChannel cfg
      mapM_
        (\p -> whenM (doesFileExist p) (removeFile p))
        [chStdinPath ch, chStdoutPath ch, chStderrPath ch]
      whenM (doesFileExist (chStdinPath ch)) (removeFile (chStdinPath ch))

    isQuestion :: Msg -> Bool
    isQuestion Question {} = True
    isQuestion _ = False

    isAnswer :: Msg -> Bool
    isAnswer Answer {} = True
    isAnswer _ = False

    findQuestion :: [Msg] -> Maybe Msg
    findQuestion = foldr (\m acc -> if isQuestion m then Just m else acc) Nothing

    waitForMessages :: Session -> Int -> IO (Maybe [Msg])
    waitForMessages sess timeoutUs = go 0 10000
      where
        go elapsed delay = do
          msgs <- recv sess
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

    waitForMessagesN :: Session -> Int -> Int -> IO (Maybe [Msg])
    waitForMessagesN sess n timeoutUs = go 0 10000 []
      where
        go elapsed delay acc = do
          msgs <- recv sess
          let acc' = acc ++ msgs
          if length acc' >= n
            then pure (Just acc')
            else do
              let elapsed' = elapsed + delay
              if elapsed' >= timeoutUs
                then pure Nothing
                else do
                  threadDelay delay
                  let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
                  go elapsed' delay' acc'

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb action = do
  b <- mb
  when b action
