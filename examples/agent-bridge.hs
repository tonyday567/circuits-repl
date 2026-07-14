{-# LANGUAGE OverloadedStrings #-}

-- | Free dual ends: a process agent + a shared channel.
--
-- Opens hermes (or any CLI) via 'replOpenPty' / 'replOpen' without waiting for
-- a prompt.  Channel messages are free emit; feeding the agent is free commit.
-- Any turn logic (boundary + timeout) is local to this runner.
--
-- @
--   cabal run agent-bridge
-- @
--
-- Write to the channel via:
--   echo "[you] your message" > /tmp/channel-stdin
module Main where

import Circuit.Comm
import Circuit.Repl
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "=== Agent bridge: free dual ends ==="

  home <- getEnv "HOME"
  let dir = home </> "mg" </> "logs" </> "process-harness" </> "agent-bridge"
  createDirectoryIfMissing True dir
  let agentCfg =
        defaultReplConfig
          { replCommand = "hermes",
            replArgs = ["chat", "--cli", "--max-turns", "50"],
            replWorkingDir = ".",
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = dir </> "stdout.md",
            replStderrPath = dir </> "stderr.md"
          }

  hPutStrLn stderr "Opening hermes on PTY (no prompt wait)..."
  agent <- replOpenPty agentCfg

  let chCfg = defaultChannelConfig "agent-bridge"
  ch <- channelAttach chCfg

  hPutStrLn stderr "Free loop: channel recv → agent commit; agent emit → stderr"
  loop agent ch

loop :: Repl -> Channel -> IO ()
loop agent ch = do
  msgs <- channelRecv ch
  forM_ msgs $ \(sender, body) ->
    when (sender /= "agent") $ do
      hPutStrLn stderr $ "  ← [" <> T.unpack sender <> "] " <> T.unpack (T.take 80 body)
      -- free commit: no wait for response in the library
      replCommit agent [body]

  -- free emit: harvest whatever the agent printed since last poll
  out <- replEmit agent
  when (not (null out)) $
    hPutStrLn stderr $
      "  agent emit (" <> show (length out) <> " lines): " <> T.unpack (T.take 80 (T.unlines out))

  threadDelay 2_000_000
  loop agent ch
