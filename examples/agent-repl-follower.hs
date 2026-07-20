{-# LANGUAGE OverloadedStrings #-}

-- | Sample agent for the file-based agent-repl-loop.
--
-- Reads per-turn state files and writes back the next command. This follower
-- implements a fixed exploration ladder; a smarter agent would parse stdout and
-- decide dynamically.
--
-- Usage:
-- @
--   cabal run agent-repl-follower -- <session>
-- @
--
-- Run this after `cabal run agent-repl-loop -- <session>`.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let session = case args of
        [s] -> s
        _   -> "circuits"
  home <- getEnv "HOME"
  let dir = home </> "mg" </> "logs" </> "process-harness" </> "agent-repl" </> session

  hPutStrLn stderr $ "=== agent-repl-follower: " <> session <> " ==="
  hPutStrLn stderr $ "session dir=" <> dir

  runFollower dir 0 plan
  where
    plan =
      [ ":t Circuit.Loop.Loop",
        ":t Circuit.Ends.In",
        ":t Circuit.Ends.Out",
        ":t Circuit.Ends.commit",
        ":t Circuit.Ends.emit",
        ":t Circuit.Ends.close",
        ":t Circuit.Repl.portsEnds",
        ":info Circuit.Loop.Loop",
        "quit"
      ]

runFollower :: FilePath -> Int -> [Text] -> IO ()
runFollower _ _ [] = do
  hPutStrLn stderr "plan exhausted"
  exitSuccess
runFollower dir turn (cmd : rest) = do
  let statePath = dir </> "state" </> ("turn-" <> show turn <> ".md")
  hPutStrLn stderr $ "turn " <> show turn <> ": waiting for state " <> statePath
  waitForFile statePath 3_600_000_000

  stateText <- TIO.readFile statePath
  let stdoutLines = extractSection "## stdout" stateText
      stderrLines = extractSection "## stderr" stateText
  hPutStrLn stderr $ "turn " <> show turn <> ": read " <> show (length stdoutLines) <> " stdout lines, " <> show (length stderrLines) <> " stderr lines"

  let cmdPath = dir </> "commands" </> ("turn-" <> show turn <> ".md")
  TIO.writeFile cmdPath $
    T.unlines
      [ cmd,
        "",
        "rationale: fixed exploration ladder"
      ]
  hPutStrLn stderr $ "turn " <> show turn <> ": wrote command: " <> T.unpack cmd

  when (cmd == "quit" || cmd == ":quit") $ do
    hPutStrLn stderr "quit command issued; follower done"
    exitSuccess

  runFollower dir (turn + 1) rest

extractSection :: Text -> Text -> [Text]
extractSection heading text =
  case dropWhile (not . T.isPrefixOf heading) (T.lines text) of
    [] -> []
    (_ : rest) -> takeWhile (not . T.isPrefixOf "## ") (dropWhile T.null rest)

waitForFile :: FilePath -> Int -> IO ()
waitForFile path timeoutUs = go 0 100000
  where
    go elapsed delay = do
      exists <- doesFileExist path
      if exists
        then pure ()
        else do
          let elapsed' = elapsed + delay
          when (elapsed' >= timeoutUs) $
            failMsg $ "timed out waiting for state: " <> path
          threadDelay delay
          let delay' = min 5_000_000 (floor (fromIntegral delay * 1.5 :: Double))
          go elapsed' delay'

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  error msg
