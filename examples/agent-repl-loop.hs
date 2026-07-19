{-# LANGUAGE OverloadedStrings #-}

-- | Multi-turn REPL environment for external agents.
--
-- Owns a real @cabal repl@ via 'ProcessPorts'. After each turn it publishes
-- stdout/stderr deltas to @state/<turn>.md@ and blocks on
-- @commands/<turn>.md@ for the agent's next command.
--
-- State directory: @$HOME/mg/logs/process-harness/agent-repl/<session>/@
--
-- Usage:
-- @
--   cabal run agent-repl-loop -- <session>
-- @
--
-- The session name defaults to "circuits". The project is looked up as
-- @~/<session>@; for now only the default is supported.
module Main (main) where

import Circuit (run)
import Circuit.Ends (Ends (..), HasUnit (..), In (..), Out (..))
import Circuit.Repl
import Circuit.Trace (Trace (..))
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let session = case args of
        [s] -> s
        _   -> "circuits"
  home <- getEnv "HOME"
  let project = home </> "haskell" </> session
      dir = home </> "mg" </> "logs" </> "process-harness" </> "agent-repl" </> session

  -- Clean session directory for a fresh run.
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "state")
  createDirectoryIfMissing True (dir </> "commands")

  let cfg =
        defaultReplConfig
          { replCommand = "cabal",
            replArgs = ["repl"],
            replWorkingDir = project,
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = dir </> "stdout.md",
            replStderrPath = dir </> "stderr.md"
          }

  hPutStrLn stderr $ "=== agent-repl-loop: " <> session <> " ==="
  hPutStrLn stderr $ "project=" <> project
  hPutStrLn stderr $ "session dir=" <> dir

  pp <- openProcessPorts cfg

  hPutStrLn stderr "waiting for initial ghci prompt..."
  startup <- emitOutUntil isGhciPrompt 180_000_000 pp >>= \case
    Nothing -> failMsg "timed out waiting for initial ghci prompt"
    Just ls -> do
      hPutStrLn stderr $ "startup " <> show (length ls) <> " stdout lines"
      pure ls

  -- Turn 0 state is the startup output; agent decides the first command.
  runLoop pp dir 0 startup

runLoop :: ProcessPorts [Text] [Text] [Text] -> FilePath -> Int -> [Text] -> IO ()
runLoop pp dir turn outLines = do
  errLines <- emitErr pp
  hPutStrLn stderr $ "turn " <> show turn <> ": publishing state"
  writeState dir turn outLines errLines

  -- Wait for agent command.
  let cmdPath = commandPath dir turn
  hPutStrLn stderr $ "turn " <> show turn <> ": waiting for " <> cmdPath
  cmd <- waitForCommand cmdPath 3_600_000_000 -- 1 hour total patience

  if cmd == "quit" || cmd == ":quit"
    then do
      hPutStrLn stderr "quit received; closing"
      peClose pp
    else do
      hPutStrLn stderr $ "turn " <> show turn <> ": executing: " <> T.unpack cmd
      commitLines pp [cmd]

      -- Collect output produced by this command for the next turn's state.
      nextOut <- emitOutUntil isGhciPrompt 60_000_000 pp >>= \case
        Nothing -> do
          hPutStrLn stderr "no new stdout prompt; closing"
          pure []
        Just ls -> pure ls

      runLoop pp dir (turn + 1) nextOut

writeState :: FilePath -> Int -> [Text] -> [Text] -> IO ()
writeState dir turn outLines errLines = do
  let path = dir </> "state" </> ("turn-" <> show turn <> ".md")
  TIO.writeFile path $
    T.unlines
      [ "# turn " <> T.pack (show turn),
        "",
        "## stdout",
        "",
        T.unlines outLines,
        "## stderr",
        "",
        T.unlines errLines
      ]

waitForCommand :: FilePath -> Int -> IO Text
waitForCommand path timeoutUs = go 0 100000
  where
    go elapsed delay = do
      exists <- doesFileExist path
      if exists
        then do
          content <- TIO.readFile path
          case T.lines content of
            [] -> failMsg $ "empty command file: " <> path
            (l : _) -> pure (T.strip l)
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= timeoutUs
            then failMsg $ "timed out waiting for command: " <> path
            else do
              threadDelay delay
              let delay' = min 5_000_000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' delay'

commitLines :: ProcessPorts [Text] [Text] [Text] -> [Text] -> IO ()
commitLines pp ts = runKleisli (run (runOut (peIn pp) outU)) ts
  where
    Ends _ outU = open

emitOutUntil :: (Text -> Bool) -> Int -> ProcessPorts [Text] [Text] [Text] -> IO (Maybe [Text])
emitOutUntil p t pp = emitUntil p t (emitOut pp)

emitOut :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitOut pp = runKleisli (run (runIn (peOut pp) inU)) ()
  where
    Ends inU _ = open

emitErr :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitErr pp = runKleisli (run (runIn (peErr pp) inU)) ()
  where
    Ends inU _ = open

emitUntil :: (Text -> Bool) -> Int -> IO [Text] -> IO (Maybe [Text])
emitUntil isBoundary timeoutUs emit = go 0 [] 10000
  where
    go elapsed acc delay = do
      news <- emit
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

isGhciPrompt :: Text -> Bool
isGhciPrompt t =
  "ghci> " `T.isSuffixOf` t
    || "λ> " `T.isSuffixOf` t
    || "> " `T.isSuffixOf` t

commandPath :: FilePath -> Int -> FilePath
commandPath dir turn = dir </> "commands" </> ("turn-" <> show turn <> ".md")

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  exitFailure
