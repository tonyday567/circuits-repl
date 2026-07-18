{-# LANGUAGE OverloadedStrings #-}

-- | Cabal repl as a muster channel participant.
--
-- Owns a real @cabal repl@ via 'ProcessPorts' and speaks to a muster channel.
-- Output is posted as the configured agent name; commands are accepted when
-- they mention @<agent-name>@ (default @repl@).
--
-- Usage:
-- @
--   cabal run cabal-repl-muster -- <project-dir> <channel> <agent-name>
-- @
--
-- Defaults: project @~/haskell/circuits@, channel @cabal-repl@, agent @repl@.
--
-- Example session:
-- @
--   muster -c cabal-repl post tony "@repl :t Circuit.Trace.Trace"
--   muster -c cabal-repl read tony
-- @
module Main (main) where

import Circuit (run)
import Circuit.Ends (openK)
import Circuit.Repl
import Circuit.Trace (runIn, runOut)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (guard, unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removePathForcibly)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  home <- getHomeDirectory
  let (project, channel, agent) = parseArgs home args
      session = T.unpack (safeName (T.pack (takeBaseName project)) <> "-" <> T.pack channel)
      dir = home </> "mg" </> "logs" </> "process-harness" </> "cabal-repl-muster" </> session

  removePathForcibly dir
  createDirectoryIfMissing True dir

  let cfg =
        defaultReplConfig
          { replCommand = "cabal",
            replArgs = ["repl"],
            replWorkingDir = project,
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = dir </> "stdout.md",
            replStderrPath = dir </> "stderr.md"
          }

  hPutStrLn stderr $ "=== cabal-repl-muster: " <> project <> " ==="
  hPutStrLn stderr $ "channel=" <> channel <> " agent=" <> agent <> " dir=" <> dir

  musterJoin channel agent
  hPutStrLn stderr "joined muster channel"

  musterPost channel agent $ "starting cabal repl in " <> T.pack project

  pp <- openProcessPorts cfg

  hPutStrLn stderr "waiting for initial ghci prompt..."
  startup <- emitOutUntil isGhciPrompt 180_000_000 pp >>= \case
    Nothing -> failMsg "timed out waiting for initial ghci prompt"
    Just ls -> do
      hPutStrLn stderr $ "startup " <> show (length ls) <> " stdout lines"
      pure ls

  err0 <- emitErr pp
  postOutput channel agent 0 startup err0

  runLoop pp channel agent 1

runLoop :: ProcessPorts [Text] [Text] [Text] -> String -> String -> Int -> IO ()
runLoop pp channel agent turn = do
  cmds <- readCommands channel agent
  case cmds of
    [] -> do
      threadDelay 1_000_000
      runLoop pp channel agent turn
    _ -> do
      hPutStrLn stderr $ "turn " <> show turn <> ": " <> show (length cmds) <> " command(s)"
      done <- processCommands pp channel agent turn cmds
      if done
        then do
          musterPost channel agent "quit received; closing"
          peClose pp
        else runLoop pp channel agent (turn + length cmds)

processCommands :: ProcessPorts [Text] [Text] [Text] -> String -> String -> Int -> [Text] -> IO Bool
processCommands _ _ _ _ [] = pure False
processCommands pp channel agent turn (cmd : rest) = do
  if cmd == "quit" || cmd == ":quit"
    then pure True
    else do
      musterPost channel agent $ "exec turn " <> T.pack (show turn) <> ": " <> cmd
      commitLines pp [cmd]
      outLines <- emitOutUntil isGhciPrompt 60_000_000 pp >>= \case
        Nothing -> do
          hPutStrLn stderr "no new stdout prompt; continuing"
          pure []
        Just ls -> pure ls
      errLines <- emitErr pp
      postOutput channel agent turn outLines errLines
      processCommands pp channel agent (turn + 1) rest

readCommands :: String -> String -> IO [Text]
readCommands channel agent = do
  raw <- musterRead channel agent
  let ls = T.lines (T.pack raw)
  pure [c | Just (name, body) <- map parseMusterLine ls, name /= T.pack agent, Just c <- [extractCommand agent body]]

parseMusterLine :: Text -> Maybe (Text, Text)
parseMusterLine t = do
  guard ("[" `T.isPrefixOf` t)
  let rest = T.drop 1 t
  case T.breakOn "]" rest of
    (_, "") -> Nothing
    (name, after) -> Just (name, T.strip (T.drop 1 after))

extractCommand :: String -> Text -> Maybe Text
extractCommand agent body = do
  let needle = "@" <> T.pack agent
  guard (needle `T.isInfixOf` body)
  let after = T.drop (T.length needle) (snd (T.breakOn needle body))
      stripped = T.strip (T.dropWhile (`elem` [' ', '\t']) after)
  guard (not (T.null stripped))
  pure stripped

postOutput :: String -> String -> Int -> [Text] -> [Text] -> IO ()
postOutput channel agent turn outLines errLines = do
  let header = "turn " <> T.pack (show turn) <> " output"
      body = T.unlines $ [header, "", "-- stdout --"] <> outLines <> ["", "-- stderr --"] <> errLines
  musterPost channel agent body

musterJoin :: String -> String -> IO ()
musterJoin channel agent = do
  _ <- readProcess "muster" ["-c", channel, "join", agent] ""
  pure ()

musterPost :: String -> String -> Text -> IO ()
musterPost channel agent msg = do
  _ <- readProcess "muster" ["-c", channel, "post", agent, T.unpack msg] ""
  pure ()

musterRead :: String -> String -> IO String
musterRead channel agent = do
  (code, out, err) <- readProcessWithExitCode "muster" ["-c", channel, "read", agent] ""
  case code of
    ExitSuccess -> pure out
    _ -> do
      hPutStrLn stderr $ "muster read failed: " <> err
      pure ""

commitLines :: ProcessPorts [Text] [Text] [Text] -> [Text] -> IO ()
commitLines pp ts = runKleisli (run (runOut (peIn pp) outU)) ts
  where
    (outU, _) = openK ()

emitOutUntil :: (Text -> Bool) -> Int -> ProcessPorts [Text] [Text] [Text] -> IO (Maybe [Text])
emitOutUntil p t pp = emitUntil p t (emitOut pp)

emitOut :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitOut pp = runKleisli (run (runIn (peOut pp) inU)) ()
  where
    (_, inU) = openK ()

emitErr :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitErr pp = runKleisli (run (runIn (peErr pp) inU)) ()
  where
    (_, inU) = openK ()

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

parseArgs :: FilePath -> [String] -> (FilePath, String, String)
parseArgs home args =
  case args of
    [p, c, a] -> (p, c, a)
    [p, c] -> (p, c, "repl")
    [p] -> (p, "cabal-repl", "repl")
    [] -> (home </> "haskell" </> "circuits", "cabal-repl", "repl")
    _ -> (home </> "haskell" </> "circuits", "cabal-repl", "repl")

safeName :: Text -> Text
safeName = T.map go
  where
    go c
      | c `elem` (['/','\\',' ',':'] :: String) = '-'
      | otherwise = c

takeBaseName :: FilePath -> String
takeBaseName = go . reverse
  where
    go [] = []
    go ('/':rs) = go rs
    go rs = reverse (takeWhile (/= '/') rs)

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  exitFailure
