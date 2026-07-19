{-# LANGUAGE OverloadedStrings #-}

-- | Multi-turn exploration against a real @cabal repl@.
--
-- Feeds a scripted sequence of @:t@ / @:info@ commands, harvests stdout on
-- @peOut@ and stderr on @peErr@, and demonstrates independent cursors across
-- multiple turns.
--
-- State under @$HOME/mg/logs/process-harness/cabal-repl-explore/@.
--
-- @
--   cabal run cabal-repl-explore
-- @
module Main (main) where

import Circuit (run)
import Circuit.Ends (Ends (..), HasUnit (..), In (..), Out (..))
import Circuit.Repl
import Circuit.Trace (Trace (..))
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  home <- getEnv "HOME"
  let project = home </> "haskell" </> "circuits"
      session = "cabal-repl-explore"
      dir = home </> "mg" </> "logs" </> "process-harness" </> session

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

  hPutStrLn stderr "=== cabal-repl-explore: multi-turn type exploration ==="
  hPutStrLn stderr $ "project=" <> project

  pp <- openProcessPorts cfg

  -- Cold build may take a while.
  mStartup <- emitOutUntil isGhciPrompt 180_000_000 pp
  case mStartup of
    Nothing -> failMsg "timed out waiting for initial ghci prompt"
    Just _ -> pure ()

  let turns =
        [ ":t Circuit.Trace.Trace",
          ":t Circuit.Ends.In",
          ":t Circuit.Ends.Out",
          ":t Circuit.Ends.commit",
          ":t Circuit.Ends.emit",
          ":t Circuit.Ends.close",
          ":t Circuit.Ends.HasUnit",
          ":t Circuit.Monoidal.par",
          ":t Circuit.Queue.openSTM",
          ":t Circuit.Repl.portsEnds",
          ":info Circuit.Trace.Trace"
        ]

  mapM_ (turn pp) turns

  hPutStrLn stderr "-- final stderr harvest --"
  errRemaining <- emitErr pp
  unless (null errRemaining) $ do
    TIO.putStrLn "\n=== stderr remaining ==="
    mapM_ TIO.putStrLn errRemaining

  peClose pp
  hPutStrLn stderr "=== DONE ==="

-- ---------------------------------------------------------------------------
-- One turn: commit a command, wait for prompt on stdout, print response.
-- ---------------------------------------------------------------------------

turn :: ProcessPorts [Text] [Text] [Text] -> Text -> IO ()
turn pp cmd = do
  TIO.putStrLn $ "\n-- turn: " <> cmd <> " --"
  commitLines pp [cmd]
  mOut <- emitOutUntil isGhciPrompt 60_000_000 pp
  case mOut of
    Nothing -> failMsg $ "timeout on command: " <> T.unpack cmd
    Just ls -> mapM_ TIO.putStrLn (filter (not . T.null) ls)

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
  "> " `T.isSuffixOf` t
    || "ghci> " `T.isSuffixOf` t
    || "λ> " `T.isSuffixOf` t

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  exitFailure
