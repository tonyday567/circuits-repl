{-# LANGUAGE OverloadedStrings #-}

-- | Spike: real @cabal repl@ with stdout and stderr split.
--
-- Uses 'Circuit.Repl.openProcessPorts' to hold three free ends:
--
--   * peIn  — stdin commit
--   * peOut — stdout harvest
--   * peErr — stderr harvest
--
-- Demonstrates that warnings / explicit stderr writes never appear on the
-- stdout cursor, and that 'portsEnds' packages the three seats into one
-- nested-par morphism.
--
-- State under @$HOME/mg/logs/process-harness/cabal-repl-split/@.
--
-- @
--   cabal run cabal-repl-split
-- @
module Main (main) where

import Circuit (run)
import Circuit.Ends (openK)
import Circuit.Repl
import Circuit.Trace (runIn, runOut)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
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
      session = "cabal-repl-split"
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

  hPutStrLn stderr "=== cabal-repl-split: real cabal repl, stdout/stderr split ==="
  hPutStrLn stderr $ "project=" <> project

  pp <- openProcessPorts cfg

  -- Cold build may take a while; timeout is on this local tie only.
  mStartup <- emitOutUntil isGhciPrompt 180_000_000 pp
  case mStartup of
    Nothing -> failMsg "timed out waiting for initial ghci prompt"
    Just _ -> pure ()

  hPutStrLn stderr "-- phase 1: stdout commands, harvested on peOut --"
  commitLines pp ["1+1"]
  sumLines <- emitOutUntil isGhciPrompt 60_000_000 pp >>= \case
    Nothing -> failMsg "timeout on 1+1"
    Just ls -> do
      TIO.putStrLn "=== stdout: 1+1 ==="
      mapM_ TIO.putStrLn ls
      unless (any (("2" ==) . T.strip) ls || any ("2" `T.isInfixOf`) ls) $
        failMsg "expected '2' in 1+1 response"
      pure ls

  commitLines pp [":t id"]
  typeLines <- emitOutUntil isGhciPrompt 60_000_000 pp >>= \case
    Nothing -> failMsg "timeout on :t id"
    Just ls -> do
      TIO.putStrLn "\n=== stdout: :t id ==="
      mapM_ TIO.putStrLn ls
      unless (any ("id ::" `T.isInfixOf`) ls) $
        failMsg "expected 'id ::' in :t id response"
      pure ls

  hPutStrLn stderr "-- phase 2: explicit stderr command, harvested on peErr --"
  commitLines pp ["import System.IO", "hPutStrLn stderr \"cabal-repl-split debug\""]
  _errLines <- emitErrUntil (T.isInfixOf "cabal-repl-split debug") 60_000_000 pp >>= \case
    Nothing -> failMsg "timeout waiting for explicit stderr line"
    Just ls -> do
      TIO.putStrLn "\n=== stderr: explicit write ==="
      mapM_ TIO.putStrLn ls
      pure ls

  -- Separation assertion: the stderr marker must not appear in stdout harvests.
  let combinedStdoutHarvest = T.unlines (typeLines <> sumLines)
  when ("cabal-repl-split debug" `T.isInfixOf` combinedStdoutHarvest) $
    failMsg "stdout harvest contained stderr marker"

  hPutStrLn stderr "-- phase 3: portsEnds snapshot of fresh stdout/stderr --"
  commitLines pp [":t const", "hPutStrLn stderr \"second debug\""]
  threadDelay 1_000_000 -- let both streams land without consuming cursors

  (_, (outSnapshot, errSnapshot)) <- runKleisli (run (portsEnds pp)) ([], ((), ()))

  TIO.putStrLn "\n=== portsEnds stdout snapshot ==="
  mapM_ TIO.putStrLn outSnapshot
  TIO.putStrLn "\n=== portsEnds stderr snapshot ==="
  mapM_ TIO.putStrLn errSnapshot

  let combinedOutSnapshot = T.unlines outSnapshot
  when ("second debug" `T.isInfixOf` combinedOutSnapshot) $
    failMsg "portsEnds stdout snapshot contained stderr marker"
  unless (any ("second debug" `T.isInfixOf`) errSnapshot) $
    failMsg "portsEnds stderr snapshot missing stderr marker"

  peClose pp
  hPutStrLn stderr "=== PASS ==="

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

commitLines :: ProcessPorts [Text] [Text] [Text] -> [Text] -> IO ()
commitLines pp ts = runKleisli (run (runOut (peIn pp) outU)) ts
  where
    (outU, _) = openK ()

emitOutUntil :: (Text -> Bool) -> Int -> ProcessPorts [Text] [Text] [Text] -> IO (Maybe [Text])
emitOutUntil p t pp = emitUntil p t (emitOut pp)

emitErrUntil :: (Text -> Bool) -> Int -> ProcessPorts [Text] [Text] [Text] -> IO (Maybe [Text])
emitErrUntil p t pp = emitUntil p t (emitErr pp)

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

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  exitFailure
