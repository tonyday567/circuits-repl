{-# LANGUAGE OverloadedStrings #-}

-- | Free dual ends against a real @cabal repl@ session.
--
-- Opens @cabal repl@ on @~/haskell/cursor@, commits @:t id@ and @1+1@,
-- harvests with a local @emitUntil@ (boundary + timeout live here, not in
-- Circuit.Repl). Demonstrates attach as a second free reader.
--
-- State under @$HOME/mg/logs/process-harness/cursor-io-real/@.
--
-- @
--   cabal run cabal-repl-real
-- @
module Main (main) where

import Circuit.Repl
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
  let project = home </> "haskell" </> "cursor"
      session = "cursor-io-real"
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

  hPutStrLn stderr "=== circuits-io free dual ends (real cabal repl) ==="
  hPutStrLn stderr $ "project=" <> project

  r <- replOpen cfg
  -- cold build may take a while; timeout is on this local tie only
  mStartup <- emitUntil isGhciPrompt 180_000_000 r
  case mStartup of
    Nothing -> failMsg "timed out waiting for initial ghci prompt"
    Just _ -> pure ()

  hPutStrLn stderr "-- commit :t id --"
  replCommit r [":t id"]
  mType <- emitUntil isGhciPrompt 60_000_000 r
  case mType of
    Nothing -> failMsg "timeout on :t id"
    Just ls -> do
      mapM_ TIO.putStrLn ls
      unless (any ("id ::" `T.isInfixOf`) ls) $
        failMsg "expected 'id ::' in :t id response"

  hPutStrLn stderr "-- commit 1+1 --"
  replCommit r ["1+1"]
  mSum <- emitUntil isGhciPrompt 60_000_000 r
  case mSum of
    Nothing -> failMsg "timeout on 1+1"
    Just ls -> do
      mapM_ TIO.putStrLn ls
      unless (any (("2" ==) . T.strip) ls || any ("2" `T.isInfixOf`) ls) $
        failMsg "expected '2' in 1+1 response"

  hPutStrLn stderr "-- attach second cursor (free emit from tail) --"
  bob <- replAttach cfg
  stale <- replEmit bob
  unless (null stale) $
    failMsg "attach should start at log tail"

  replCommit r [":t const"]
  -- owner drains; bob may also free-emit the same log
  mBob <- emitUntil isGhciPrompt 60_000_000 bob
  case mBob of
    Nothing -> failMsg "bob did not see :t const"
    Just ls -> mapM_ TIO.putStrLn ls

  replClose r
  hPutStrLn stderr "=== all checks passed ==="

isGhciPrompt :: Text -> Bool
isGhciPrompt t =
  "ghci> " `T.isSuffixOf` t
    || "λ> " `T.isSuffixOf` t
    || "> " `T.isSuffixOf` t

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

failMsg :: String -> IO a
failMsg msg = do
  hPutStrLn stderr $ "FAIL: " <> msg
  exitFailure
