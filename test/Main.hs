{-# LANGUAGE OverloadedStrings #-}

-- | Oracle tests for Cursor — mem and file backends, same type.
module Main (main) where

import Cursor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Prelude

main :: IO ()
main = do
  putStrLn "cursor oracle"
  memOracle
  fileOracle
  parityOracle
  truncOracle
  putStrLn "all green"

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  if expected == actual
    then putStrLn $ "  ok  " <> label
    else do
      putStrLn $ "  FAIL " <> label
      putStrLn $ "    expected: " <> show expected
      putStrLn $ "    actual:   " <> show actual
      exitFailure

memOracle :: IO ()
memOracle = do
  putStrLn "mem"
  c <- newMem 0
  n0 <- get c
  assertEq "start at 0" 0 n0
  a <- pollLines c ["a", "b"]
  assertEq "first poll" (["a", "b"] :: [Text]) a
  b <- pollLines c ["a", "b", "c"]
  assertEq "delta poll" (["c"] :: [Text]) b
  empty <- pollLines c ["a", "b", "c"]
  assertEq "idempotent" ([] :: [Text]) empty
  seekEnd c ["a", "b", "c", "d"]
  d <- pollLines c ["a", "b", "c", "d", "e"]
  assertEq "seekEnd then poll" (["e"] :: [Text]) d

fileOracle :: IO ()
fileOracle =
  withSystemTempDirectory "cursor-test" $ \dir -> do
    putStrLn "file"
    let curPath = dir </> ".cursor-alice"
        logPath = dir </> "log.md"
    c <- newFile curPath
    n0 <- get c
    assertEq "missing file → 0" 0 n0
    TIO.writeFile logPath "one\ntwo\nthree\n"
    got <- pollFile c logPath
    assertEq "pollFile first" (["one", "two", "three"] :: [Text]) got
    pos <- get c
    assertEq "pos after poll" 3 pos
    raw <- readFile curPath
    assertEq "file format" "3\n" raw
    TIO.appendFile logPath "four\n"
    got2 <- pollFile c logPath
    assertEq "pollFile delta" (["four"] :: [Text]) got2
    -- second reader on same log, own file cursor
    c2 <- newFile (dir </> ".cursor-bob")
    seekEndFile c2 logPath
    TIO.appendFile logPath "five\n"
    fromBob <- pollFile c2 logPath
    fromAlice <- pollFile c logPath
    assertEq "bob sees five" (["five"] :: [Text]) fromBob
    assertEq "alice sees five" (["five"] :: [Text]) fromAlice

-- | A stale cursor (position beyond the current log length) resets to 0 so
-- deleting or truncating a log does not leave the cursor permanently broken.
truncOracle :: IO ()
truncOracle =
  withSystemTempDirectory "cursor-trunc" $ \dir -> do
    putStrLn "trunc"
    let curPath = dir </> ".cursor"
        logPath = dir </> "log.md"
    c <- newFile curPath
    TIO.writeFile logPath "one\ntwo\nthree\n"
    _ <- pollFile c logPath
    TIO.writeFile logPath "alpha\nbeta\n"
    got <- pollFile c logPath
    assertEq "stale cursor replays current log" (["alpha", "beta"] :: [Text]) got
    got2 <- pollFile c logPath
    assertEq "then idempotent" ([] :: [Text]) got2

-- | Same poll sequence on mem and file backends yields the same news.
parityOracle :: IO ()
parityOracle =
  withSystemTempDirectory "cursor-parity" $ \dir -> do
    putStrLn "parity mem≡file"
    let logPath = dir </> "log.md"
        steps =
          [ "a\n" :: Text
          , "a\nb\n"
          , "a\nb\nc\n"
          ]
    mem <- newMem 0
    file <- newFile (dir </> ".cursor")
    TIO.writeFile logPath ""
    mapM_
      ( \content -> do
          TIO.writeFile logPath content
          let ls = T.lines content
          m <- pollLines mem ls
          f <- pollFile file logPath
          assertEq ("parity " <> show ls) m f
      )
      steps
