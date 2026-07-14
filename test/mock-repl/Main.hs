{-# LANGUAGE OverloadedStrings #-}

-- | Controllable mock REPL for testing the circuits-io Repl pipeline.
--
-- This simulates the awkward attributes of real REPLs and agent processes:
--   * Noisy startup output before the first prompt
--   * Custom / configurable prompt
--   * Incremental output (multiple lines per "response")
--   * Optional extra "async" noise lines
--   * Simple state (counter or last value)
--   * Line-oriented, exactly like GHCi, pi, hermes profiles, etc.
--
-- It is driven via the exact same FIFO + log mechanism as real targets.
--
-- Parking note: All development here (including the multi-round agent comms
-- thread) is safely parked in circuits-io while side-activity is on the
-- main `circuits` package. See Repl.hs, examples/cabal-repl.hs, and readme.md.
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data MockConfig = MockConfig
  { prompt :: Text,
    startupNoise :: [Text],
    responseDelayMs :: Int,
    extraNoise :: Bool,
    hangingPrompt :: Bool -- if True, print prompt with putStr (no \n) to test partial-line reading
  }

defaultConfig :: MockConfig
defaultConfig =
  MockConfig
    { prompt = "mock> ",
      startupNoise =
        [ "Mock REPL starting...",
          "Loading simulation environment...",
          "Ready."
        ],
      responseDelayMs = 50,
      extraNoise = True,
      hangingPrompt = False
    }

parseArgs :: [String] -> MockConfig
parseArgs = foldr go defaultConfig
  where
    go arg cfg
      | "--prompt=" `T.isPrefixOf` t = cfg {prompt = T.strip (T.drop 9 t) <> " "}
      | "--delay=" `T.isPrefixOf` t =
          case reads (T.unpack $ T.drop 8 t) of
            [(n, "")] -> cfg {responseDelayMs = n}
            _ -> cfg
      | "--no-extra-noise" == t = cfg {extraNoise = False}
      | "--hanging-prompt" == t = cfg {hangingPrompt = True}
      | otherwise = cfg
      where
        t = T.pack arg

main :: IO ()
main = do
  args <- getArgs
  let cfg = parseArgs args

  -- Simulate noisy startup (this is one of the hardest things real pipelines face)
  forM_ (startupNoise cfg) $ \line -> do
    TIO.putStrLn line
    hFlush stdout
    threadDelay 10_000

  if hangingPrompt cfg
    then TIO.putStr (prompt cfg)
    else TIO.putStrLn (prompt cfg)
  hFlush stdout

  loop cfg 0

loop :: MockConfig -> Int -> IO ()
loop cfg counter = do
  line <- TIO.getLine
  let trimmed = T.strip line
  unless (T.null trimmed) $ do
    threadDelay (responseDelayMs cfg * 1000)

    -- Simulate "thinking" / multiple output lines
    TIO.putStrLn $ "received: " <> trimmed

    when (extraNoise cfg) $
      TIO.putStrLn "  [mock noise: processing...]"

    -- Simple stateful response
    let response = case T.words trimmed of
          ["add", n] ->
            let val = counter + read (T.unpack n) :: Int
             in "result: " <> T.pack (show val)
          ["get"] -> "counter: " <> T.pack (show counter)
          _ -> "echo: " <> trimmed

    TIO.putStrLn response

    when (extraNoise cfg) $
      TIO.putStrLn "  [mock noise: done]"

  if hangingPrompt cfg
    then TIO.putStr (prompt cfg)
    else TIO.putStrLn (prompt cfg)
  hFlush stdout

  -- Very simple state: increment on every non-empty command
  let newCounter = if T.null trimmed then counter else counter + 1
  loop cfg newCounter
