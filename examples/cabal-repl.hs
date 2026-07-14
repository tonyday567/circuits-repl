{-# LANGUAGE OverloadedStrings #-}

-- | Free dual ends on a process agent (mock-repl stand-in for cabal/ghci).
--
-- @
--   open → commit / emit independently → close
-- @
--
-- No request–response helper in the library.  This example builds a local
-- @emitUntil@ that ties emit to a boundary + timeout — that circuit is the
-- place the clock lives, not the Repl.
--
-- @
--   cabal run cabal-repl-example
-- @
module Main where

import Circuit.Repl
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  hPutStrLn stderr "=== circuits-io free dual ends (mock-repl) ==="

  let cfg =
        defaultReplConfig
          { replCommand = "./dist-newstyle/build/aarch64-osx/ghc-9.14.1/circuits-io-0.1.0.0/x/mock-repl/build/mock-repl/mock-repl",
            replArgs = ["--prompt=ghci> ", "--delay=10"],
            replWorkingDir = "."
          }

  r <- replOpen cfg
  threadDelay 300_000

  -- Drain startup by tying emit to a local boundary (example only).
  _ <- emitUntil (T.isSuffixOf "ghci> ") 5_000_000 r

  let (_write, _read) = endsRepl r
  hPutStrLn stderr "endsRepl: free Commit + Emit in hand"

  -- Free commit, then a local turn circuit.
  replCommit r [":t id"]
  mType <- emitUntil (T.isSuffixOf "ghci> ") 10_000_000 r
  TIO.putStrLn "=== :t id ==="
  mapM_ TIO.putStrLn (maybe [] id mType)

  replCommit r ["add 3"]
  mAdd <- emitUntil (T.isSuffixOf "ghci> ") 10_000_000 r
  TIO.putStrLn "\n=== add 3 ==="
  mapM_ TIO.putStrLn (maybe [] id mAdd)

  -- Free emit without a turn: non-blocking harvest.
  extra <- replEmit r
  TIO.putStrLn $ "\nfree emit (should be empty): " <> T.pack (show extra)

  replClose r
  hPutStrLn stderr "=== done ==="

-- | Local tie: poll free emit until boundary or timeout (µs).
-- Not exported from Circuit.Repl — lives with the runner.
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
