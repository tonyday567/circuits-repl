{-# LANGUAGE OverloadedStrings #-}

-- | Dual-mode fake process for Backend abstraction tests.
--
--   * 'FakeFifo'  — commit writes the response into the log /directly/
--     (child-writes-fd model).
--   * 'FakePty'   — commit enqueues to a 'Chan'; a pump thread appends
--     to the log (parent-pumps-bytes model).
--
-- Both expose the same free dual: 'replCommit' / 'replEmit'.
module MockBackend
  ( MockMode (..),
    openMockRepl,
  )
where

import Circuit.Repl
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Prelude

data MockMode
  = -- | Commit appends response to log immediately (FIFO analogy).
    FakeFifo
  | -- | Commit enqueues; pump thread appends (PTY analogy).
    FakePty
  deriving (Eq, Show)

-- | Open a mock 'Repl' under @/tmp/circuits-io-backend-\<tag\>/@.
-- Prompt is fixed @mock> @ (with trailing space).
openMockRepl :: MockMode -> String -> IO Repl
openMockRepl mode tag = do
  let dir = "/tmp" </> ("circuits-io-backend-" <> tag)
  removePathForcibly dir
  createDirectoryIfMissing True dir
  let logPath = dir </> "stdout.md"
      cfg =
        defaultReplConfig
          { replCommand = "mock",
            replArgs = [],
            replWorkingDir = dir,
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = logPath,
            replStderrPath = dir </> "stderr.md"
          }
  writeFile logPath ""
  -- Initial prompt so free emit can see something before first commit.
  TIO.appendFile logPath "welcome\nmock> "

  inject <- case mode of
    FakeFifo -> pure (respond logPath)
    FakePty -> do
      ch <- newChan
      _ <- forkIO (ptyPump ch logPath)
      pure (writeChan ch)

  replOpenInject cfg inject

-- | Fake process: echo command, result line, prompt.
respond :: FilePath -> Text -> IO ()
respond logPath cmd = do
  -- slight delay so emit ordering matches real processes
  threadDelay 20_000
  TIO.appendFile logPath $
    T.unlines
      [ "received: " <> cmd,
        "echo: " <> cmd,
        "mock> "
      ]

ptyPump :: Chan Text -> FilePath -> IO ()
ptyPump ch logPath = forever $ do
  cmd <- readChan ch
  respond logPath cmd
