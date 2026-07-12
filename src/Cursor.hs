{-# LANGUAGE OverloadedStrings #-}

-- | A position in an append-only log of lines.
--
-- Two storage backends, one type:
--
--   * 'newMem'  — 'IORef' (ephemeral; dies with the process)
--   * 'newFile' — file holding a decimal line count (survives restart)
--
-- Both answer the same question: /what is new since I last asked?/
--
-- This module has no dependency on muster or circuits-io. Either consumer
-- can hold a 'Cursor' and call 'pollLines' / 'pollFile' without caring
-- where the position lives.
--
-- === Line index convention
--
-- Positions are zero-based counts of complete lines (same as
-- @length (T.lines content)@ when every record ends in @\\n@, and same as
-- @wc -l@). 'pollLines' advances to @length xs@ after returning the suffix.
module Cursor
  ( Cursor,
    newMem,
    newFile,
    get,
    set,
    pollLines,
    pollFile,
    seekEnd,
    seekEndFile,
  )
where

import Data.Char (isSpace)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Prelude

-- | Opaque read position in a line-oriented log.
--
-- Construct with 'newMem' or 'newFile'. Read/write with 'get'/'set'.
-- Advance with 'pollLines' (in-memory log) or 'pollFile' (path to log).
data Cursor = Cursor
  { cursorGet :: IO Int,
    cursorSet :: Int -> IO ()
  }

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Cursor
-- >>> import Data.Text (Text)

-- | In-memory cursor (IORef). Position dies with the process.
--
-- >>> c <- newMem 0
-- >>> get c
-- 0
-- >>> pollLines c ["a", "b" :: Text]
-- ["a","b"]
-- >>> pollLines c ["a", "b", "c"]
-- ["c"]
-- >>> get c
-- 3
newMem :: Int -> IO Cursor
newMem n0 = do
  ref <- newIORef n0
  pure
    Cursor
      { cursorGet = readIORef ref,
        cursorSet = writeIORef ref
      }

-- | File-backed cursor. Contents are a decimal integer plus newline
-- (muster-compatible: @show n <> \"\\n\"@). Missing file reads as 0;
-- first 'set' creates it.
newFile :: FilePath -> IO Cursor
newFile path =
  pure
    Cursor
      { cursorGet = readFilePos path,
        cursorSet = writeFilePos path
      }

-- | Current line position.
get :: Cursor -> IO Int
get = cursorGet

-- | Set line position (does not touch the log).
set :: Cursor -> Int -> IO ()
set = cursorSet

-- | Given the full current log as lines, return those after the cursor
-- and advance the cursor to @length xs@.
--
-- Idempotent on a frozen log: a second call with the same @xs@ yields @[]@.
--
-- >>> c <- newMem 0
-- >>> pollLines c ["x" :: Text]
-- ["x"]
-- >>> pollLines c ["x"]
-- []
pollLines :: Cursor -> [Text] -> IO [Text]
pollLines c xs = do
  pos <- cursorGet c
  let total = length xs
      news = drop pos xs
  cursorSet c total
  pure news

-- | Read a log file as lines ('T.lines'), then 'pollLines'.
--
-- Missing file → empty log. Empty file → empty log.
--
-- Partial last lines (no trailing newline) are kept as a final element of
-- 'T.lines' only when content is non-empty and does not end in @\\n@ —
-- actually 'T.lines' drops a trailing empty segment, so a file ending
-- without @\\n@ still yields its last partial line. Prompt-style partial
-- lines are therefore visible to the cursor; completeness is the caller's
-- concern (prompt detection lives above this layer).
pollFile :: Cursor -> FilePath -> IO [Text]
pollFile c path = do
  ls <- readLogLines path
  pollLines c ls

-- | Move the cursor to the end of the given lines without returning them.
-- Attach pattern: start at "now" so the next poll only sees future output.
--
-- >>> c <- newMem 0
-- >>> seekEnd c ["old" :: Text, "history"]
-- >>> pollLines c ["old", "history", "new"]
-- ["new"]
seekEnd :: Cursor -> [Text] -> IO ()
seekEnd c xs = cursorSet c (length xs)

-- | 'seekEnd' for a log path.
seekEndFile :: Cursor -> FilePath -> IO ()
seekEndFile c path = do
  ls <- readLogLines path
  seekEnd c ls

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

readFilePos :: FilePath -> IO Int
readFilePos path = do
  exists <- doesFileExist path
  if not exists
    then pure 0
    else do
      -- Strict read: lazy 'readFile' holds the handle and locks subsequent writes.
      raw <- T.unpack <$> TIO.readFile path
      pure $ fromMaybe 0 $ readMaybe (filter (not . isSpace) raw)

writeFilePos :: FilePath -> Int -> IO ()
writeFilePos path n = TIO.writeFile path (T.pack (show n <> "\n"))

-- | Line split matching muster @T.lines@ / @wc -l@ for newline-terminated
-- records. Empty file → @[]@.
readLogLines :: FilePath -> IO [Text]
readLogLines path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      content <- TIO.readFile path
      pure $ if T.null content then [] else T.lines content
