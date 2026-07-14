{-# LANGUAGE OverloadedStrings #-}

-- | Local agent via muster: MusterRepl dual over the live muster bus +
-- Hermes/ollama on the linux farm (one-shot @hermes chat -q@).
--
-- @
--   cabal run muster-local-agent
--   # elsewhere: muster post deep "llama: what is 2+2?"
-- @
--
-- Hermes session store is SQLite (@~\/.hermes\/state.db@), not JSONL.
-- This spike uses @hermes chat -q@ (creates a session row) rather than
-- 'replOpenHermes' (legacy flat JSON). See comments at bottom for path discovery.
module Main where

import Circuit.Comm
import Circuit.Repl
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (getHomeDirectory)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr)
import System.Process (readCreateProcess, shell)

main :: IO ()
main = do
  args <- getArgs
  let name = case args of
        (n : _) -> T.pack n
        [] -> "llama"
  home <- getHomeDirectory
  let root = home </> "mg/logs/muster/general"
      cfg =
        ChannelConfig
          { chStdinPath = root </> "bus.fifo",
            chStdoutPath = root </> "log.md",
            chStderrPath = root </> "err.md",
            chName = name,
            chWorkingDir = root
          }
  hPutStrLn stderr $
    "muster-local-agent: attach as ["
      <> T.unpack name
      <> "] on "
      <> root
  hPutStrLn stderr "emit←muster  commit→hermes→muster (HERMES_LOCAL default; set HERMES_SSH for farm)"
  r <- attachMusterRepl cfg
  -- drain backlog so we only answer new posts
  _ <- replEmit r
  hPutStrLn stderr "ready — post e.g.  deep: hello   or   llama: what is 2+2?"
  hFlush stderr
  forever $ do
    msgs <- replEmit r
    mapM_ (handle r name) msgs
    threadDelay 1_500_000

handle :: Repl -> Text -> Text -> IO ()
handle r name body = do
  let b = T.strip body
  when (shouldAnswer name b) $ do
    let prompt = stripAddress name b
    hPutStrLn stderr $ "  task: " <> T.unpack (T.take 80 prompt)
    hFlush stderr
    reply <- hermesQuery prompt
    let line = T.unwords . T.words $ T.strip reply -- collapse newlines for muster
    if T.null line
      then hPutStrLn stderr "  (empty hermes reply)"
      else do
        hPutStrLn stderr $ "  reply: " <> T.unpack (T.take 80 line)
        replCommit r [line]
        hFlush stderr

-- | Handle if addressed to us, or any @llama:@ / @local:@ prefix.
shouldAnswer :: Text -> Text -> Bool
shouldAnswer name b =
  let n = T.toLower name
      low = T.toLower b
   in (n <> ":") `T.isPrefixOf` low
        || ("@" <> n) `T.isPrefixOf` low

stripAddress :: Text -> Text -> Text
stripAddress name b =
  let low = T.toLower b
      n = T.toLower name
      dropPref p t =
        if T.toLower p `T.isPrefixOf` low
          then T.drop (T.length p) t
          else t
   in T.strip $
        dropPref (n <> ":") $
          dropPref ("@" <> n <> " ") $
            dropPref "llama:" b

-- | One-shot hermes (local by default; optional SSH farm).
--
-- Env:
--   HERMES_LOCAL    if set (any value), run hermes on this machine (default when
--                   HERMES_SSH is unset or empty)
--   HERMES_SSH      if set non-empty, ssh to this host (e.g. tony@100.82.255.106)
--   HERMES_MODEL    default deepseek-v4-pro (mac deep); use llama3.1:8b for ollama farm
--   HERMES_PROVIDER default deepseek; use custom for ollama
--   HERMES_CONTINUE optional session title for --continue (e.g. deep-harness)
--   HERMES_MAX_TURNS default 4
hermesQuery :: Text -> IO Text
hermesQuery prompt = do
  mSsh <- lookupEnv "HERMES_SSH"
  mLocal <- lookupEnv "HERMES_LOCAL"
  model <- maybe "deepseek-v4-pro" id <$> lookupEnv "HERMES_MODEL"
  provider <- maybe "deepseek" id <$> lookupEnv "HERMES_PROVIDER"
  cont <- lookupEnv "HERMES_CONTINUE"
  maxTurns <- maybe "4" id <$> lookupEnv "HERMES_MAX_TURNS"
  let q = shellQuote (T.unpack prompt)
      contFlag = maybe "" (\t -> " --continue " <> shellQuote t) cont
      hermesCmd =
        "export PATH=\"$HOME/.local/bin:$PATH\"; "
          <> "hermes chat -q "
          <> q
          <> " -m "
          <> shellQuote model
          <> " --provider "
          <> shellQuote provider
          <> contFlag
          <> " -Q --max-turns "
          <> maxTurns
          <> " 2>/dev/null"
      useSsh = case (mLocal, mSsh) of
        (Just _, _) -> False
        (_, Just h) | not (null h) -> True
        _ -> False -- default: local hermes (mac deep)
      cmd
        | useSsh =
            "ssh -o BatchMode=yes -o ConnectTimeout=30 "
              <> fromMaybe "" mSsh
              <> " "
              <> shellQuote hermesCmd
        | otherwise = hermesCmd
  out <- readCreateProcess (shell cmd) ""
  pure $ T.pack $ lastNonEmpty (lines out)

lastNonEmpty :: [String] -> String
lastNonEmpty xs =
  case filter (not . all isSpace) xs of
    [] -> ""
    ys -> last ys

shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

{- Session path discovery (BackendHermes backlog)

  Hermes v0.18 stores sessions in SQLite, not flat JSONL:

    ~/.hermes/state.db
      sessions(id, model, title, message_count, ...)
      messages(id, session_id, role, content, timestamp, ...)

  How to get a session id / path for attach:

  1. List:   hermes sessions list
             → ID column e.g. 20260715_063039_c2ad59

  2. Export (jsonl dump, not the live store):
             hermes sessions export --session-id ID --format jsonl out.jsonl

  3. Live dual over SQLite (cleanest for commit/emit):
             read/write messages table WHERE session_id = ?
             emit: SELECT content FROM messages
                   WHERE session_id=? AND role='assistant' AND id > last_id
             commit: INSERT INTO messages (session_id, role, content, timestamp)
             Needs sqlite in process (direct-sqlite / sqlite-simple); linux may
             lack sqlite3 CLI but the DB file is readable.

  4. One-shot (this spike): hermes chat -q ... prints session_id: on stderr/stdout
     and creates a new session row — no attach path needed.

  Legacy: ~/.hermes/sessions/session_*.json still exists on some hosts (old
  format). replOpenHermes targets those files; prefer state.db going forward.
-}
