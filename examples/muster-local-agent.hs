{-# LANGUAGE OverloadedStrings #-}

-- | Muster peer agent: MusterRepl dual + full hermes (tools, skills, high turn budget).
--
-- Spec for deep: orchestrator/verifier with terminal, files, git, mg surface.
-- Not a one-line chatbot — hermes runs a real tool loop, then posts results.
--
-- @
--   HERMES_CONTINUE=deep-harness HERMES_LOCAL=1 \\
--     HERMES_SKILLS=mg,verification-sweep \\
--     HERMES_MAX_TURNS=90 \\
--     cabal run muster-local-agent -- deep
-- @
module Main where

import Circuit.Comm
import Circuit.Repl
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when, forM_)
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
        [] -> "deep"
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
    "muster-local-agent: ["
      <> T.unpack name
      <> "] dual on "
      <> root
      <> " + full hermes tool loop"
  r <- attachMusterRepl cfg
  _ <- replEmit r -- drain backlog
  hPutStrLn stderr "ready — address with deep: … / @deep …"
  hFlush stderr
  forever $ do
    msgs <- replEmit r
    mapM_ (handle r name home) msgs
    threadDelay 1_500_000

handle :: Repl -> Text -> FilePath -> Text -> IO ()
handle r name home body = do
  let b = T.strip body
  when (shouldAnswer name b) $ do
    let task = stripAddress name b
        prompt = rolePrompt name home task
    hPutStrLn stderr $ "  task: " <> T.unpack (T.take 100 task)
    hFlush stderr
    reply <- hermesQuery prompt
    let lines' = take 12 $ filter (not . T.null) $ map T.strip $ T.lines reply
    if null lines'
      then hPutStrLn stderr "  (empty hermes reply)"
      else do
        forM_ lines' $ \line -> do
          hPutStrLn stderr $ "  post: " <> T.unpack (T.take 100 line)
          replCommit r [line]
        hFlush stderr

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
          dropPref ("@" <> n <> " ") b

-- | Frame task so hermes acts as deep-at-spec (tools + mg surface).
rolePrompt :: Text -> FilePath -> Text -> Text
rolePrompt name home task =
  T.unlines
    [ "You are **" <> name <> "** on the mg muster bus (Mac). Full agent mode.",
      "",
      "Role: multi-agent orchestrator / verifier. You drive work via muster posts;",
      "you do not wait for the runner to spoon-feed the next step.",
      "",
      "Environment:",
      "- HOME / mg surface: " <> T.pack (home </> "mg"),
      "- Haskell repos: " <> T.pack (home </> "haskell"),
      "- Muster: MUSTER_NAME=" <> name <> " muster read|post|watch (binary on PATH)",
      "- Board: " <> T.pack (home </> "mg/loom/board.md"),
      "- Skills: buff/deep-orchestrator, buff/verifier-workflow under mg",
      "",
      "Every turn:",
      "1. Use tools (terminal, file, code) as needed — cabal, git, read cards.",
      "2. Prefer terse muster-ready lines (one idea per line, no markdown fences).",
      "3. If verifying: report package + status + evidence (commit/docspec counts).",
      "",
      "Task from muster:",
      task
    ]

-- | Full hermes tool loop (local default; optional SSH farm).
--
-- Env:
--   HERMES_LOCAL / HERMES_SSH  — local unless HERMES_SSH set
--   HERMES_MODEL               — default deepseek-v4-pro
--   HERMES_PROVIDER            — default deepseek
--   HERMES_CONTINUE            — session title (deep-harness)
--   HERMES_MAX_TURNS           — default 90 (orchestration budget)
--   HERMES_SKILLS              — comma list, default mg,verification-sweep
--   HERMES_YOLO                — default on (unattended tool approval)
hermesQuery :: Text -> IO Text
hermesQuery prompt = do
  mSsh <- lookupEnv "HERMES_SSH"
  mLocal <- lookupEnv "HERMES_LOCAL"
  model <- maybe "deepseek-v4-pro" id <$> lookupEnv "HERMES_MODEL"
  provider <- maybe "deepseek" id <$> lookupEnv "HERMES_PROVIDER"
  cont <- lookupEnv "HERMES_CONTINUE"
  maxTurns <- maybe "90" id <$> lookupEnv "HERMES_MAX_TURNS"
  skills <- maybe "mg,verification-sweep" id <$> lookupEnv "HERMES_SKILLS"
  yolo <- maybe True (const True) <$> lookupEnv "HERMES_YOLO"
  let q = shellQuote (T.unpack prompt)
      contFlag = maybe "" (\t -> " --continue " <> shellQuote t) cont
      skillsFlag =
        if null skills
          then ""
          else " --skills " <> shellQuote skills
      yoloFlag = if yolo then " --yolo" else ""
      hermesCmd =
        "export PATH=\"$HOME/.local/bin:$PATH\"; "
          <> "hermes chat -q "
          <> q
          <> " -m "
          <> shellQuote model
          <> " --provider "
          <> shellQuote provider
          <> contFlag
          <> skillsFlag
          <> yoloFlag
          <> " -Q --max-turns "
          <> maxTurns
      useSsh = case (mLocal, mSsh) of
        (Just _, _) -> False
        (_, Just h) | not (null h) -> True
        _ -> False
      cmd
        | useSsh =
            "ssh -o BatchMode=yes -o ConnectTimeout=60 "
              <> fromMaybe "" mSsh
              <> " "
              <> shellQuote hermesCmd
        | otherwise = hermesCmd
  out <- readCreateProcess (shell cmd) ""
  pure $ cleanHermesOut (T.pack out)

-- | Drop banners / session_id noise; keep substantive answer lines.
cleanHermesOut :: Text -> Text
cleanHermesOut t =
  T.unlines $
    filter keep $
      map T.strip $
        T.lines t
  where
    keep l
      | T.null l = False
      | "session_id:" `T.isPrefixOf` l = False
      | "Warning:" `T.isPrefixOf` l = False
      | "Resumed session" `T.isInfixOf` l = False
      | "Reached maximum" `T.isInfixOf` l = False
      | "Requesting summary" `T.isInfixOf` l = False
      | otherwise = True

shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]
