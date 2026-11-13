{-# LANGUAGE OverloadedStrings #-}

-- | Run one persistent cabal REPL behind a free-agent channel.
module Main (main) where

import Circuit.Repl.Process
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import System.Exit (ExitCode (..), exitWith)
import Prelude

data Options = Options
  { optionRoot :: FilePath,
    optionProject :: FilePath,
    optionName :: Text,
    optionChannel :: Text,
    optionTarget :: Maybe String,
    optionImports :: [Text]
  }

options :: Parser Options
options =
  Options
    <$> strOption (long "root" <> metavar "DIR" <> help "existing scratch free-agent bus root")
    <*> strOption (long "project" <> metavar "DIR" <> help "real Haskell project passed to cabal repl")
    <*> strOption (long "name" <> metavar "NAME" <> value "cabal-repl" <> showDefault <> help "owner identity on the channel")
    <*> strOption (long "channel" <> metavar "NAME" <> value "repl" <> showDefault <> help "shared command and result channel")
    <*> optional (strOption (long "target" <> metavar "TARGET" <> help "optional cabal repl target"))
    <*> many (T.pack <$> strOption (long "import" <> metavar "MODULE" <> help "module to import once at startup"))

parserInfo :: ParserInfo Options
parserInfo =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Serialise channel commands into one persistent real cabal REPL"
        <> header "repl-channel — persistent cabal-repl bus channel"
    )

main :: IO ()
main = do
  opts <- execParser parserInfo
  exit <-
    runChannel
      ChannelConfig
        { channelRoot = optionRoot opts,
          channelProject = optionProject opts,
          channelName = optionName opts,
          channelAddress = optionChannel opts,
          channelTarget = optionTarget opts,
          channelImports = optionImports opts
        }
  putStrLn $
    "REPL_EXIT pid="
      ++ maybe "unknown" show (channelExitPid exit)
      ++ " code="
      ++ show (channelExitCode exit)
      ++ " log="
      ++ channelExitRoot exit
      ++ "/log.jsonl"
  case channelExitCode exit of
    ExitSuccess -> pure ()
    code -> exitWith code
