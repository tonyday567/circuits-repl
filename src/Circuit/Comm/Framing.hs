{-# LANGUAGE OverloadedStrings #-}

-- | Bus message framing.
--
-- One format: @[timestamp] sender: body@.
module Circuit.Comm.Framing
  ( frameMessage,
    parseMessage,
    parseMessageTs,
    formatNow,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Prelude

-- | Get current time as ISO-8601 text.
formatNow :: IO Text
formatNow = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- | Frame a message with timestamp and sender prefix.
--
-- >>> frameMessage "2026-07-21T14:30:00" "hermes" "status check"
-- "[2026-07-21T14:30:00] hermes: status check"
frameMessage :: Text -> Text -> Text -> Text
frameMessage ts sender body = "[" <> ts <> "] " <> sender <> ": " <> body

-- | Parse a framed message into @(sender, body)@.
--
-- >>> parseMessage "[2026-07-21T14:30:00] hermes: status check"
-- Just ("hermes","status check")
--
-- >>> parseMessage "unframed text"
-- Nothing
parseMessage :: Text -> Maybe (Text, Text)
parseMessage t = fmap (\(_, s, b) -> (s, b)) (parseMessageTs t)

-- | Parse a framed message into @(timestamp, sender, body)@.
--
-- >>> parseMessageTs "[2026-07-21T14:30:00] hermes: status check"
-- Just ("2026-07-21T14:30:00","hermes","status check")
--
-- >>> parseMessageTs "unframed text"
-- Nothing
parseMessageTs :: Text -> Maybe (Text, Text, Text)
parseMessageTs t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      case T.breakOn "] " rest of
        (ts, afterBracket)
          | T.null ts -> Nothing
          | T.null afterBracket -> Nothing
          | otherwise ->
              let after = T.drop 2 afterBracket
               in case T.breakOn ": " after of
                    (sender, body)
                      | T.null sender -> Nothing
                      | T.null body -> Nothing
                      | otherwise ->
                          let body' = T.drop 2 body
                           in if T.null body'
                                then Nothing
                                else Just (ts, sender, body')
