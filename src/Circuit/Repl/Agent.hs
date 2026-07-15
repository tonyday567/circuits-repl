{-# LANGUAGE OverloadedStrings #-}

-- | BackendCustom Repl whose turn is the pure Int morphism from 'Circuit.Int'.
--
-- The free dual ends ('replCommit' / 'replEmit') are the transport boundary;
-- 'causal' 'agentRoster' is the category boundary. This module wires them so a
-- real 'replOpenCustom' turn is forced to agree with the Int morph — the
-- honesty check that the discrete dual spike is not only algebraic.
module Circuit.Repl.Agent
  ( openAgentRosterRepl,
    parseAgentVerb,
  )
where

import Circuit.Int
  ( AgentVerb (..),
    IntMorph (runIntMorph),
    agentRoster,
    causal,
    verbDelta,
  )
import Circuit.Repl (Repl, replOpenCustom)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | Parse a bus-style lifecycle verb from text.
parseAgentVerb :: Text -> Maybe AgentVerb
parseAgentVerb t = case T.toLower (T.strip t) of
  "join" -> Just Join
  "ack" -> Just Ack
  "quit" -> Just Quit
  _ -> Nothing

-- | Open a 'BackendCustom' 'Repl' whose state transitions are exactly
-- @'runIntMorph' ('causal' 'agentRoster')@.
--
-- * __commit__ applies zero or more lifecycle verbs (@join@ / @ack@ / @quit@).
-- * __emit__ returns the current roster count as a single decimal line.
--
-- Unknown commit lines are ignored (no state change).
openAgentRosterRepl :: IO Repl
openAgentRosterRepl = do
  ref <- newIORef (0 :: Int)
  let commit ts = mapM_ step ts
        where
          step t = case parseAgentVerb t of
            Nothing -> pure ()
            Just v -> do
              n <- readIORef ref
              let (n', _) = runIntMorph (causal agentRoster) (n, verbDelta v)
              writeIORef ref n'
      emit = do
        n <- readIORef ref
        pure [T.pack (show n)]
      close = pure ()
  replOpenCustom commit emit close
