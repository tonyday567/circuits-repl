{-# LANGUAGE OverloadedStrings #-}

-- | BackendCustom 'Repl' whose turn is a pure 'Int' morphism.
--
-- The free dual ends ('replCommit' / 'replEmit') are the transport boundary;
-- 'causal' 'agentRoster' is the category boundary. This module wires them so a
-- real 'replOpenCustom' turn is forced to agree with the 'Int' morph — the
-- honesty check that the discrete dual spike is not only algebraic.
--
-- This module also holds the agent-lifecycle vocabulary that was formerly in
-- @Circuit.Int@. Keeping bus verbs out of the category core keeps the core
-- pure algebra and lets the operational layer own its own theory.
module Circuit.Repl.Agent
  ( AgentVerb (..),
    verbDelta,
    agentRoster,
    parseAgentVerb,
    openAgentRosterRepl,
  )
where

import Circuit.Int
  ( IntMorph (runIntMorph),
    causal,
    comp,
  )
import Circuit.Poly (Mono, Morphism (..), applyLens, lens)
import Circuit.Repl (Repl, replOpenCustom)
import Circuit.Trace (Trace (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | Lifecycle verbs a bus peer can commit (join / claim / leave).
--
-- Encoded as 'Int' deltas on the monomial direction wire so the lens stays in
-- the @'Mono' 'Int' 'Int'@ fragment ('causal' / 'Netlist' friendly):
-- 'Join' = @+1@, 'Ack' = @0@, 'Quit' = @-1@.
data AgentVerb = Join | Ack | Quit
  deriving (Eq, Show)

-- | Encode a lifecycle verb as a roster delta.
verbDelta :: AgentVerb -> Int
verbDelta = \case
  Join -> 1
  Ack -> 0
  Quit -> -1

-- | Roster count as a monomial lens: forward face is the observed count
-- (emit dual), backward face is a signed delta on that count (commit dual).
agentRoster :: Morphism (Mono Int Int) (Mono Int Int)
agentRoster = lens (\n -> n) (\n d -> max 0 (n + d))

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
