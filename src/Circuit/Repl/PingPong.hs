{-# LANGUAGE OverloadedStrings #-}

-- | Concrete ping/pong turn as a Repl backend wired through @causal/IN@.
--
-- This module picks a single operational agent turn — commit @["ping"]@,
-- emit until the body contains @"pong"@ — and shows that the backend's
-- immediate response semantics is exactly the causal fragment of Poly:
--
--   * 'pingPongLens' is a monomial lens
--     @'Mono' ['Text'] ['Text'] -> 'Mono' ['Text'] ['Text']@.
--   * 'causal' 'pingPongLens' is an 'IntMorph' over @(,) (->)@.
--   * 'openPingPongRepl' is a real 'BackendCustom' 'Repl' whose stateless
--     transitions agree with that 'IntMorph'.
--
-- Honest boundary: the "emit until" polling schedule itself is not causal.
-- It lives in the runner circuit ('Circuit.Repl.Turn.turnUntil') that ties the
-- free dual ends.  The @causal/IN@ fragment captures only the backend's
-- immediate response function.
module Circuit.Repl.PingPong
  ( pingPongLens,
    openPingPongRepl,
  )
where

import Circuit.Int (IntMorph (..), causal)
import Circuit.Poly (Mono, Morphism, applyLens, lens)
import Circuit.Repl (Repl, replOpenCustom)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | Stateless ping/pong response as a monomial lens.
--
-- Forward face: given a commit, produce an emit.
-- Backward face: trivial — the backend has no upstream feedback in this turn.
--
-- The direction types are both @[Text]@ because a Repl turn is an endomorphism
-- on the agent wire: commits flow backward, emits flow forward.
pingPongLens :: Morphism (Mono [Text] [Text]) (Mono [Text] [Text])
pingPongLens = lens get (const (const []))
  where
    get cmd
      | any ("ping" `T.isInfixOf`) cmd = ["pong"]
      | otherwise = []

-- | Open a 'BackendCustom' 'Repl' whose commit/emit agrees with
-- @'causal' 'pingPongLens'@.
--
--   * __commit__ stores the input lines.
--   * __emit__ applies the causal lens to the stored input and returns the
--     resulting lines, clearing the store.
--
-- This is the transport boundary: the same pure 'IntMorph' drives the real
-- free dual ends.
openPingPongRepl :: IO Repl
openPingPongRepl = do
  ref <- newIORef []
  let commit ts = writeIORef ref ts
      emit = do
        ts <- readIORef ref
        writeIORef ref []
        let (_feedback, out) = runIntMorph (causal pingPongLens) (ts, [])
        pure out
      close = pure ()
  replOpenCustom commit emit close
