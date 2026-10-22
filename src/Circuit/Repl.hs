-- | Repl-loop agents.
--
-- An agent is a repl loop: read, evaluate, print, loop. As an 'FBody':
-- begin is the repl's birth (a fixed newborn state — a seat's birth is
-- prior to any input), step is read+evaluate folded into the channel,
-- extract is print, reading the fresh channel.
--
-- Read and evaluate are one leg because they share a domain: "read"
-- names the arrival of the @(state, input)@ pair, not a computation —
-- there is no intermediate value to separate. Print is its own leg
-- because its input, the fresh channel, exists only after evaluate
-- completes, and because operationally a print can be late or never
-- happen.
--
-- The output of a turn is @Maybe o@: a payload, or nothing. Nothing
-- covers "evaluated, nothing to say"; it does not cover death — a
-- crashed repl returns no answer at all, and no pure type names that.
module Circuit.Repl
  ( -- * the repl
    Repl,

    -- * runners
    turn,
    runRepl,
    session,

    -- * compositions
    await,
    race,
  )
where

import Circuit.Machine (FBody (..))
import Circuit.Tensor (Bias (..))

-- | A repl-loop agent over pure functions: a fixed newborn state,
-- state @s@ on the channel wire, input @i@, at most one payload about
-- @o@ per turn. Construct with the 'FBody' record directly.
type Repl s i o = FBody (,) s (->) i (Maybe o)

-- | One turn, fused: fold the input in, print from the fresh channel.
-- 'begin' is not part of the tick — a mid-session turn works on a
-- state that already exists.
--
-- This is 'fuseFBody''s tick inlined: @copy .> tensor id e@ duplicates
-- the fresh channel for the extract leg, which at @(->)@ is
-- @\(s,i) -> let s' = k (s,i) in (s', e s')@. 'fuseFBody' itself asks
-- 'Copy' of the channel, and 'Copy' at @(->)@ is a closed per-type
-- instance set — it cannot cover an abstract @s@.
turn :: Repl s i o -> (s, i) -> (s, Maybe o)
turn (FBody _ k e) (s, i) = let s' = k (s, i) in (s', e s')

-- | A full session from birth: 'begin' seeds the channel, the fused
-- step iterates. States are kept.
runRepl :: Repl s i o -> [i] -> [(s, Maybe o)]
runRepl r@(FBody b _ _) = go (b ())
  where
    go _ [] = []
    go s (i : is) = let res@(s', _) = turn r (s, i) in res : go s' is

-- | The outputs of a session, states dropped.
session :: Repl s i o -> [i] -> [Maybe o]
session r is = snd <$> runRepl r is

-- | The product: both legs receive the input, both states advance, and
-- the composite's payload is the pair of leg outputs. The composite
-- prints nothing exactly when both legs do.
await ::
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i (Maybe o, Maybe o)
await (FBody b1 k1 e1) (FBody b2 k2 e2) =
  FBody
    (\() -> (b1 (), b2 ()))
    (\((s1, s2), i) -> (k1 (s1, i), k2 (s2, i)))
    ( \(s1, s2) ->
        case (e1 s1, e2 s2) of
          (Nothing, Nothing) -> Nothing
          pair -> Just pair
    )

-- | Choice: both legs receive the input and both states advance — the
-- pure corner has nothing to cancel, so both turns land — and the
-- composite's output is one leg's. 'Nothing' loses to any payload; two
-- payloads are decided by the explicit 'Bias'; two 'Nothing's are
-- 'Nothing'.
--
-- What is revoked is the loser's print, not its turn. The stronger
-- semantics — the loser's state update rolled back, STM @orElse@-style
-- — needs the stale state to live somewhere, which doubles the
-- channel; deferred (loom/repl.md §idk).
race ::
  Bias ->
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i o
race bias (FBody b1 k1 e1) (FBody b2 k2 e2) =
  FBody
    (\() -> (b1 (), b2 ()))
    (\((s1, s2), i) -> (k1 (s1, i), k2 (s2, i)))
    ( \(s1, s2) ->
        case (e1 s1, e2 s2) of
          (Just x, Just y) -> case bias of
            LeftFirst -> Just x
            RightFirst -> Just y
          (Just x, Nothing) -> Just x
          (Nothing, Just y) -> Just y
          (Nothing, Nothing) -> Nothing
    )
