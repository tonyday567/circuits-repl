-- | Repl-loop agents.
--
-- An agent is a repl loop: read, evaluate, print, loop. The loop is a
-- 'Moore' over pure functions. 'commit' is read-and-evaluate, 'emit' is
-- print reading the fresh channel, and 'init' is the first turn, so the
-- newborn state is seeded through the same evaluate leg as every later
-- turn.
--
-- Read and evaluate are one leg because they share a domain: "read"
-- names the arrival of the @(state, input)@ pair, not a computation —
-- there is no intermediate value to separate. Print is its own leg
-- because its input, the fresh channel, exists only after evaluate
-- completes, and because operationally a print can be late or never
-- happen.
--
-- A seat's birth is prior to any input, and a 'Moore' seeds from its first
-- input. 'fixedBirth' is where that difference is paid: it feeds the fixed
-- newborn state through the evaluate leg, so the first input sees the
-- state the birth produced and no input-free seed station is needed. An
-- interface transform that must also birth without an input takes the
-- birth as an explicit argument.
--
-- The output of a turn is @Maybe o@: a payload, or nothing. Nothing
-- covers "evaluated, nothing to say"; it does not cover death — a
-- crashed repl returns no answer at all, and no pure type names that.
module Circuit.Repl
  ( -- * the repl
    Repl,
    fixedBirth,

    -- * runners
    turn,
    runRepl,
    session,

    -- * compositions
    await,
    race,
  )
where

import Circuit.Machine (Moore (..))
import Circuit.Tensor (Bias (..))

-- | A repl-loop agent over pure functions: state @s@ on the channel wire,
-- input @i@, at most one payload about @o@ per turn.
type Repl s i o = Moore (,) s (->) i (Maybe o)

-- | A seat from a fixed newborn state. The birth runs through the evaluate
-- leg, so the first turn is the newborn state's first turn.
fixedBirth :: (() -> s) -> ((s, i) -> s) -> (s -> Maybe o) -> Repl s i o
fixedBirth b k e = Moore (\i -> k (b (), i)) k e

-- | One turn: evaluate the input into the fresh channel, print from the
-- fresh channel.
turn :: Repl s i o -> (s, i) -> (s, Maybe o)
turn (Moore _ k e) (s, i) = let s' = k (s, i) in (s', e s')

-- | A full session from the first input: 'init' seeds the channel, later
-- inputs use 'commit'. States are kept. No input, no turn.
runRepl :: Repl s i o -> [i] -> [(s, Maybe o)]
runRepl (Moore j k e) = go
  where
    go [] = []
    go (i : is) = let s' = j i in (s', e s') : rest s' is
    rest _ [] = []
    rest s (i : is) = let s' = k (s, i) in (s', e s') : rest s' is

-- | The outputs of a session, states dropped.
session :: Repl s i o -> [i] -> [Maybe o]
session r is = snd <$> runRepl r is

-- | The product: both legs receive the input, both states advance, and
-- the composite's payload is the pair of leg outputs. The composite
-- prints nothing exactly when both legs do. Both first turns occur,
-- because 'init' composes leg-wise.
await ::
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i (Maybe o, Maybe o)
await (Moore j1 k1 e1) (Moore j2 k2 e2) =
  Moore
    (\i -> (j1 i, j2 i))
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
race bias (Moore j1 k1 e1) (Moore j2 k2 e2) =
  Moore
    (\i -> (j1 i, j2 i))
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
