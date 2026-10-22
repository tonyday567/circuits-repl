-- | Repl-loop agents.
--
-- An agent is a repl loop: read (awaiting input, referencing a state),
-- evaluate (change state, compute an output, blocking by custom), print
-- (whenever it feels like it, without reference to the system), loop.
--
-- A 'Repl' is that spec as a factored 'Body': 'commit' is read+evaluate,
-- 'emit' is print, reading the freshly committed channel — the Moore
-- discipline is the post-step observation point. The fused tick
-- ('Circuit.Machine.fuse') is what a lockstep runner iterates; the legs
-- stay visible for scheduling.
--
-- The print leg yields a 'Speech' act, never a list: @[]@ conflates
-- "said nothing" with "still thinking", leaving the listener to diagnose
-- silence from the payload. Here silence is declared by the speaker —
-- 'StandDown' is an act — and consumed by the type's own eliminator.
module Circuit.Repl
  ( -- * speech acts
    Speech (..),

    -- * the repl
    Repl,
    mkRepl,

    -- * runners
    turn,
    turns,
    session,

    -- * seat compositions
    await,
    race,
  )
where

import Circuit.Machine (Body (..))
import Circuit.Tensor (Bias (..))
import Prelude hiding (init)

-- | One print: a speech act. The speaker declares; the listener never
-- diagnoses. 'StandDown' is committed silence — an act, not the absence
-- of acts — so a race can lose cleanly and a log reader can tell "stood
-- down" apart from "still thinking".
data Speech o
  = Said o
  | StandDown
  deriving (Eq, Show, Functor)

-- | A repl-loop agent over pure functions: state @s@ on the channel
-- wire, percept @i@ in, one speech act about @o@ out per turn.
type Repl s i o = Body (,) (,) s s (->) i (Speech o)

-- | Build a repl from a newborn state, an absorb (read+evaluate), and
-- an utter (print) that reads the post-absorb state.
mkRepl :: s -> (s -> i -> s) -> (s -> Speech o) -> Repl s i o
mkRepl s0 absorb utter =
  Body
    { init = \i -> absorb s0 i,
      commit = \(s, i) -> absorb s i,
      emit = \s -> (s, utter s)
    }

-- | One turn: absorb the percept, then print from the fresh state.
turn :: Repl s i o -> (s, i) -> (s, Speech o)
turn (Body _ k e) (s, i) = e (k (s, i))

-- | A multi-turn session over a script, keeping every fresh state.
turns :: Repl s i o -> s -> [i] -> [(s, Speech o)]
turns _ _ [] = []
turns r s (i : is) = let res@(s', _) = turn r (s, i) in res : turns r s' is

-- | The acts of a session, states dropped.
session :: Repl s i o -> s -> [i] -> [Speech o]
session r s is = snd <$> turns r s is

-- | The product of two repl loops: both absorb the same percept, both
-- states advance, both prints land. The composite's payload is the pair
-- of acts — the ordering of two print streams is the consumer's policy,
-- not the composition's, so no bias lives here. The composite stands
-- down exactly when both legs stand down (quiescent when both are);
-- otherwise it says the pair, keeping a leg's stand-down visible in its
-- own slot.
await ::
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i (Speech o, Speech o)
await (Body j1 k1 e1) (Body j2 k2 e2) =
  Body
    { init = \i -> (j1 i, j2 i),
      commit = \((s1, s2), i) -> (k1 (s1, i), k2 (s2, i)),
      emit = \(s1, s2) ->
        let pair = (snd (e1 s1), snd (e2 s2))
         in ( (s1, s2),
              case pair of
                (StandDown, StandDown) -> StandDown
                _ -> Said pair
            )
    }

-- | Choice over two repl loops: both absorb the same percept; one
-- speech act wins; the loser is revoked — its state update dropped,
-- which is the pure corner's simulation of cancellation.
--
-- "First" has no meaning inside a total function, so the both-'Said'
-- case is decided by an explicit 'Bias' — the scheduler as an argument.
-- When exactly one branch speaks, that branch wins under either bias;
-- when neither speaks, the composite stands down and neither advances.
--
-- Revocation costs the stale pair: the channel carries @(stale, fresh)@
-- so the loser's advance can be dropped. The next tick's stale is this
-- tick's resolved pair.
race ::
  Bias ->
  Repl s1 i o ->
  Repl s2 i o ->
  Repl ((s1, s2), (s1, s2)) i o
race bias (Body j1 k1 e1) (Body j2 k2 e2) =
  Body
    { init = \i -> let s = (j1 i, j2 i) in (s, s),
      commit = \((_, (s1, s2)), i) -> ((s1, s2), (k1 (s1, i), k2 (s2, i))),
      emit = \((st1, st2), (f1, f2)) ->
        let a1 = snd (e1 f1)
            a2 = snd (e2 f2)
            (act, pair) = case (a1, a2) of
              (Said x, Said y) -> case bias of
                LeftFirst -> (Said x, (f1, st2))
                RightFirst -> (Said y, (st1, f2))
              (Said x, StandDown) -> (Said x, (f1, st2))
              (StandDown, Said y) -> (Said y, (st1, f2))
              (StandDown, StandDown) -> (StandDown, (st1, st2))
         in ((pair, pair), act)
    }
