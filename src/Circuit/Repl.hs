-- | Repl-loop agents.
--
-- An agent is a repl loop: read, evaluate, print, loop. As a 'Body',
-- commit is read+evaluate and emit is print. Read and evaluate are one
-- leg because they share a domain: "read" names the arrival of the
-- @(state, input)@ pair, not a computation — there is no intermediate
-- value to separate. Print is a second leg because its input, the
-- fresh state, exists only after evaluate completes, and because
-- operationally a print can be late or never happen.
--
-- The target grammar is @(,)@, not 'Const': the tick's output carries
-- the fresh state, so a driver can pause and resume mid-session. The
-- Moore corner (target 'Const', state dropped from the output) is one
-- bridge away ('replMoore'), and with it 'Circuit.Machine.scanProcess'.
module Circuit.Repl
  ( -- * one turn's output
    Reply (..),

    -- * the repl
    Repl,
    mkRepl,
    replMoore,
    mooreRepl,

    -- * runners
    step,
    runRepl,
    session,

    -- * compositions
    await,
    race,
  )
where

import Circuit.Machine (Body (..), Moore)
import Circuit.Tensor (Bias (..), Const (..), unConst)

-- | The output of one turn: a payload, or done.
--
-- 'Done' covers "evaluated, nothing to say". It does not cover death:
-- a crashed repl returns nothing at all, and no pure type can name
-- that. Death-as-content is the operational lane's problem, not this
-- one's.
data Reply o
  = Said o
  | Done
  deriving (Eq, Show, Functor)

-- | A repl-loop agent over pure functions: state @s@, input @i@, one
-- 'Reply' about @o@ per turn. @o@ is unconstrained — a repl can print
-- a list if a list is what it has to say.
type Repl s i o = Body (,) (,) s s (->) i (Reply o)

-- | Build a repl from a newborn state, a commit function
-- (read+evaluate), and an utter function (print) over the fresh state.
-- The construction is 'moore''s recipe, kept at the visible channel:
-- 'MooreT' seals the channel existentially, and a driver that pauses
-- and resumes mid-session needs to see it.
mkRepl :: s -> (s -> i -> s) -> (s -> Reply o) -> Repl s i o
mkRepl s0 k u = mooreRepl (Body (k s0) (uncurry k) (Const . u))

-- | The emit bridge to the Moore corner: drop the fresh state from
-- the tick's output. With it comes 'Circuit.Machine.scanProcess'.
replMoore :: Repl s i o -> Moore (,) s (->) i (Reply o)
replMoore (Body j k e) = Body j k (Const . snd . e)

-- | The emit bridge back: re-attach the fresh state to the tick's
-- output. Inverse to 'replMoore' on the nose — the @(,)@ emit is
-- @\s -> (s, utter s)@, so dropping and re-attaching loses nothing.
mooreRepl :: Moore (,) s (->) i (Reply o) -> Repl s i o
mooreRepl (Body j k e) = Body j k (\s -> (s, unConst (e s)))

-- | The iterating tick. 'init' is out of scope here — it seeds tick
-- one; a mid-session step works on a state that already exists. (Same
-- scope as 'Circuit.Machine.fuse', which drops 'init' for the same
-- reason.)
step :: Repl s i o -> (s, i) -> (s, Reply o)
step (Body _ k e) (s, i) = e (k (s, i))

-- | A full session from scratch: 'init' seeds the channel from the
-- first input, 'commit' iterates. States are kept. The seed discipline
-- is 'Circuit.Machine.scanProcess''s, and the agreement is oracled in
-- @Axioma.Repl@.
runRepl :: Repl s i o -> [i] -> [(s, Reply o)]
runRepl r@(Body j _ _) = \case
  [] -> []
  (i : is) -> firstStep : go (fst firstStep) is
    where
      firstStep = emit r (j i)
      go _ [] = []
      go s (i' : is') = let res = step r (s, i') in res : go (fst res) is'

-- | The outputs of a session, states dropped.
session :: Repl s i o -> [i] -> [Reply o]
session r is = snd <$> runRepl r is

-- | The product: both legs receive the input, both states advance, and
-- the composite's payload is the pair of leg outputs. The composite is
-- 'Done' exactly when both legs are.
await ::
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i (Reply o, Reply o)
await (Body j1 k1 e1) (Body j2 k2 e2) =
  Body
    { init = \i -> (j1 i, j2 i),
      commit = \((s1, s2), i) -> (k1 (s1, i), k2 (s2, i)),
      emit = \(s1, s2) ->
        let pair = (snd (e1 s1), snd (e2 s2))
         in ( (s1, s2),
              case pair of
                (Done, Done) -> Done
                _ -> Said pair
            )
    }

-- | Choice: both legs receive the input and both states advance — the
-- pure corner has nothing to cancel, so both turns land — and the
-- composite's output is one leg's. 'Done' loses to any 'Said'; two
-- 'Said's are decided by the explicit 'Bias'; two 'Done's are 'Done'.
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
race bias (Body j1 k1 e1) (Body j2 k2 e2) =
  Body
    { init = \i -> (j1 i, j2 i),
      commit = \((s1, s2), i) -> (k1 (s1, i), k2 (s2, i)),
      emit = \(s1, s2) ->
        ( (s1, s2),
          case (snd (e1 s1), snd (e2 s2)) of
            (Said x, Said y) -> case bias of
              LeftFirst -> Said x
              RightFirst -> Said y
            (Said x, Done) -> Said x
            (Done, Said y) -> Said y
            (Done, Done) -> Done
        )
    }
