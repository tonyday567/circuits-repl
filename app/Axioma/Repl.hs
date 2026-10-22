-- | Repl oracles: the repl loop, structural silence, and the race and
-- await seat compositions.
--
-- Every assertion names its counterexample: the counter-seats below are
-- built so that a stateless absorb, a pre-commit emit, a selection
-- instead of a product, a bias leak into single-Said races, or a
-- missing revocation would each turn a specific check red.
module Axioma.Repl
  ( replTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Repl
import Circuit.Tensor (Bias (..))
import Control.Monad (when)
import Prelude hiding (init)

-- | A counting echo repl: the absorb counts and stores the input; the
-- utter cites both. A stateless absorb yields 1 forever; a pre-commit
-- emit cites the previous input.
countRepl :: Repl (Int, Maybe Int) Int (Int, Int)
countRepl =
  mkRepl
    (0, Nothing)
    (\(n, _) i -> (n + 1, Just i))
    (\(n, mi) -> maybe StandDown (Said . (n,)) mi)

-- | Always stands down; the absorb still counts.
muteRepl :: Repl Int Int (Int, Int)
muteRepl = mkRepl 0 (\s _ -> s + 1) (const StandDown)

-- | Always speaks; says a payload distinct from 'countRepl's.
chatRepl :: Repl Int Int (Int, Int)
chatRepl = mkRepl 0 (\s _ -> s + 1) (\s -> Said (s, 0))

replTopic :: Verbosity -> IO [Bool]
replTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Repl oracles"
  sequence
    [ -- the loop: state is continuous across turns. Counterexample: a
      -- stateless absorb yields (1,_) on every turn.
      checkV verbosity "the loop threads state across turns" $
        turns countRepl (0, Nothing) [10, 20, 30]
          == [ ((1, Just 10), Said (1, 10)),
               ((2, Just 20), Said (2, 20)),
               ((3, Just 30), Said (3, 30))
             ],
      -- print reads the fresh channel. Counterexample: a pre-commit
      -- emit cites the previous input (Nothing on turn one).
      checkV verbosity "print reads the post-absorb state" $
        session countRepl (0, Nothing) [10, 20]
          == [Said (1, 10), Said (2, 20)],
      -- silence is an act: the mute repl's turn yields StandDown, and
      -- the absorb still landed. Counterexample: a list-shaped print
      -- leg yields [] and the two readings are indistinguishable.
      checkV verbosity "stand-down is declared, and the absorb still lands" $
        turn muteRepl (0, 7) == (1, StandDown),
      -- await: both legs absorb, both states advance, both acts land.
      -- Counterexample: a selection (race-shaped) drops one act and
      -- leaves one state stale.
      checkV verbosity "await lands both acts and advances both states" $
        turn (await countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Said (Said (1, 5), Said (1, 0))),
      -- await across a script: neither leg falls behind.
      checkV verbosity "await advances both legs through a session" $
        session (await countRepl chatRepl) ((0, Nothing), 0) [5, 6]
          == [ Said (Said (1, 5), Said (1, 0)),
               Said (Said (2, 6), Said (2, 0))
             ],
      -- await keeps a leg's stand-down visible in its own slot.
      -- Counterexample: a flattening await yields Said (1,5) and the
      -- mute leg's act is gone.
      checkV verbosity "await reports a standing-down leg in its own slot" $
        turn (await countRepl muteRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Said (Said (1, 5), StandDown)),
      -- the composite stands down exactly when both legs do.
      -- Counterexample: an always-Said product claims speech on a quiet
      -- turn; an any-StandDown product goes quiet while a leg speaks.
      checkV verbosity "await stands down exactly when both legs do" $
        snd (turn (await muteRepl muteRepl) ((0, 0), 5)) == StandDown,
      -- race, one speaker: bias-independent, loser revoked.
      -- Counterexamples: a bias leak flips the act under the other
      -- bias; missing revocation leaves the loser's counter at 1.
      checkV verbosity "race: a lone speaker wins under either bias, loser stays stale" $
        let leftWins bias =
              turn (race bias countRepl muteRepl) ((((0, Nothing), 0), ((0, Nothing), 0)), 5)
                == ((((1, Just 5), 0), ((1, Just 5), 0)), Said (1, 5))
            rightWins bias =
              turn (race bias muteRepl countRepl) (((0, (0, Nothing)), (0, (0, Nothing))), 5)
                == (((0, (1, Just 5)), (0, (1, Just 5))), Said (1, 5))
         in all leftWins [LeftFirst, RightFirst]
              && all rightWins [LeftFirst, RightFirst],
      -- race, both speak: the explicit bias is the whole decision.
      -- Counterexample: a hidden left preference turns the RightFirst
      -- clause red; swapped payload order turns the LeftFirst clause red.
      checkV verbosity "race: two speakers are decided by the named bias alone" $
        turn (race LeftFirst countRepl chatRepl) ((((0, Nothing), 0), ((0, Nothing), 0)), 5)
          == ((((1, Just 5), 0), ((1, Just 5), 0)), Said (1, 5))
          && turn (race RightFirst countRepl chatRepl) ((((0, Nothing), 0), ((0, Nothing), 0)), 5)
            == ((((0, Nothing), 1), ((0, Nothing), 1)), Said (1, 0)),
      -- race, neither speaks: the composite stands down and nothing
      -- advances. Counterexample: an output-inspecting race reports a
      -- winner; a non-revoking race advances both counters.
      checkV verbosity "race: two stand-downs stand down, neither leg advances" $
        turn (race LeftFirst muteRepl muteRepl) (((0, 0), (0, 0)), 5)
          == (((0, 0), (0, 0)), StandDown),
      -- revocation persists: the loser does not catch up next turn.
      -- Counterexample: stale-state loss on re-entry lets the right
      -- counter leave 0.
      checkV verbosity "race: the revoked leg never catches up" $
        let r = race LeftFirst countRepl chatRepl
            s0 = (((0, Nothing), 0), ((0, Nothing), 0))
            (s1, _) = turn r (s0, 5)
         in turn r (s1, 6)
              == ((((2, Just 6), 0), ((2, Just 6), 0)), Said (2, 6))
    ]
