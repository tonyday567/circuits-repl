-- | Repl oracles: the repl loop, turn outputs, the runners, and the
-- race and await compositions.
--
-- Each check names its counterexample: the counter-seats are built so
-- that a stateless commit, a pre-commit emit, a selection instead of a
-- product, a bias leak into single-Said races, or a runner that
-- disagrees with 'scanProcess' would each turn a specific check red.
module Axioma.Repl
  ( replTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Machine (scanProcess)
import Circuit.Repl
import Circuit.Tensor (Bias (..))
import Control.Monad (when)
import Prelude hiding (init)

-- | Counts and stores the input on commit; the utter cites both. A
-- stateless commit yields 1 forever; a pre-commit emit cites the
-- previous input.
countRepl :: Repl (Int, Maybe Int) Int (Int, Int)
countRepl =
  mkRepl
    (0, Nothing)
    (\(n, _) i -> (n + 1, Just i))
    (\(n, mi) -> maybe Done (Said . (n,)) mi)

-- | Never prints; the commit still counts.
muteRepl :: Repl Int Int (Int, Int)
muteRepl = mkRepl 0 (\s _ -> s + 1) (const Done)

-- | Always prints; the payload differs from 'countRepl's.
chatRepl :: Repl Int Int (Int, Int)
chatRepl = mkRepl 0 (\s _ -> s + 1) (\s -> Said (s, 0))

replTopic :: Verbosity -> IO [Bool]
replTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Repl oracles"
  sequence
    [ -- state is continuous across turns. Counterexample: a stateless
      -- commit yields (1,_) on every turn.
      checkV verbosity "the loop threads state across turns" $
        runRepl countRepl [10, 20, 30]
          == [ ((1, Just 10), Said (1, 10)),
               ((2, Just 20), Said (2, 20)),
               ((3, Just 30), Said (3, 30))
             ],
      -- print reads the fresh state. Counterexample: a pre-commit emit
      -- cites the previous input (Nothing on turn one).
      checkV verbosity "print reads the post-commit state" $
        session countRepl [10, 20]
          == [Said (1, 10), Said (2, 20)],
      -- 'Done' is a value, and the commit still landed.
      -- Counterexample: a list-shaped turn output yields [], and
      -- "evaluated, nothing to say" is indistinguishable from "no
      -- turn happened".
      checkV verbosity "done is a value, and the commit still lands" $
        step muteRepl (0, 7) == (1, Done),
      -- our from-scratch runner agrees with the Machine runner on the
      -- Moore corner. Counterexample: an init/commit disagreement or
      -- an off-by-one in runRepl splits the two.
      checkV verbosity "runRepl agrees with scanProcess on the Moore corner" $
        session countRepl [10, 20, 30]
          == scanProcess (replMoore countRepl) [10, 20, 30],
      -- await: both legs commit, both states advance, both outputs
      -- land. Counterexample: a selection (race-shaped) drops one
      -- output and leaves one state behind.
      checkV verbosity "await lands both outputs and advances both states" $
        step (await countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Said (Said (1, 5), Said (1, 0))),
      -- await across a script: neither leg falls behind.
      checkV verbosity "await advances both legs through a session" $
        session (await countRepl chatRepl) [5, 6]
          == [ Said (Said (1, 5), Said (1, 0)),
               Said (Said (2, 6), Said (2, 0))
             ],
      -- a leg's Done stays visible in its own slot. Counterexample: a
      -- flattening await yields Said (1,5) and the mute leg's output
      -- is gone.
      checkV verbosity "await keeps a done leg visible in its own slot" $
        step (await countRepl muteRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Said (Said (1, 5), Done)),
      -- the composite is Done exactly when both legs are.
      -- Counterexample: an always-Said product claims output on a
      -- quiet turn; an any-Done product goes quiet while a leg prints.
      checkV verbosity "await is done exactly when both legs are" $
        snd (step (await muteRepl muteRepl) ((0, 0), 5)) == Done,
      -- race, one printer: the printer wins under either bias. Both
      -- commits landed — the mute leg's counter advanced; only its
      -- print was revoked. Counterexamples: a bias leak flips the
      -- output under the other bias; a turn-revoking race leaves the
      -- mute counter at 0.
      checkV verbosity "race: a lone printer wins under either bias; its turn is not revoked" $
        let leftPrints bias =
              step (race bias countRepl muteRepl) (((0, Nothing), 0), 5)
                == (((1, Just 5), 1), Said (1, 5))
            rightPrints bias =
              step (race bias muteRepl countRepl) ((0, (0, Nothing)), 5)
                == ((1, (1, Just 5)), Said (1, 5))
         in all leftPrints [LeftFirst, RightFirst]
              && all rightPrints [LeftFirst, RightFirst],
      -- race, two printers: the explicit bias is the whole decision —
      -- the states are identical under either bias. Counterexample: a
      -- hidden left preference turns the RightFirst clause red.
      checkV verbosity "race: two printers are decided by the named bias alone" $
        step (race LeftFirst countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Said (1, 5))
          && step (race RightFirst countRepl chatRepl) (((0, Nothing), 0), 5)
            == (((1, Just 5), 1), Said (1, 0)),
      -- race, two Dones: the composite is Done, both commits landed.
      -- Counterexample: an output-inspecting race reports a winner.
      checkV verbosity "race: two dones are done" $
        step (race LeftFirst muteRepl muteRepl) ((0, 0), 5)
          == ((1, 1), Done),
      -- race across turns: both legs keep advancing; the bias keeps
      -- deciding the output.
      checkV verbosity "race: both legs advance through a session, the bias keeps deciding" $
        session (race LeftFirst countRepl chatRepl) [5, 6]
          == [Said (1, 5), Said (2, 6)]
    ]
