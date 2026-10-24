-- | Repl oracles: the repl loop, turn outputs, the runners, and the
-- race and await compositions.
--
-- Each check names its counterexample: the counter-seats are built so
-- that a stateless step, a pre-step extract, a selection instead of a
-- product, or a bias leak into single-payload races would each turn a
-- specific check red.
module Axioma.Repl
  ( replTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Machine (FoldBody (..))
import Circuit.Repl
import Circuit.Tensor (Bias (..))
import Control.Monad (when)
import Data.Maybe (isNothing)
import Prelude

-- | Counts and stores the input on step; the extract cites both. A
-- stateless step yields 1 forever; a pre-step extract cites the
-- previous input.
countRepl :: Repl (Int, Maybe Int) Int (Int, Int)
countRepl =
  FoldBody
    { begin = const (0, Nothing),
      step = \((n, _), i) -> (n + 1, Just i),
      extract = \(n, mi) -> (n,) <$> mi
    }

-- | Never prints; the step still counts.
muteRepl :: Repl Int Int (Int, Int)
muteRepl = FoldBody (const 0) (\(s, _) -> s + 1) (const Nothing)

-- | Always prints; the payload differs from 'countRepl's.
chatRepl :: Repl Int Int (Int, Int)
chatRepl = FoldBody (const 0) (\(s, _) -> s + 1) (\s -> Just (s, 0))

replTopic :: Verbosity -> IO [Bool]
replTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Repl oracles"
  sequence
    [ -- state is continuous across turns. Counterexample: a stateless
      -- step yields (1,_) on every turn.
      checkV verbosity "the loop threads state across turns" $
        runRepl countRepl [10, 20, 30]
          == [ ((1, Just 10), Just (1, 10)),
               ((2, Just 20), Just (2, 20)),
               ((3, Just 30), Just (3, 30))
             ],
      -- print reads the fresh channel. Counterexample: a pre-step
      -- extract cites the previous input (Nothing on turn one).
      checkV verbosity "print reads the post-step state" $
        session countRepl [10, 20]
          == [Just (1, 10), Just (2, 20)],
      -- Nothing on a quiet turn, and the step still landed.
      -- Counterexample: a list-shaped turn output yields [], and
      -- "evaluated, nothing to say" is indistinguishable from "no
      -- turn happened".
      checkV verbosity "nothing on a quiet turn, and the step still lands" $
        turn muteRepl (0, 7) == (1, Nothing),
      -- await: both legs step, both states advance, both outputs
      -- land. Counterexample: a selection (race-shaped) drops one
      -- output and leaves one state behind.
      checkV verbosity "await lands both outputs and advances both states" $
        turn (await countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Just (Just (1, 5), Just (1, 0))),
      -- await across a script: neither leg falls behind.
      checkV verbosity "await advances both legs through a session" $
        session (await countRepl chatRepl) [5, 6]
          == [ Just (Just (1, 5), Just (1, 0)),
               Just (Just (2, 6), Just (2, 0))
             ],
      -- a leg's Nothing stays visible in its own slot.
      -- Counterexample: a flattening await yields Just (1,5) and the
      -- mute leg's output is gone.
      checkV verbosity "await keeps a quiet leg visible in its own slot" $
        turn (await countRepl muteRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Just (Just (1, 5), Nothing)),
      -- the composite prints nothing exactly when both legs do.
      -- Counterexample: an always-Just product claims output on a
      -- quiet turn; an any-Nothing product goes quiet while a leg
      -- prints.
      checkV verbosity "await is quiet exactly when both legs are" $
        isNothing (snd (turn (await muteRepl muteRepl) ((0, 0), 5))),
      -- race, one printer: the printer wins under either bias. Both
      -- steps landed — the mute leg's counter advanced; only its
      -- print was revoked. Counterexamples: a bias leak flips the
      -- output under the other bias; a turn-revoking race leaves the
      -- mute counter at 0.
      checkV verbosity "race: a lone printer wins under either bias; its turn is not revoked" $
        let leftPrints bias =
              turn (race bias countRepl muteRepl) (((0, Nothing), 0), 5)
                == (((1, Just 5), 1), Just (1, 5))
            rightPrints bias =
              turn (race bias muteRepl countRepl) ((0, (0, Nothing)), 5)
                == ((1, (1, Just 5)), Just (1, 5))
         in all leftPrints [LeftFirst, RightFirst]
              && all rightPrints [LeftFirst, RightFirst],
      -- race, two printers: the explicit bias is the whole decision —
      -- the states are identical under either bias. Counterexample: a
      -- hidden left preference turns the RightFirst clause red.
      checkV verbosity "race: two printers are decided by the named bias alone" $
        turn (race LeftFirst countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Just (1, 5))
          && turn (race RightFirst countRepl chatRepl) (((0, Nothing), 0), 5)
            == (((1, Just 5), 1), Just (1, 0)),
      -- race, two Nothings: the composite is Nothing, both steps
      -- landed. Counterexample: an output-inspecting race reports a
      -- winner.
      checkV verbosity "race: two nothings are nothing" $
        turn (race LeftFirst muteRepl muteRepl) ((0, 0), 5)
          == ((1, 1), Nothing),
      -- race across turns: both legs keep advancing; the bias keeps
      -- deciding the output.
      checkV verbosity "race: both legs advance through a session, the bias keeps deciding" $
        session (race LeftFirst countRepl chatRepl) [5, 6]
          == [Just (1, 5), Just (2, 6)]
    ]
