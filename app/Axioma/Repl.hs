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
import Circuit.Repl
import Circuit.Repl.Transform (batchIn, dimap, drainOut, flattenAwait, idleable, queueOut, unbatchOut)
import Circuit.Tensor (Bias (..))
import Control.Monad (when)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Prelude

-- | Counts and stores the input on step; the extract cites both. A
-- stateless step yields 1 forever; a pre-step extract cites the
-- previous input.
countRepl :: Repl (Int, Maybe Int) Int (Int, Int)
countRepl =
  fixedBirth
    (const (0, Nothing))
    (\((n, _), i) -> (n + 1, Just i))
    (\(n, mi) -> (n,) <$> mi)

-- | Never prints; the step still counts.
muteRepl :: Repl Int Int (Int, Int)
muteRepl = fixedBirth (const 0) (\(s, _) -> s + 1) (const Nothing)

-- | Always prints; the payload differs from 'countRepl's.
chatRepl :: Repl Int Int (Int, Int)
chatRepl = fixedBirth (const 0) (\(s, _) -> s + 1) (\s -> Just (s, 0))

-- | The three-leg fixed-birth reading — a unit-sourced birth, a step, and a
-- post-step print — spelt directly, independent of 'Repl' and of the
-- transforms.
refRun :: (() -> s) -> ((s, i) -> s) -> (s -> Maybe o) -> [i] -> [(s, Maybe o)]
refRun b k e = go (b ())
  where
    go _ [] = []
    go s (i : is) = let s' = k (s, i) in (s', e s') : go s' is

-- | The migration witness: 'fixedBirth' against the three-leg reading above.
--
-- Coverage: the empty tape; first and later turns; the print citing the
-- post-commit state; quiet turns that still advance state; 'await' and both
-- 'race' biases, including that both legs' first turns occur; and the batch,
-- queue, drain, unbatch, idle, mapping and broadcast scripts. The
-- integration half is the bus topic, unchanged by the migration, whose
-- transcript pins an acknowledgement citing the fresh cursor.
fixedBirthAgreement :: Bool
fixedBirthAgreement =
  let
    -- source one: the state doubles and adds the input; prints even states only
    b = const (0 :: Int)
    k = \(s, i) -> s * 2 + i
    e s = if even s then Just s else Nothing
    r = fixedBirth b k e
    ref = refRun b k e
    tape = [1, 2, 3, 4 :: Int]

    -- source two: never quiet
    b2 = const (100 :: Int)
    k2 = \(s, i) -> s + i
    e2 = Just
    r2 = fixedBirth b2 k2 e2

    -- the two legs' own per-turn references, paired: the composite rows below
    -- are built from these rather than through 'await' and 'race'
    leg1 = refRun b k e tape
    leg2 = refRun b2 k2 e2 tape
    pairStates = zip (map fst leg1) (map fst leg2)
    pairs = zip (map snd leg1) (map snd leg2)

    both (o1, o2) = case (o1, o2) of
      (Nothing, Nothing) -> Nothing
      p -> Just p
    biasLeft (o1, o2) = case (o1, o2) of
      (Just x, _) -> Just x
      (Nothing, Just y) -> Just y
      (Nothing, Nothing) -> Nothing
    biasRight (o1, o2) = case (o1, o2) of
      (_, Just y) -> Just y
      (Just x, Nothing) -> Just x
      (Nothing, Nothing) -> Nothing
    flat (o1, o2) = case catMaybes [o1, o2] of
      [] -> Nothing
      os -> Just os

    lists = fixedBirth (const 0) (\(s, _) -> s + 1) (\s -> Just [s, s + 10 :: Int])
   in
    and
      [ -- the empty tape runs no turn and observes nothing
        null (runRepl r []) && null (session r []) && null (ref []),
        -- first and later turns agree with the three-leg reading
        runRepl r tape == ref tape && session r tape == map snd (ref tape),
        -- the print cites the state the commit just produced
        session r [1, 2] == [e (k (b (), 1)), e (k (k (b (), 1), 2))],
        -- every turn advances, including the quiet ones
        map fst (runRepl r tape) == [1, 4, 11, 26]
          && map snd (runRepl r tape) == [Nothing, Just 4, Nothing, Just 26],
        -- a mid-session turn is the same leg as an in-session turn
        turn r (4, 3) == (k (4, 3), e (k (4, 3))),
        -- await: both first turns occur and the composite matches the two-leg
        -- reference
        runRepl (await r r2) tape == zip pairStates (map both pairs)
          && map fst (runRepl (await r r2) [1]) == [(1, 101)],
        -- race: the named bias decides, and both states advance
        runRepl (race LeftFirst r r2) tape == zip pairStates (map biasLeft pairs)
          && runRepl (race RightFirst r r2) tape == zip pairStates (map biasRight pairs),
        -- batch: one turn carrying the whole tape, payloads in encounter order
        session (batchIn b r) [tape] == [Just (catMaybes (map snd (ref tape)))],
        -- queue: a single-output source keeps the observed stream in phase,
        -- and the final drain recovers it exactly
        session (queueOut r) tape == session r tape
          && drainOut r tape == catMaybes (map snd (ref tape)),
        -- unbatch keeps the head of each batch
        session (unbatchOut lists) [0, 0, 0] == map (>>= listToMaybe) (session lists [0, 0, 0]),
        -- idle ticks run no source turn and replay nothing; real inputs do
        map fst (runRepl (idleable b r) [Nothing, Just 1, Nothing, Just 2])
          == [(0, False), (1, True), (1, False), (4, True)]
          && session (idleable b r) [Nothing, Just 1, Nothing, Just 2]
            == [Nothing, Nothing, Nothing, Just 4],
        -- dimap premaps the input and postmaps only present payloads
        session (dimap (+ 1) (* 2) r) [1, 2] == map (fmap (* 2)) (session r [2, 3]),
        -- the broadcast: left-to-right payloads, silence exactly when both
        -- legs are silent
        session (flattenAwait r r2) tape == map flat pairs
      ]

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
          == [Just (1, 5), Just (2, 6)],
      -- the migration witness: the Body-shaped seat against the independent
      -- three-leg reading, across runners, compositions and transforms
      checkV verbosity "fixedBirth agrees with the three-leg reading across runners, compositions and transforms" fixedBirthAgreement
    ]
