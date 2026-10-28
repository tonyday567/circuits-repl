-- | Behavioural oracles for the repl interface transforms.
--
-- Each witness names a mutation it catches: dropped quiet-turn transitions,
-- reversed queues, replayed idle output, discarded pending output, or mapping
-- applied at the wrong interface.
module Axioma.Transform
  ( transformTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Machine (FoldBody (..))
import Circuit.Repl
import Circuit.Repl.Transform
import Control.Monad (when)
import Data.Maybe (catMaybes, mapMaybe)
import Prelude

countRepl :: Repl (Int, Maybe Int) Int (Int, Int)
countRepl =
  FoldBody
    { begin = const (0, Nothing),
      step = \((n, _), i) -> (n + 1, Just i),
      extract = \(n, mi) -> (n,) <$> mi
    }

quietEverySecond :: Repl (Int, Maybe Int) Int (Int, Int)
quietEverySecond =
  FoldBody
    { begin = const (0, Nothing),
      step = \((n, _), i) -> (n + 1, Just i),
      extract = \(n, mi) -> if even n then Nothing else (n,) <$> mi
    }

muteRepl :: Repl Int Int (Int, Int)
muteRepl = FoldBody (const 0) (\(s, _) -> s + 1) (const Nothing)

chatRepl :: Repl Int Int (Int, Int)
chatRepl = FoldBody (const 0) (\(s, _) -> s + 1) (\s -> Just (s, 0))

listRepl :: Repl Int Int [Int]
listRepl =
  FoldBody
    (const 0)
    (\(s, _) -> s + 1)
    ( \case
        1 -> Just [10, 11]
        2 -> Just []
        _ -> Nothing
    )

transformTopic :: Verbosity -> IO [Bool]
transformTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Transform oracles"
  sequence
    [ -- A batched turn is the independent sequence of source turns. A
      -- quiet-second implementation that stops folding, reverses output, or
      -- leaves stale state fails.
      checkV verbosity "batchIn matches folded source turns with quiet turns collected out" $
        let r = quietEverySecond
            s0 = (0, Nothing)
            (s1, o1) = turn r (s0, 10)
            (s2, o2) = turn r (s1, 20)
            (s3, o3) = turn r (s2, 30)
            os = catMaybes [o1, o2, o3]
         in turn (batchIn r) ((s0, [(-1, -1)]), [10, 20, 30])
              == ((s3, os), Just os),
      -- Empty and all-quiet batches have an evaluated empty collection, and
      -- all-quiet inputs still advance every source state.
      checkV verbosity "batchIn represents empty and all-quiet batches as Just empty" $
        turn (batchIn muteRepl) ((0, [(0, 0)]), [])
          == ((0, []), Just [])
          && turn (batchIn muteRepl) ((0, [(0, 0)]), [7, 8])
            == ((2, []), Just []),
      -- Include the pending tail in the observation. Dropping that tail or
      -- advancing a different underlying state breaks the flattened-stream
      -- comparison with the independent source run.
      checkV verbosity "queueOut preserves flattened output including its pending tail" $
        let source = runRepl quietEverySecond [10, 20, 30]
            queued = runRepl (queueOut quietEverySecond) [10, 20, 30]
            pending = drop 1 (snd (fst (last queued)))
         in mapMaybe snd queued <> pending
              == mapMaybe snd source
              && fst (fst (last queued)) == fst (last source),
      -- The head is the previous observation and the tail is pending. A
      -- drain-before-enqueue mutation emits the fresh payload before the
      -- preloaded pending value; a queue-reversing mutation also goes red.
      checkV verbosity "queueOut drains a preloaded pending head before fresh output" $
        turn (queueOut countRepl) (((0, Nothing), [(-1, 0), (-2, 0)]), 5)
          == (((1, Just 5), [(-2, 0), (1, 5)]), Just (-2, 0))
          && turn (queueOut countRepl) (((1, Just 5), [(-2, 0), (1, 5)]), 6)
            == (((2, Just 6), [(1, 5), (2, 6)]), Just (1, 5)),
      -- Final drain is compared with the source, not with queueOut's own
      -- projected output. An implementation that omits pending output or
      -- invents output for an empty session fails.
      checkV verbosity "drainOut observes exactly the flattened source stream" $
        drainOut quietEverySecond [10, 20, 30]
          == [(1, 10), (3, 30)]
          && null (drainOut countRepl []),
      -- First-or-nothing: the tail is not saved for turn two, empty and quiet
      -- batches stay quiet, and every source state transition still lands.
      checkV verbosity "unbatchOut keeps only the head and discards the tail permanently" $
        runRepl (unbatchOut listRepl) [0, 0, 0]
          == [(1, Just 10), (2, Nothing), (3, Nothing)],
      -- Just input delegates exactly to the source turn; only the trigger bit
      -- is added to its resulting channel.
      checkV verbosity "idleable Just input is exactly the source turn" $
        turn (idleable countRepl) (((0, Nothing), False), Just 5)
          == (((1, Just 5), True), Just (1, 5)),
      -- Idle after a payload suppresses extract without touching source
      -- state. A same-channel implementation would replay Just (1,5) here.
      checkV verbosity "idleable Nothing is quiet and leaves source state untouched" $
        turn (idleable countRepl) (((1, Just 5), True), Nothing)
          == (((1, Just 5), False), Nothing),
      -- Idle ticks around real inputs neither evaluate nor replay output.
      -- Reevaluating idle changes both later counts and payloads.
      checkV verbosity "idleable scripts keep quiet ticks observationally invisible" $
        session (idleable countRepl) [Nothing, Just 5, Nothing, Just 6, Nothing]
          == [Nothing, Just (1, 5), Nothing, Just (2, 6), Nothing],
      -- Compare with an independent source turn. Applying the premap to the
      -- channel, omitting the postmap, or manufacturing output fails.
      checkV verbosity "dimap precomposes input and postcomposes only present payloads" $
        let (s', mo) = turn countRepl ((0, Nothing), 6)
         in turn (dimap (+ 1) (uncurry (+)) countRepl) ((0, Nothing), 5)
              == (s', fmap (uncurry (+)) mo)
              && session (dimap id id countRepl) [5, 6]
                == session countRepl [5, 6],
      -- Both await transitions land and left precedes right. Reversing the
      -- list or selecting one leg fails.
      checkV verbosity "flattenAwait preserves both states and left-to-right payloads" $
        turn (flattenAwait countRepl chatRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Just [(1, 5), (1, 0)]),
      -- Lone payloads survive in either position. Zip semantics fails both.
      checkV verbosity "flattenAwait preserves a lone payload from either leg" $
        turn (flattenAwait countRepl muteRepl) (((0, Nothing), 0), 5)
          == (((1, Just 5), 1), Just [(1, 5)])
          && turn (flattenAwait muteRepl countRepl) ((0, (0, Nothing)), 5)
            == ((1, (1, Just 5)), Just [(1, 5)]),
      -- Two quiet legs produce outer silence rather than Just []; both source
      -- transitions still land.
      checkV verbosity "flattenAwait is quiet exactly when both legs are" $
        turn (flattenAwait muteRepl muteRepl) ((0, 0), 5)
          == ((1, 1), Nothing)
    ]
