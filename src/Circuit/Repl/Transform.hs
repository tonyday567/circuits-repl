-- | Interface transforms for repl loops.
--
-- The transforms preserve the underlying source transition unless their
-- haddock names a loss. Output queues live in the visible channel so a driver
-- can inspect output that a turn has not yet exposed.
module Circuit.Repl.Transform
  ( batchIn,
    queueOut,
    drainOut,
    unbatchOut,
    idleable,
    dimap,
    flatten,
    flattenAwait,
  )
where

import Circuit.Machine (FoldBody (..))
import Circuit.Repl (Repl, await, runRepl, turn)
import Control.Monad ((>=>))
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Prelude

-- | Fold a whole input batch in one turn. The channel stores the payloads
-- collected from that batch in encounter order; the turn always emits that
-- collection, including @Just []@ for an empty or all-quiet batch.
--
-- The underlying final state and collected payloads agree with applying
-- 'turn' to each input from left to right.
batchIn :: Repl s i o -> Repl (s, [o]) [i] [o]
batchIn r@(FoldBody b _ _) =
  FoldBody
    (\() -> (b (), []))
    (\((s, _), is) -> go s [] is)
    (Just . snd)
  where
    go s acc [] = (s, reverse acc)
    go s acc (i : is) =
      let (s', mo) = turn r (s, i)
       in go s' (maybe acc (: acc) mo) is

-- | Queue source payloads and expose at most one per turn. In the widened
-- channel, the queue head is the payload exposed by the latest turn and the
-- tail is still pending. A step retires that previous head, evaluates the
-- source exactly once, and appends any fresh payload.
--
-- Observed payloads followed by the pending tail equal the source payloads in
-- encounter order. From birth, a single-output source and a single-output
-- drain keep the queue length at most one; preloaded channels exercise the
-- backlogged case.
queueOut :: Repl s i o -> Repl (s, [o]) i o
queueOut r@(FoldBody b _ _) =
  FoldBody
    (\() -> (b (), []))
    ( \((s, q), i) ->
        let (s', mo) = turn r (s, i)
         in (s', drop 1 q <> maybeToList mo)
    )
    (\(_, q) -> listToMaybe q)

-- | Run a session through 'queueOut' and observe both its emitted payloads
-- and any final pending tail. This final-drain observation preserves the
-- flattened source stream.
--
-- This is not an inverse from @Repl (s,[o]) i o@ to @Repl s i o@: erasing a
-- queue from an arbitrary live channel would discard pending output. The
-- observation makes the drain policy explicit instead.
drainOut :: Repl s i o -> [i] -> [o]
drainOut r is =
  case runRepl (queueOut r) is of
    [] -> []
    rs -> mapMaybe snd rs <> drop 1 (snd (fst (last rs)))

-- | Keep only the first payload of each present output batch. The source
-- state transition is unchanged. @Nothing@ and @Just []@ are quiet; every
-- tail element is discarded rather than delayed.
unbatchOut :: Repl s i [o] -> Repl s i o
unbatchOut (FoldBody b k e) = FoldBody b k (e >=> listToMaybe)

-- | Add input-free ticks. The added trigger bit distinguishes an idle tick
-- from a real input whose source state happens to be unchanged: idle leaves
-- the underlying source state untouched and emits nothing, while @Just i@
-- performs exactly the source turn.
--
-- The trigger bit clears on idle; the source state projection is the
-- observationally invisible part. A same-channel wrapper cannot express this
-- law when the source extracts a payload from an unchanged state.
idleable :: Repl s i o -> Repl (s, Bool) (Maybe i) o
idleable (FoldBody b k e) =
  FoldBody
    (\() -> (b (), False))
    ( \((s, _), mi) ->
        case mi of
          Nothing -> (s, False)
          Just i -> (k (s, i), True)
    )
    (\(s, fire) -> if fire then e s else Nothing)

-- | Precompose the input and postcompose every present output payload. The
-- source channel and silence are unchanged.
dimap :: (i' -> i) -> (o -> o') -> Repl s i o -> Repl s i' o'
dimap f g (FoldBody b k e) =
  FoldBody b (\(s, i') -> k (s, f i')) (fmap g . e)

-- | Flatten an await-shaped payload pair into left-to-right order. Outer
-- silence stays silent; a present pair contributes each present component.
flatten :: Repl s i (Maybe o, Maybe o) -> Repl s i [o]
flatten = dimap id (\(left, right) -> maybeToList left <> maybeToList right)

-- | The broadcast repl: 'await' followed by 'flatten', with left-to-right
-- output order and both source transitions preserved.
flattenAwait ::
  Repl s1 i o ->
  Repl s2 i o ->
  Repl (s1, s2) i [o]
flattenAwait left right = flatten (await left right)
