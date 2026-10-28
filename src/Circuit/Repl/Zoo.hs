-- | The repl zoo as six machine types.
--
-- Each row fixes the machine shape while leaving its channel and interface
-- carriers as parameters. The haddocks answer the taxonomy's four questions:
-- birth, print timing, evaluate's input-dependence, and trigger.
module Circuit.Repl.Zoo
  ( Lsp,
    Ghci,
    Cron,
    Echo,
    BuildRunner,
    Replay,
  )
where

import Circuit.Category (K)
import Circuit.Machine (Body, Cell, LensBody, Poles, ProfunctorBody, TheseBody)
import Circuit.Tensor (Const)

-- | LSP: effectful parallel set/view legs, with diagnostics read from the
-- tick-start channel. Birth is supplied by the enclosing driver, @set@ folds
-- each input into a fresh channel, and every full tick takes an input.
type Lsp s i d = LensBody (,) s s (K IO) i (Maybe d)

-- | ghci: effectful birth from the first input, an input-dependent commit,
-- and a post-commit response. The response carrier includes operational
-- outcomes such as process exit as content.
type Ghci s i o = Body (,) Const s s (K IO) i o

-- | Cron: effectful channel-only, input-only, and combined legs. Birth is
-- supplied by the enclosing driver, each firing leg produces its optional
-- event, and 'Circuit.Machine.chLeg' gives spontaneous ticks.
type Cron s i e = TheseBody (,) s s (K IO) i (Maybe e)

-- | Echo: a pure memoryless machine whose channel is the current input.
-- The first input seeds the channel, each input drives one write/read pair,
-- and the echo response is the present current input.
type Echo i = Poles i (->) i (Maybe i)

-- | Build-runner: an effectful pre/transform/post decomposition. Birth is
-- supplied by the enclosing driver, the request configures @pre@, @pmap@ is a
-- uniform channel transform, @post@ reports from the transformed channel,
-- and every full tick takes a request.
type BuildRunner s r o = ProfunctorBody (,) s s (K IO) r (Maybe o)

-- | Replay: a pure unit seed plus one fused input-dependent step. The fused
-- result carries the fresh channel and optional event together, with one tick
-- per replay input.
type Replay s i e = Cell (,) (,) s s (->) i (Maybe e)
