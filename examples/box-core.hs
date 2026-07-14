{-# LANGUAGE FlexibleInstances #-}

-- | Box: profunctor streaming with Trace Either (Kleisli Identity).
--
--   Types:
--     Box c e        = Trace Either (Kleisli Identity) c e
--     Emitter a      = Box () a      — produces one a per step
--     Committer a    = Box a ()      — consumes one a per step
--
--   The realised form is a single step: c -> e.
--   Multi-step (streaming) is circuit composition, not function call.
--
--   Unit creates a matched pair. Counit annihilates: runB . Compose.
--
--   Note: a generic @Monad m@ version is blocked by overlapping
--   'Traced (Kleisli m) Either' instances in the current library
--   (the IO-specific instance overlaps the general Monad instance).
--   This example uses 'Identity' to stay pure and compile cleanly.
module Box where

import Circuit.Trace (Trace (..), realise)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category ((.))
import Data.Functor.Identity (Identity (..))
import Prelude hiding (id, (.))

-- Core types
type Box c e = Trace Either (Kleisli Identity) c e

type Emitter a = Box () a

type Committer a = Box a ()

-- Lower: interpret to a pure function (single step)
runB :: Box c e -> c -> e
runB b c = runIdentity (runKleisli (realise b) c)

runE :: Emitter a -> a
runE e = runB e ()

runC :: Committer a -> a -> ()
runC = runB

-- Unit: create a bidirectional channel from a value.
--   The Emitter produces a, the Committer consumes a.
unit :: a -> (Emitter a, Committer a)
unit a = (Lift (Kleisli (const (pure a))), Lift (Kleisli (const (pure ()))))

-- Counit: compose and run — the annihilator.
counit :: Committer a -> Emitter a -> ()
counit c e = runB (Compose c e) ()

-- Glue: convenience alias for counit
glue :: Committer a -> Emitter a -> ()
glue = counit

-- ---------------------------------------------------------------------------
-- Emitter combinators
-- ---------------------------------------------------------------------------

-- | Emit a single value and stop.
yield :: a -> Emitter a
yield = Lift . Kleisli . const . pure

-- ---------------------------------------------------------------------------
-- Committer combinators
-- ---------------------------------------------------------------------------

-- | Consume a value.
consume :: (a -> ()) -> Committer a
consume f = Lift (Kleisli (Identity . f))

-- | Always accept.
accept :: Committer a
accept = Lift (Kleisli (const (pure ())))
