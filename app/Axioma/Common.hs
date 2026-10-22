-- | Shared helpers for the circuits-repl axioma topic modules.
module Axioma.Common
  ( -- * Verbosity
    Verbosity (..),
    checkV,
    checkIOV,
  )
where

import Circuit.Axioma.Test (check)

-- | Output granularity for the axioma runner.
data Verbosity
  = -- | One symbol for the whole run.
    Package
  | -- | One symbol per topic.
    Topic
  | -- | One symbol per axiom (the default).
    Axioms
  deriving (Show, Eq)

-- | Print a PASS/FAIL line only when verbosity is 'Axioms'.
checkV :: Verbosity -> String -> Bool -> IO Bool
checkV Axioms name ok = check name ok
checkV _ _ ok = pure ok

-- | Print a PASS/FAIL line for an IO assertion only when verbosity is 'Axioms'.
checkIOV :: Verbosity -> String -> IO Bool -> IO Bool
checkIOV Axioms name act = do
  ok <- act
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ name
  pure ok
checkIOV _ _ act = act
