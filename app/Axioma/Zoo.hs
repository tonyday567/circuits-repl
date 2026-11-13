{-# LANGUAGE BlockArguments #-}

-- | Zoo oracles: the LSP row, pinned behaviourally.
--
-- The zoo states six machine shapes as type synonyms. A type synonym proves
-- nothing on its own — the mock pins one row ('Circuit.Repl.Repl', a pure
-- FBody seat) and the rest are claims about shapes nobody has run. This topic
-- pins the second row, and it pins the row where FBody is the wrong answer:
-- the LSP seat reports from the tick-start channel, so its print never contains
-- the input being evaluated.
--
-- The difference is one turn, and it is the whole difference between a seat
-- whose readers block on the in-flight recompute and a seat whose readers do
-- not. 'fbodyShaped' below reads the channel after @set@ instead of before it,
-- which is exactly what an FBody extract does; it is the counterexample held in
-- the same module as the checks, so the checks are visibly able to fail.
--
-- The taxonomy's four questions, answered for this row ⟜ birth is prior (the
-- driver seeds the channel and the first report is that seed), print timing is
-- pre-evaluate (the view leg reads the tick-start channel), evaluate is
-- input-dependent (@set@ folds each input into a fresh channel), and every full
-- tick takes an input.
--
-- The price the card named — "independence pays a copy" — is real for a
-- mutable channel: the view leg reads a channel that @set@ will replace, so
-- anything shared by reference needs a copy before the set. With a pure channel
-- the copy is a let-binding, which is why this oracle is legible; a mutable one
-- would need the same guarantee stated over the copy.
module Axioma.Zoo
  ( ghciTopic,
    zooTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Category (K (..))
import Circuit.Machine (Body (Body), LensBody (..))
import Circuit.Machine qualified as M
import Circuit.Repl.Zoo (Ghci, Lsp)
import Circuit.Tensor (Const (..), unConst)
import Control.Monad (when)
import Prelude

-- | A diagnostics count as the channel, a file's error count as the input. Small
-- enough to check by eye and the same shape as the real row: @set@ accumulates,
-- @view@ reports what has been published so far.
lsp :: Lsp Int Int Int
lsp =
  LensBody
    { set = K \(n, i) -> pure (n + i),
      view = K (pure . Just)
    }

-- | One tick, in the order the machine gives it: the print leg reads the
-- tick-start channel, then the evaluate leg produces the next one. A driver
-- that swapped these two lines would be running a different row.
tick :: Int -> Int -> IO (Maybe Int, Int)
tick ch i = do
  report <- runK (view lsp) ch
  ch' <- runK (set lsp) (ch, i)
  pure (report, ch')

-- | The counterexample, in FBody's shape: report from the channel @set@ just
-- produced. One line differs, and it is the line the row is about.
fbodyShaped :: Int -> Int -> IO (Maybe Int, Int)
fbodyShaped ch i = do
  ch' <- runK (set lsp) (ch, i)
  report <- runK (view lsp) ch'
  pure (report, ch')

drive :: (Int -> Int -> IO (Maybe Int, Int)) -> [Int] -> IO [(Maybe Int, Int)]
drive stepOf = go 0
  where
    go _ [] = pure []
    go ch (i : is) = do
      r <- stepOf ch i
      rest <- go (snd r) is
      pure (r : rest)

-- | Errors arriving per file, including a quiet file.
inputs :: [Int]
inputs = [10, 20, 0, 30]

zooTopic :: Verbosity -> IO [Bool]
zooTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Zoo oracles"
  lspTurns <- drive tick inputs
  fbodyTurns <- drive fbodyShaped inputs
  let reports = [r | (r, _) <- lspTurns]
      channels = [c | (_, c) <- lspTurns]
  sequence
    [ -- the report is the tick-start channel, so turn one reports the seed and
      -- the whole stream lags the channel stream by one. Counterexample: a
      -- post-set read reports [10, 30, 30, 60].
      checkV verbosity "the LSP row reports the tick-start channel" $
        reports == [Just 0, Just 10, Just 30, Just 30],
      -- the same fact as a stream identity rather than a sample: the report
      -- stream is the channel stream shifted by one turn. Stated this way the
      -- quiet input cannot make the check vacuous — turn three's report and
      -- channel coincide because the channel did not move, which is a fact
      -- about the input, not about the row.
      checkV verbosity "the report stream is the channel stream one turn behind" $
        reports == Just 0 : map Just (init channels),
      -- the channel still advances, so the row is not a frozen reader.
      checkV verbosity "the channel advances by every input" $
        channels == [10, 30, 30, 60],
      -- the quiet file is a real tick: the channel does not move and the report
      -- is still the previous channel rather than a stand-down.
      checkV verbosity "a quiet input is a tick, not a silence" $
        (reports !! 2) == Just 30 && (channels !! 2) == 30,
      -- the counterexample clears the same bar in the other direction: the
      -- FBody-shaped reader gives the post-set stream, and a check that cannot
      -- tell the two apart is not pinned to a row at all.
      checkV verbosity "the FBody-shaped reader gives the other stream" $
        [r | (r, _) <- fbodyTurns] == [Just 10, Just 30, Just 30, Just 60]
    ]

-- | The session channel: the boot configuration and the frames produced so far.
type Sess = (String, [String])

-- | The newest frame without a partial 'last'.
newest :: [String] -> String
newest = foldl (\_ x -> x) ""

-- | Exit is a frame, not a stand-down: the carrier is a plain output, so the
-- only way to say "the process ended" is to say it in the payload.
frameOf :: String -> String
frameOf "quit" = "<exited>"
frameOf cmd = "=> " <> cmd

-- | @Ghci s i o = Body (,) Const s s (K IO) i o@ — birth from the first input,
-- an input-dependent commit, a post-commit response.
ghci :: Ghci Sess String String
ghci = Body ini com emi
  where
    -- the config lives in the channel: a session cannot exist before its config
    ini = K \cfg -> pure (cfg, ["<spawned " <> cfg <> ">"])
    com = K \((cfg, fr), cmd) -> pure (cfg, fr <> [frameOf cmd])
    emi = K \(_, fr) -> pure (Const (newest fr))

data Order = Post | Pre

-- | Drive the row: birth from the config, then a turn per command with the
-- print leg either after the commit (the ghci row) or before it (the LSP row).
driveWith :: Order -> String -> [String] -> IO [(String, [String])]
driveWith order cfg cmds = do
  ch0 <- runK ini cfg
  go ch0 cmds
  where
    Body ini com emi = ghci
    go _ [] = pure []
    go ch (c : cs) = case order of
      Post -> do
        ch' <- runK com (ch, c)
        Const o <- runK emi ch'
        ((o, snd ch') :) <$> go ch' cs
      Pre -> do
        Const o <- runK emi ch
        ch' <- runK com (ch, c)
        ((o, snd ch') :) <$> go ch' cs

commandLine :: [String]
commandLine = ["1+1", ":info", "quit"]

ghciTopic :: Verbosity -> IO [Bool]
ghciTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Zoo oracles ⟜ ghci"
  phase <- driveWith Post "cfg-a" commandLine
  lagging <- driveWith Pre "cfg-a" commandLine
  cfgB <- driveWith Post "cfg-b" ["1+1"]
  let reports = [o | (o, _) <- phase]
      frames = [newest fr | (_, fr) <- phase]
  sequence
    [ checkV verbosity "the ghci row is born from its first input" $
        case (cfgB, phase) of
          ((_, frB) : _, (_, frP) : _) ->
            frB /= frP
              && "<spawned cfg-b>" `elem` frB
              && "<spawned cfg-a>" `elem` frP
          _ -> False,
      checkV verbosity "the ghci report is in phase with the channel (shift 0)" $
        reports == frames && reports == ["=> 1+1", "=> :info", "<exited>"],
      checkV verbosity "death arrives as a frame, not as a silence" $
        newest reports == "<exited>",
      checkV verbosity "the pre-commit order reproduces the LSP shift on the same input" $
        [o | (o, _) <- lagging] == ["<spawned cfg-a>", "=> 1+1", "=> :info"],
      -- the whole-row falsifier, carried from the row this one replaces: the LSP
      -- lag identity does not hold for a @Body@-shaped row. Stated as a
      -- negative claim over the two streams the same commands produce.
      checkV verbosity "the LSP lag identity does not hold for this row" $
        reports /= [o | (o, _) <- lagging]
    ]
