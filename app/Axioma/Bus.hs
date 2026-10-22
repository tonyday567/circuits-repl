{-# LANGUAGE OverloadedStrings #-}

-- | Bus oracles: the mock free-agent-like bus — single stamper, local
-- address judgement, thread edges, no ack-of-ack, and Rest (decidable
-- quiescence) instead of timeout inference.
module Axioma.Bus
  ( busTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Repl (Speech (..))
import Circuit.Repl.Bus
import Control.Monad (when)
import Prelude hiding (init)

roster :: [Seat]
roster = [runnerSeat "runner", ackSeat "k1", ackSeat "k2"]

hello :: Post
hello = Post "runner" "k1" "hi" Nothing

-- | The one-post conversation, traced by hand: k1 acks (citing its
-- fresh cursor and the parent id), everyone drains the ack and stands
-- down, all cursors rest at log end.
onePost :: BusResult
onePost = runBus 10 roster [hello]

busTopic :: Verbosity -> IO [Bool]
busTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Bus oracles"
  sequence
    [ -- the bus is the only stamper: ids are 0..n-1 in append order.
      -- Counterexample: a seat-side stamp collides or reorders.
      checkV verbosity "single stamper: ids are sequential in append order" $
        (loggedId <$> busLog onePost) == [0, 1],
      -- seats judge addressedness locally: k2 saw the whole log (its
      -- cursor rests at log end) yet never spoke. Counterexample:
      -- bus-side filtering leaves k2's cursor behind; broadcast-ack
      -- adds a k2 Said.
      checkV verbosity "address judgement is local: k2 saw everything, said nothing" $
        let k2 = busSeats onePost !! 2
         in cursor (seatState k2) == length (busLog onePost)
              && not (any isK2Said (busActs onePost)),
      -- the ack cites the fresh cursor — the post-commit observation.
      -- Counterexample: a pre-commit emit says "ack 0".
      checkV verbosity "the ack cites the fresh state" $
        case busLog onePost !! 1 of
          Logged _ p -> postBody p == "ack 1",
      -- the thread edge cites the parent. Counterexample: a thread-less
      -- reply (the orphan-replies regression) yields Nothing.
      checkV verbosity "the ack cites its parent" $
        case busLog onePost !! 1 of
          Logged _ p -> postThread p == Just 0 && postTo p == "runner" && postFrom p == "k1",
      -- the full conversation, pinned exactly: log, acts trace (every
      -- stand-down visible), and Rest. Counterexample: any driver that
      -- drops, reorders, or fabricates an act breaks the trace.
      checkV verbosity "the one-post conversation, exact" $
        busRested onePost
          && busLog onePost
            == [ Logged 0 hello,
                 Logged 1 (Post "k1" "runner" "ack 1" (Just 0))
               ]
          && busActs onePost
            == [ ("runner", StandDown),
                 ("k1", Said (Post "k1" "runner" "ack 1" (Just 0))),
                 ("k2", StandDown),
                 ("k2", StandDown),
                 ("runner", StandDown),
                 ("k1", StandDown)
               ],
      -- an ack is never acked: the cascade is impossible by
      -- construction. Counterexample: a reply-to-everything seat
      -- ping-pongs until fuel and returns busRested False.
      checkV verbosity "no ack-of-ack: the cascade dies at the first ack" $
        let result = runBus 10 roster [Post "runner" "k1" "ack 9" Nothing]
         in busRested result
              && length (busLog result) == 1
              && all ((== StandDown) . snd) (busActs result),
      -- the runner seat never speaks: its prints enter only via the
      -- script. Counterexample: a runner-shaped auto-reply adds a Said
      -- from "runner".
      checkV verbosity "the runner seat only stands down" $
        not (any isRunnerSaid (busActs onePost)),
      -- state continuity through the bus: two posts, two distinct
      -- acks, each citing its own parent. Counterexample: a stateless
      -- seat says "ack 1" twice.
      checkV verbosity "two posts, two acks, each citing its own parent" $
        let again = Post "runner" "k1" "again" Nothing
            result = runBus 10 roster [hello, again]
         in busRested result
              && busLog result
                == [ Logged 0 hello,
                     Logged 1 again,
                     Logged 2 (Post "k1" "runner" "ack 1" (Just 0)),
                     Logged 3 (Post "k1" "runner" "ack 2" (Just 1))
                   ]
    ]
  where
    isK2Said ("k2", Said _) = True
    isK2Said _ = False
    isRunnerSaid ("runner", Said _) = True
    isRunnerSaid _ = False
