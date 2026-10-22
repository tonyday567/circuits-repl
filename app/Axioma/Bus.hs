{-# LANGUAGE OverloadedStrings #-}

-- | Bus oracles: the mock bus — bus-assigned ids, local address
-- decisions, thread edges, the structural cascade stop, and quiescence
-- as a fixpoint.
module Axioma.Bus
  ( busTopic,
  )
where

import Axioma.Common (Verbosity (..), checkV)
import Circuit.Repl (Reply (..))
import Circuit.Repl.Bus
import Control.Monad (when)
import Prelude hiding (init)

roster :: [Seat]
roster = [runnerSeat "runner", ackSeat "k1", ackSeat "k2"]

hello :: Post
hello = Post "runner" "k1" "hi" Nothing

-- | The one-post conversation: k1 answers (citing its fresh cursor
-- and the parent id), every seat drains the answer and is done, all
-- cursors rest at log end.
onePost :: BusResult
onePost = runBus 10 roster [hello]

busTopic :: Verbosity -> IO [Bool]
busTopic verbosity = do
  when (verbosity == Axioms) $ putStrLn "Bus oracles"
  sequence
    [ -- ids are 0..n-1 in append order. Counterexample: a seat-side
      -- stamp collides or reorders.
      checkV verbosity "the bus assigns ids sequentially in append order" $
        (loggedId <$> busLog onePost) == [0, 1],
      -- address decisions are local: k2's cursor rests at log end (it
      -- saw every entry) and it never printed. Counterexample:
      -- bus-side filtering leaves k2's cursor behind; a broadcast
      -- answer adds a k2 Said.
      checkV verbosity "address decisions are local: k2 saw everything, printed nothing" $
        let k2 = busSeats onePost !! 2
         in cursor (seatState k2) == length (busLog onePost)
              && not (any isK2Said (busReplies onePost)),
      -- the answer cites the fresh cursor — the post-commit
      -- observation. Counterexample: a pre-commit emit says "ack 0".
      checkV verbosity "the answer cites the fresh state" $
        case busLog onePost !! 1 of
          Logged _ p -> postBody p == "ack 1",
      -- the thread edge cites the parent. Counterexample: a
      -- thread-less answer (the orphan-replies regression) yields
      -- Nothing.
      checkV verbosity "the answer cites its parent" $
        case busLog onePost !! 1 of
          Logged _ p -> postThread p == Just 0 && postTo p == "runner" && postFrom p == "k1",
      -- the full conversation, pinned exactly: log, replies (Dones
      -- included), and quiescence. Counterexample: any driver that
      -- drops, reorders, or fabricates a turn breaks the trace.
      checkV verbosity "the one-post conversation, exact" $
        busRested onePost
          && busLog onePost
            == [ Logged 0 hello,
                 Logged 1 (Post "k1" "runner" "ack 1" (Just 0))
               ]
          && busReplies onePost
            == [ ("runner", Done),
                 ("k1", Said (Post "k1" "runner" "ack 1" (Just 0))),
                 ("k2", Done),
                 ("k2", Done),
                 ("runner", Done),
                 ("k1", Done)
               ],
      -- the cascade stop is the thread edge, not the body text: the
      -- same body is answered when it starts a thread and ignored
      -- when it carries one. Counterexample: a body-prefix rule
      -- answers neither (red on the first clause); a
      -- reply-to-everything seat answers both (red on the second).
      checkV verbosity "threaded posts are not answered; thread-starters with the same text are" $
        let starter = runBus 10 roster [Post "runner" "k1" "ack 9" Nothing]
            threaded = runBus 10 roster [Post "runner" "k1" "ack 9" (Just 0)]
         in length (busLog starter) == 2
              && busRested threaded
              && length (busLog threaded) == 1
              && all ((== Done) . snd) (busReplies threaded),
      -- the runner seat never prints. Counterexample: a runner-shaped
      -- auto-reply adds a Said from "runner".
      checkV verbosity "the runner seat is always done" $
        not (any isRunnerSaid (busReplies onePost)),
      -- state continuity through the bus: two posts, two distinct
      -- answers, each citing its own parent. Counterexample: a
      -- stateless seat says "ack 1" twice.
      checkV verbosity "two posts, two answers, each citing its own parent" $
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
