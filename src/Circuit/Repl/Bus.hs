{-# LANGUAGE OverloadedStrings #-}

-- | A free-agent-like mock bus: an append-only log as the shared
-- rendezvous, and seats that are repl loops over it.
--
-- The shape is free-agent's, kept honest at mock scale:
--
-- * The log is shared — seats see every post, judge addressedness
--   locally, and stand down on the unaddressed. There is no bus-side
--   filtering.
-- * The bus is the only stamper. Seats never stamp; a 'Said' post is
--   stamped on append.
-- * Addressing is direct. There is no broadcast sentinel.
-- * Quiescence is Rest — a decidable fixpoint (every cursor at log
--   end), asserted by the oracles. Fuel bounds the loop; it is not a
--   timeout inference.
module Circuit.Repl.Bus
  ( -- * posts
    Name,
    Post (..),
    Logged (..),

    -- * seats
    SeatState (..),
    newSeatState,
    Seat (..),
    ackSeat,
    runnerSeat,

    -- * the driver
    BusResult (..),
    runBus,
  )
where

import Circuit.Repl (Repl, Speech (..), mkRepl, turn)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude hiding (init)

-- | A seat name.
type Name = Text

-- | A bus post: direct addressing and a thread edge to the parent.
data Post = Post
  { postFrom :: Name,
    postTo :: Name,
    postBody :: Text,
    postThread :: Maybe Int
  }
  deriving (Eq, Show)

-- | A post stamped by the bus — the only stamper.
data Logged = Logged
  { loggedId :: Int,
    loggedPost :: Post
  }
  deriving (Eq, Show)

-- | A seat's monologue: a cursor into the shared log, and the post
-- absorbed this turn for the print leg to read (the fresh channel).
data SeatState = SeatState
  { cursor :: Int,
    lastPost :: Maybe Logged
  }
  deriving (Eq, Show)

newSeatState :: SeatState
newSeatState = SeatState 0 Nothing

-- | A named repl at a state.
data Seat = Seat
  { seatName :: Name,
    seatRepl :: Repl SeatState Logged Post,
    seatState :: SeatState
  }

-- | The mock repl seat: absorbs every post it is shown (the cursor
-- advances — it saw the log), and prints an ack citing the parent
-- thread when addressed — unless the post is itself an ack. An ack
-- never acks an ack: the cascade is impossible by construction.
ackSeat :: Name -> Seat
ackSeat me = Seat me (mkRepl newSeatState absorb utter) newSeatState
  where
    absorb s l = s {cursor = cursor s + 1, lastPost = Just l}
    utter s = case lastPost s of
      Nothing -> StandDown
      Just l ->
        let p = loggedPost l
         in if postTo p /= me
              then StandDown
              else
                if "ack " `T.isPrefixOf` postBody p
                  then StandDown
                  else
                    Said
                      Post
                        { postFrom = me,
                          postTo = postFrom p,
                          postBody = "ack " <> T.pack (show (cursor s)),
                          postThread = Just (loggedId l)
                        }

-- | The runner seat: always stands down. Its prints enter the log only
-- via the external script — neutrality made typable.
runnerSeat :: Name -> Seat
runnerSeat me = Seat me (mkRepl newSeatState absorb (const StandDown)) newSeatState
  where
    absorb s l = s {cursor = cursor s + 1, lastPost = Just l}

-- | What a run produced: the final log, every act in order (stand-downs
-- included — the trace is the oracle subject), the final seats, and
-- whether quiescence was reached within fuel.
data BusResult = BusResult
  { busLog :: [Logged],
    busActs :: [(Name, Speech Post)],
    busSeats :: [Seat],
    busRested :: Bool
  }

-- | Run a roster of seats over a scripted log to quiescence.
--
-- Each pass drains every seat in roster order (the named
-- representative); a seat turns once per undelivered post in log order;
-- 'Said' posts are stamped and appended immediately, so later seats in
-- the same pass see them. Quiescent when every cursor is at log end —
-- a pass with work strictly advances a cursor or the log, so the loop
-- either rests or burns fuel.
runBus :: Int -> [Seat] -> [Post] -> BusResult
runBus fuel seats0 script = go fuel seats0 (stamp 0 script) []
  where
    stamp _ [] = []
    stamp n (p : ps) = Logged n p : stamp (n + 1) ps

    go f seats lg acts
      | all (rested lg) seats = BusResult lg (reverse acts) seats True
      | f <= 0 = BusResult lg (reverse acts) seats False
      | otherwise =
          let (seats', lg', acts') = drainPass seats lg acts
           in go (f - 1) seats' lg' acts'

    rested lg s = cursor (seatState s) >= length lg

    drainPass seats lg acts =
      foldl
        (\(ss, lg', as) seat -> let (s', lg'', as') = drainSeat lg' as seat in (ss ++ [s'], lg'', as'))
        ([], lg, acts)
        seats

    drainSeat lg acts seat =
      let pending = drop (cursor (seatState seat)) lg
       in foldl step (seat, lg, acts) pending
      where
        step (s, lg', as) l =
          let (st', act) = turn (seatRepl s) (seatState s, l)
           in case act of
                Said p ->
                  let l' = Logged (length lg') p
                   in (s {seatState = st'}, lg' ++ [l'], (seatName s, act) : as)
                StandDown -> (s {seatState = st'}, lg', (seatName s, act) : as)
