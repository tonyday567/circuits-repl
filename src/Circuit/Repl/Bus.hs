{-# LANGUAGE OverloadedStrings #-}

-- | A mock of a free-agent-style bus: an append-only log, and seats
-- that are repl loops over it.
--
-- * Seats see every log entry and decide locally whether it is
--   addressed to them; there is no bus-side filtering.
-- * The bus assigns ids on append ('Logged'); seats never assign ids.
-- * Addressing is direct; there is no broadcast.
-- * The driver loops until every seat's cursor is at the log end —
--   quiescence as a fixpoint, not a timeout. Fuel only bounds the
--   loop; 'busRested' reports which of the two happened.
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

import Circuit.Machine (FBody (..))
import Circuit.Repl (Repl, turn)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | A seat name.
type Name = Text

-- | A bus post: direct addressing, and a thread edge to the parent
-- post's id when the post is a reply.
data Post = Post
  { postFrom :: Name,
    postTo :: Name,
    postBody :: Text,
    postThread :: Maybe Int
  }
  deriving (Eq, Show)

-- | A post with its bus-assigned id.
data Logged = Logged
  { loggedId :: Int,
    loggedPost :: Post
  }
  deriving (Eq, Show)

-- | Seat state: a cursor into the shared log, and the entry absorbed
-- this turn for the utter function to read.
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

-- | The mock repl seat. It answers a post when the post is addressed
-- to it and starts a thread ('postThread' is 'Nothing'); the answer
-- carries a thread edge to the post's id. Since every answer carries
-- an edge, no answer is ever answered — the cascade stop is the edge,
-- not the body text. This mock answers thread-starters only;
-- multi-turn threading is future work.
ackSeat :: Name -> Seat
ackSeat me = Seat me repl newSeatState
  where
    repl =
      FBody
        { begin = const newSeatState,
          step = \(s, l) -> s {cursor = cursor s + 1, lastPost = Just l},
          extract = \s -> case lastPost s of
            Nothing -> Nothing
            Just l ->
              let p = loggedPost l
               in if postTo p == me && isNothing (postThread p)
                    then
                      Just
                        Post
                          { postFrom = me,
                            postTo = postFrom p,
                            postBody = "ack " <> T.pack (show (cursor s)),
                            postThread = Just (loggedId l)
                          }
                    else Nothing
        }

-- | A seat that never prints. Its posts enter the log only via the
-- external script.
runnerSeat :: Name -> Seat
runnerSeat me = Seat me repl newSeatState
  where
    repl =
      FBody
        { begin = const newSeatState,
          step = \(s, l) -> s {cursor = cursor s + 1, lastPost = Just l},
          extract = const Nothing
        }

-- | The outcome of a run: the final log, every turn's output in order
-- ('Nothing's included), the final seats, and whether quiescence was
-- reached within fuel.
data BusResult = BusResult
  { busLog :: [Logged],
    busReplies :: [(Name, Maybe Post)],
    busSeats :: [Seat],
    busRested :: Bool
  }

-- | Run a roster of seats over a scripted log.
--
-- Each pass drains every seat in roster order; a seat steps once per
-- undelivered entry in log order; 'Just' posts are assigned an id and
-- appended immediately, so later seats in the same pass see them. The
-- loop stops when every cursor is at the log end. A pass with pending
-- work strictly advances a cursor or the log, so the loop either rests
-- or exhausts fuel.
runBus :: Int -> [Seat] -> [Post] -> BusResult
runBus fuel seats0 script = go fuel seats0 (stamp 0 script) []
  where
    stamp _ [] = []
    stamp n (p : ps) = Logged n p : stamp (n + 1) ps

    go f seats lg replies
      | all (rested lg) seats = BusResult lg (reverse replies) seats True
      | f <= 0 = BusResult lg (reverse replies) seats False
      | otherwise =
          let (seats', lg', replies') = drainPass seats lg replies
           in go (f - 1) seats' lg' replies'

    rested lg s = cursor (seatState s) >= length lg

    drainPass seats lg replies =
      foldl
        (\(ss, lg', as) seat -> let (s', lg'', as') = drainSeat lg' as seat in (ss ++ [s'], lg'', as'))
        ([], lg, replies)
        seats

    drainSeat lg replies seat =
      let pending = drop (cursor (seatState seat)) lg
       in foldl deliver (seat, lg, replies) pending
      where
        deliver (s, lg', as) l =
          let (st', reply) = turn (seatRepl s) (seatState s, l)
           in case reply of
                Just p ->
                  let l' = Logged (length lg') p
                   in (s {seatState = st'}, lg' ++ [l'], (seatName s, reply) : as)
                Nothing -> (s {seatState = st'}, lg', (seatName s, reply) : as)
