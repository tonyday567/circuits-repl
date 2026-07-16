# circuits-repl

Repl primitive for the circuits ecosystem: **commit / emit dual**, turn,
channel, session — plus the shared `Cursor` over append-only logs.

```haskell
-- process token + free In/Out (open)
replCommit :: Repl -> [Text] -> IO ()   -- write TO the agent
replEmit   :: Repl -> IO [Text]         -- read FROM the agent
openRepl   :: Repl -> (Out (Kleisli IO) (,) [Text], In (Kleisli IO) (,) [Text])
-- unit plug: runOut inR outU / runIn outR inU  with openK ()
```

Backends: FIFO, PTY, inject, MusterRepl via Comm (`openMusterRepl` /
`attachMusterRepl` — identity in `ChannelConfig`, self-echo filtered on emit).

Concrete transports (TCP/WebSocket, timers) live in **`circuits-io`**.

Formerly the `cursor` package; expanded to own the free REPL surface.
