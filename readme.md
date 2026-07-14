# circuits-repl

Repl primitive for the circuits ecosystem: **commit / emit dual**, turn,
channel, session — plus the shared `Cursor` over append-only logs.

```haskell
-- dual ends (same object type — Queue dual)
replCommit :: Repl -> [Text] -> IO ()   -- write TO the agent
replEmit   :: Repl -> IO [Text]         -- read FROM the agent
endsRepl   :: Repl -> (Commit IO [Text], Emit IO [Text])
```

Backends: FIFO, PTY, inject, Hermes session JSON (`replOpenHermes`),
MusterRepl via Comm (`openMusterRepl` / `attachMusterRepl` — identity in
`ChannelConfig`, self-echo filtered on emit).

Concrete transports (TCP/WebSocket, timers) live in **`circuits-io`**.

Formerly the `cursor` package; expanded to own the free REPL surface.
