# circuits-repl

An agent is a repl loop: read, evaluate, print, loop.

circuits-repl builds that spec on circuits
[Body](../circuits/src/Circuit/Machine.hs) — factored commit and emit
legs, speech acts with structural silence (`Said o | StandDown`), race
and await as seat compositions, and a free-agent-like mock bus as the
first oracle subject.

```bash
cabal build all
cabal run circuits-repl-axioma -- all
```

The design card lives at ~/coffee/loom/repl.md.
