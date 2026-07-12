# cursor

Position in an append-only log — *what is new since I last asked?*

Two backends, one type:

| constructor | storage | survives restart |
|-------------|---------|------------------|
| `newMem` | `IORef` | no |
| `newFile` | decimal file (`N\n`) | yes |

```haskell
import Cursor

c <- newMem 0
pollLines c ["a", "b"]       -- ["a","b"]
pollLines c ["a", "b", "c"]  -- ["c"]

c <- newFile ".cursor-alice"
pollFile c "log.md"          -- new lines since last poll
seekEndFile c "log.md"       -- attach at tail
```

Standalone primitive for process harnesses (repl emit) and coordination buses (muster read). Depends only on `base`, `directory`, and `text`.

```
cabal test
cabal-docspec
```
