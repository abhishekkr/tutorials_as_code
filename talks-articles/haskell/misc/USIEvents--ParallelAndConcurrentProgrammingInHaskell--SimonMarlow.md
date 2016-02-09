## Parallel and Concurrent Programming in Haskell

> at USI Events 2013
> by Simon Marlow


[slides](http://community.haskell.org/~simonmar/USI2013.pdf)

### Haskell's Philosophy

Give write tool for the job

* Multiple Parallel APIs
> Parallel annotations, dataflow, parallel arrays, GPUs

* Concurrency with various synchronization APIs
>  Threads, MVar, transactional memory

---
```
  .___________________________________________________________.
  |                             |                             |
  ._____________________________|_____________________________.
  |                             |                             |
  | * multi-core programming    | * multi-threaded programming|
  |                             |                             |
  | * usually deterministic     | * non-deterministic         |
  |                             |                             |
  | * declarative               | * imperative                |
  |                             |                             |
  |                             |                             |
  |        [works like]         |     [works like]            |
  |                             |                             |
  | * Image Manipulation        | * Webserver                 |
  |                             |                             |
  | * Machine Learning          | * GUI                       |
  |                             |                             |
  | * db joins                  | * chat server               |
  |                             |                             |
  | * spreadsheet calculation   | * telephone exchange        |
  ._____________________________|_____________________________.
```

---

### Parallel Haskell

> can use tools like [ThreadScope](https://wiki.haskell.org/ThreadScope) for performance profiling of parallel haskell programs.

#### Parallel Haskell: Strategies

* Simple map over list
```
Prelude> let somelist = [1,10,100,1000,10000,100000,1000000,10000000]
Prelude> let plusOne x = x + 1
Prelude> map plusOne somelist 
[2,11,101,1001,10001,100001,1000001,10000001]
```

* Running same with Parallel Strategy
```
Prelude> let somelist = [1,10,100,1000,10000,100000,1000000,10000000]
Prelude> let plusOne x = x + 1

Prelude> import Control.Parallel
Prelude Control.Parallel> import Control.Parallel.Strategies
Prelude Control.Parallel Control.Parallel.Strategies> 

Prelude Control.Parallel Control.Parallel.Strategies>  map plusOne somelist `using` parList rseq
[2,11,101,1001,10001,100001,1000001,10000001]

Prelude Control.Parallel Control.Parallel.Strategies> 
```
might need
```
cabal install parallel
```
It's deterministic on any number of cores given Haskell being pure.

> Parallelism here is modular, with unchanged core logic.
> Like an annotation. 'Cuz of Lazy Evaluation.

---

#### Parallel Haskell: The Par Monad

[details chapter](http://chimera.labs.oreilly.com/books/1230000000929/ch04.html)

> need 'Control.Monad.Par'
```
cabal install monad-par
```

* Dataflow Parallelism, program dataflow directly.
* Doesn't give similar modularity guarantees as with Strategies.
* Direct control over granularity and dependencies.
* Has more imperative flavor, still deterministic.

---

#### Parallel Haskell: Repa

Narrow focus on parallelizing array computation.

> Programming model for GPUs shares a lot with REPA.

---

### Concurrent Haskell

> Problem: How to build a tree of threads such that failures are never ignored not threads orphaned.

#### Concurrency Basics

* 'forkIO' creates a new thread to run the IO ()

* Threads are really lightweight in haskell, very little cost when blocked. Good for modularity of interaction. Use them.

* 'MVar' is basic communication primitive in Haskell, 'putMVar' blocks when 'MVar' is full.

* e.g. downloading URLs concurrently
```
getURL :: String -> IO String

do
    m1 <- newEmptyMVar
    forkIO (do
        r <- getURL "http://www.google.com"
        putMVar m1 r)
    m2 <- newEmptyMVar
    forkIO (do
        r <- getURL "http://www.bing.com"
        putMVar m2 r)
    r1 <- takeMVar m1
    r2 <- takeMVar m2
```

* Abstracting common pattern, 'Async'
```
newtype Async a = Async (MVar a)    --  is a new abstract type containing just one MVar
async :: IO a -> IO (Async a)       --  async creates the MVar and forks the thread
wait :: Async a -> IO a
```

* Using our new abstraction
```
do
  a1 <- async (getURL "http://www.google.com")

  a2 <- async (getURL "http://www.google.com")

  r1 <- wait a1
  r2 <- wait a2
```
> If async action fails, exception is raised in each child thread which terminates the thread.
> Main thread deadlocks, waiting for the results.

* To propagate error to the parent
```
newtype Async a = Async (MVar (Either SomeException a))   -- result includes possibility of Exception

async :: IO a -> IO (Async a)

wait :: Async a -> IO a   -- wait re-throws the exception in the calling thread
```

---

### Cancellation

* Cancellation in Haskell via throwing an async exception to thread. Handled like normal exception.
```
throwTo :: Exception e => ThreadId -> e -> IO ()
```

* Add cancellation to our async API by adding 'cancel' operation defined using 'throwTo'
```
cancel :: Async a -> IO ()
```

Cancelling downloads
```
do
  a1 <- async (getURL "http://www.google.com")
  a2 <- async (getURL "http://www.google.com")

  async (forever $ do
          c <- getChar
          when (c == 'q') (do cancel a1; cancel c2)
        )

  r1 <- wait a1
  putStrLn "r1"
  r2 <- wait a2
  putStrLn "r2"

```

* Revisiting earlier way, 'a2' continues as zombie if 'a1' raises exception and re-raised by 'wait a1'
> we need a way to automatically cancel 'a2' if 'a1' fails
```
withAsync :: IO a -> (Async a -> IO b) -> IO b    -- if the scope returns or fails, async cancelled
```

* no zombies code
```
do
  withAsync (getURL "http://www.google.com")  $ \a1 -> do
  withAsync (getURL "http://www.google.com")  $ \a2 -> do

  r1 <- wait a1
  r2 <- wait a2

-- here if parent or 'a1' dies, all die; but if 'a2' dies 'a1' still wait
-- need a symmetric tree
```

* symmetric exception propagation, 'waitBoth'
> doing this with 'MVar' need 2 extra threads
> using Software Transactional Memory (STM), can do this without extra threads
```
concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right =
  withAsync left  $ \a ->
  withAsync right $ \b ->   -- runs 2 IO operations at same time, and
  waitBoth a b              -- returns a pair of the results

-- either throwing exception cancels both and exceptions raised concurrently
-- no exceptions lost, no zombies

(r1, r2) <- concurrently
              (getURL "http://www.google.com")
              (getURL "http://www.bing.com")
```

* we can use to build higher-level operations
```
pages <- mapConcurrently getURL
          ["http://www.google.com",
           "http://www.google.co.in",
           "http://www.google.co.jp",
           "http://www.yahoo.com",
           "http://www.bing.com",
           "http://www.duckduckgo.com"
          ]
```

---

### dual to 'concurrently', 'race'

Returns the result of first IO action to complete, cancelling others.
Either IO action's exception re-thrown by 'race
```
race :: IO a -> IO b -> IO (Either a b)
```

Need variant of 'waitBoth', 'waitEither'
```
waitEither :: Async a -> Async b -> IO (Either a b)
```

* to see which URL downloads first
```
race1 :: IO a -> IO a -> IO a   -- variant of race


sites = ["http://www.google.com",
           "http://www.yahoo.com",
           "http://www.bing.com",
           "http://www.duckduckgo.com"
          ]


main = do
  first <- foldr1 race1 (map timeDownload sites)
  printf "%s was first\n" first
```

---
---
