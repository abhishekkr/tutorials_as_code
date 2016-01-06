## Rocking TimeSeries boat with C, Haskell and ClojureScript
by @ifesdjeen , Alex Petrov (Instana) 
at Polyconf 2015

Data
> * Monotonically increasing primary key
> * write-only data
> * range queries (majority)
> * rolling aggregates

Common regime
> * LevelDB backend
> * Tight Combination of Haskell and C (to work on LevelDB from Haskell)
> * Optimising space, reads
> * Flexible aggregates
> * Parallel queries

---

### Querying

* 3-step aggregate?
> * Local aggregate (parallel)
> * Append results of local aggregate (Monoids for data operation)
> * Combine by co-ordinator node to then send response of query

Break queries, parallel aggregate, combine aggregates and transform result.
Can optimize further using Binary Data Format but have impact on Schema Migration.

* snapshots
> * Snapshot Consensus
> * Rolling CRC of data
> * Async, No quorum for snapshot reads
> * Parallel reads from snapshotted data

---

### Writing

Primary-key as <timestampID><sequenceID>.

SequenceID
> * avoid timestamp resolution collisions
> * ensure sub-resolution order
> * snapshot the data on overflow or timeout
> * ensures idempotence

Using RangeTables to enable parallel write and read.

---

### using LevelDB

* Stream Fusion for reading data from LevelDB
> Read Paper called "Stream Fusion - From Lists to Stream to Nothing at All"
> by 'Duncan Coutts', 'Roman Leschinskiy' and 'Don Stewart'
> It's available in LevelDB's binding for Haskell.
> Basic idea is there is Stream and Step.
> Step is either yielding the next item and preserving the state, skipping the iterator or closing the iteration.
> Stream takes the Step and yield the next step.

---

Good to have

> * Lightweight data format
> * Partial Decoding of data
> * Parallel queries
> * Composable, extendable query system
> * Lightweight Consensus

---
