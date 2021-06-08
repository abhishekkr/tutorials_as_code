
## Bitcask

> A Log-Structured Hash Table for Fast Key/Val Data
>
> by Justin Sheehy & David Smith (with inspiration from Eric Brewer), Basho Technologies


### Goals

Goals when evaluating such storage engines, including:

> * low latency per item read/write
>
> * high throughput, esp. when writing incoming random item stream
>
> * can handle datasets lot larger than RAM w/o degradation
>
> * robust to crash for data & service recovery
>
> * ease to backup/restore
>
> * maintainable source code & data format
>
> * limited impact of heavy traffic & large volume
>
> * license.

---

### Basic Architecture

* Bitcask instance is a `directory`

* Only one OS process will open that Bitcask for writing a given time; like acting as a DB Server

* At any moment, one `file` is `active` in that `directory` for writing by server

* When a `file` meets size threshold; it's rolled over for a new `active` file

* Once a file is closed (any reason; purposefully or server exit), it's Immutable so never written to again

* `active` file is only written by appending; so no disk seek for sequential writes

> format for each K/V entry is simple `[ crc | tstamp | ksz | value_sz | key | value ]`

* After `append` completes, in-mem structure `keydir` is atomically updated

> `keydir` is a hash-table of every `bitcask key` to a fixed-size structure `[file_id | value_sz | value_pos | tstamp]`

* On first overwrite of a `key`; `keydir` gets updated location first so infers correctly and merge process cleans up old value later

* Reading just requires single disk seek; lookup `key` is `keydir` and engine has exact location of latest value

* Merge Process iterates over all Immutable Files producing only latest version data files

* During Merge, also creates a `hint file` next to each data file with `[tstamp | ksz | value_sz | value_pos | key]`

* When a Bitcask gets opened in an ErlangVM, checks if there is another Erlang proess using that Bitcask to share `keydir` with that process

> to open a new Bitcask it scans all data in directory to build new `keydir`; if `hint` file is available it's a preferred scan as quicker

* **the internal `keydir` locking scheme** is one of the hard implementations not covered here

* it does not compress data; as benefit of that is app dependent so shall be managed there

---

### Where at Goals

* low latency per item read/write; with sub-millisec typical median latency

* high throughput of 5K-6K writes/sec on early tests at laptops w/ slow disks for random items

* handle datasets much larger than RAM, tests mentioned dataset 10X RAM on system without any degradation

* data files & commit log being same thing; recovery is trivial with no need of replay

* backup/restore are easy due to immutable data files after rotation

* it is a simple code & data structure

* predicatable behavior under load

---

### API is simple

* `bitcask:open(DirName, Opts) -> BitcaskHandle | {:error, any()}`

* `bitcask:open(DirName) -> BitcaskHandle | {:error, any()}`

* `bitcask:get(BitcaskHandle, Key) -> :not_found | {:ok, value}`

* `bitcask:put(BitcaskHandle, Key, Value) -> :ok | {:error, any()}`

* `bitcask:delete(BitcaskHandle, Key) -> :ok | {:error, any()}`

* `bitcask:list_keys(BitcaskHandle) -> [key] | {:error, any()}`

* `bitcask:fold(BitcaskHandle, Fun, Acc0) -> Acc`

* `bitcask:merge(DirName) -> :ok | {:error, any()}`

* `bitcask:sync(BitcaskHandle) -> :ok`

* `bitcask:close(BitcaskHandle) -> :ok`

---
