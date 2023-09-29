
## Redis Data Structures

> At root, its a key-val in-mem db written in C. Also allows memory snapshots as AOF (or RDB) for persistence.

### Basic Value Types

* Primitve `string` type. If required data structure ain't supported by Redis, can always just serialize and use this. Max length 512MB.

> Bit-oriented op are supported on Redis strings. So, for 512MB it can manipulate 2^32 bits. Eg: usage like tracking up/down states of million sensors in a simple bitmapped string.

* Hashes for shallow objects.

* Lists with items sorted by insertion order. Implemented with LinkedLists, so good for Queues but positional index is slower (better suited to Sorted Sets).

* Sets, only store unique elements & do allow union/intersection/other-op.

* Sorted Sets, ordered on (explicit) score & lexical order. `O(N)` for add/del.

* GeoSpatial Index Support based on Sorted Set, with score based on Geohash algorithm.

#### Probabilistic

* Hyperloglogs to get quick estimate of unique value in a huge dataset.


#### Streams

* This log data-structure is append-only table, allowing multiple consumers to track event data.


---
