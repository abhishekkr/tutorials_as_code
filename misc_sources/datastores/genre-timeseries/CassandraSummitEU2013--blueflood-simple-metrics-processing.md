## Blueflood
### Simple Metrics Processing
> Cassandra Summit EU 2013
> by @gdusbabek

Freenode: #blueflood
http://blueflood.io
https://github.com/rackerlabs/blueflood

#### Column Families

METRIC
* One full-resolution table
* 5 Roll-up One per Granularity (5m, 20m, 60m, 240m, 1440m)
* One row per metric
* Locator is key
* No bucketing (maybe in future for high frequncy)

METADATA
* One row per metric

ROLLUP STATE
* Nasty map for tracking Shard state

ACTIVE METRIC
* Shard to list of Locators

STRING & BOOLEAN Metrics
* Don't change very often, only updated when changed
* Plumbing keeps old value in memory

---

#### Library

* Ingestion

```
insert_metrics(list<metric>)

update_state(shard, granularity, slot) // SLOT == Bucket-of-Time
```

* Roll-up (support bulk-ops outside service)
```
get_active_locators(shard)

get_state(shard, granularity, slot)

get_metrics(from, to, locator, granularity)

write_rollup(list<rollup>)

update_state(shard, granularity, slot) // SLOT == Bucket-of-Time
```
Roll-ups contain [count, min, max, mean, variance]

* Query
```
get_data(from, to, granularity)

get_data(from, to, num_points) //decides granularity for you based on required points
```


#### Roll-up

* When
> if older than 'N seconds' or not refreshed in 'M seconds'

* What about late date
> tolerate till 24-hrs


#### Ingestion

* API Endpoints


#### Scale

* Ingestion scales linearly for Cassandra acting as bottleneck
* Roll-up scale almost linearly by spreading out shard ownership


---
---
