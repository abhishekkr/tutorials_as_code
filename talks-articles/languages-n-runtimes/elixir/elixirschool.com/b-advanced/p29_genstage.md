
## GenStage

> sample app at [p29_genstage](./p29_genstage)

* is a spec and computation flow providing constructs to define work pipeline containing independent stages running isolated process

* in a pipeline `[stage1] -> [stage2] -> [stage3]`; `stage1` is a **producer**, `stage2` is **producer-consumer** and `stage3` is a **consumer**

* it's possible to have multiple producers and/or multiple consumer at any stage


### Consumers and Producers

* GenStage spec provides 3 roles, which can be given to any stage

> * `:producer`, a source waiting for requests from consumers and responding with events
>
> * `:producer_consumer`, has a source responding to requests and sink requesting events
>
> * `:consumer`, a consumer sink requesting and receiving events

* `back-pressure` puts onus on producer to not struggle when consumers are busy


### Getting Started

* sample app created using `mix new p29_genstage --sup` displays all 3 role implementations, `gen_stage` gets added to dependencies


### Producer

* `init/1` set initial state and label self as `:producer`, this response is utilized by GenStage to mark role

* `handle_demand/2` defines producer task, mandatory to implement by all producers


### Producer Consumer

* `init/1` serves similar usability here, just role responded is `:producer_consumer`

* `handle_events/3` is another mandatory function to be implemented; receives incoming events, process them and return transformed set

> here 2nd element in `handle_events/3` response is used to pass result downstream; this is the only thing different of this from Consumer as that discards it


### Consumer

* `init/1` response `:consumer` as role; `handle_events/3` response's 2nd element is useless as no events are transmitted


### Putting it all together

* after defining all required stages for a pipeline, add defined module as stages with initial values under `children` list in `P29Genstage.Application.start/2`

* the pipeline can be run as `mix run --no-halt`


### Multiple Producers or Consumers

* to invoke multiple copies of a module as a role with different/same initial values, multiple entries can be made under `children` list in `P29Genstage.Application.start/2` with different `id`

```
children = [
  worker(P29Genstage.Producer, [0]),
  worker(P29Genstage.ProducerConsumer, []),
  worker(P29Genstage.Consumer, [], id: 1),
  worker(P29Genstage.Consumer, [], id: 2)
]
```

* `subscribe_to` field in `init/1` can have multiple modules for multiple producer/consumer


### Use Cases

* ETL pipelines

* Worker queues

* Event Processing pipelines

---
