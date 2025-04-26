
# Kafka Performance Tuning Tips

## Producer

* `request.required.acks` once message is sent to broker, how long to wait

> `0` - producer never waits for ACK from broker, lowest latency at cost of durability
> `1` - producer gets ACK when leader replica gets data
> `-1` - producer gets ACK when all in-sync replicas got data


* use `async` producers to batch messages by using `producer.type=1`

> `queue.buffer.max.ms` for window of batch message
> `batch.num.messages` for count of messages batched


* for large message rely on `compression`, but do consider CPU load

* for very huge blobs rather use Kafka to File and sharing File


* proper different timeouts and retries

> `request.timeout.ms` for broker to try required ACK
> amount of time to wait before dropping message

---

## Broker

* for more parallel processing, plan for higher number of partitions

> but more partitions can lead to latency and more resource usage


* keep monitoring on partition load distribution and do re-assignment to new brokers when needed


* some configs

> `num.io.threads` number of I/O threads that server uses for executing requests
> `num.partitions` number of partitions
> `log.flush.interval.messages` number of messages written to a log parition before fsync forced
> [config options](https://kafka.apache.org/08/configuration.html)

---

## Consumer

* max number for a topic be equal to number of partitions


* just adding more consumer groups does not affect performance


---
