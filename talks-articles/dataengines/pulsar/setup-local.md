
## Setup Pulsar locally

> standalone mode includes Pulsar Broker, nexessary ZooKeeper and BookKeeper components inside a single JVM process

* download binary tarball

```
wget -c https://archive.apache.org/dist/pulsar/pulsar-2.5.0/apache-pulsar-2.5.0-bin.tar.gz

tar xvfz apache-pulsar-2.5.0-bin.tar.gz

cd apache-pulsar-2.5.0
```

#### Pulsar tarball contains

* `bin`	cli tools, such as pulsar and pulsar-admin
* `conf` conf files, for broker, ZK and more.
* `examples` jar file containing Pulsar Functions example
* `lib` jar files used

* `data` data storage directory used by ZooKeeper and BookKeeper
* `instances` artifacts created for Pulsar Functions
* `logs` logs created by the installation

#### Running Pulsar

* starting standalone service

```
bin/pulsar standalone
```

* publishing message

```
bin/pulsar-client produce my-topic --messages "hello-pulsar"
```

* consuming message

```
bin/pulsar-client consume my-topic -s "first-subscription"
```

---
