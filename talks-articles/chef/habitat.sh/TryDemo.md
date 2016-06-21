## Habitat.sh Demo Steps

[source](https://www.habitat.sh/try/)

Learn about Habitat
> * [Tutorials](https://www.habitat.sh/tutorials)
> * [Docs](https://www.habitat.sh/docs)
> * [Community](https://www.habitat.sh/community)
> * [Tutorial - Getting Started](https://www.habitat.sh/tutorials/getting-started-overview/)

---

* habitat uses immutable package with tweakable configuration changes

```
## habitat already go multiple package, can just go ahead and run one
hab start core/redis
```

* services are managed by a [supervisor](https://www.habitat.sh/docs/concepts-supervisor/)

```
## ask supervisor what's configurable in a service
hab sup config core/redis
```

* supervisor handles multiple things
> * reporting what's configurable on a service
> * start a configured service
> more...

* config changes can be applied in multiple ways
> * setting particular environment variable
> * a config file like `config.toml`, or package particular like `reids.conf`
> (maybe, supervisor picks configurable tips from config.toml)

```
## configure service via env variable
HAB_REDIS="tcp-backlog=128" hab start core/redis

## make permanent changes via config file
## change required config in 'config.toml' file
```

* once config file updated, need to roll-out changes to all nodes
> you can change in a single command with each node picking it up

```
## configure service through discovery
hab config apply redis.default 1 /tmp/config.toml --peer 172.17.0.4

## peer to join and apply the new config
```

* habitat places running services into a [Service Group](https://www.habitat.sh/docs/concepts-services/) with standalone topology
> one can explicitly start services in a user-defined group that optionally apply different topologies
> like leader/follower for multiple redis to start in a particular order

```
## setting up a service group leader/follower topology
hab start -t leader core/redis

## needs 3 node for quorum
## Adding services to an existing group
hab start -t leader core/redis --peer 172.17.0.4
hab start -t leader core/redis --peer 172.17.0.4
```

---
---
