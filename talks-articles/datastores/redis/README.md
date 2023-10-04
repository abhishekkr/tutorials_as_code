
## Redis

> REmote DIctionary Server

### QuickBytes

> * Safeguard your setup with `requirepass SECRETWORD` in `/etc/redis/redis.conf`; usable as `redis-cli -a SECRETWORD`.
> * For complex regular operations, use Lua scripts to avoid roundtrips of data multiple times.. saving on time & other costs.


### Notes

* [Data Structures](./data-structures.md)

* [Scaling](./scaling.md) for Reads/Writes

* [Redis Modules](./modules.md)

* [Sample CLI Commands](./client-commands.md)

* [Sample Code for Pub-Sub](./redis-pub-sub.py)

* [RedisConf 2016 : Managing Redis with K8s](./RedisConf2016--Managing.Redis.with.K8--Kelsey.Hightower.md)

---
