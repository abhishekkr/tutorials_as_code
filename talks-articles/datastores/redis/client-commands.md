
## Redis Cli Commands' Example

```
127.0.0.1:6379> SET cache-accounts:alice:now-watching "SERIALIZED_DATA"
OK
127.0.0.1:6379> GET cache-accounts:alice:now-watching
"SERIALIZED_DATA"
127.0.0.1:6379> SET cache-accounts:alice:recommendations "SERIALIZED_DATA" EX 60000
OK
127.0.0.1:6379> GET cache-accounts:alice:recommendations
"SERIALIZED_DATA"
127.0.0.1:6379> TTL cache-accounts:alice:recommendations
(integer) 59962

127.0.0.1:6379> HMSET cache-accounts:alice:session name "Alice" email "alice@e.com" author "token" credits "250"
OK
127.0.0.1:6379> HMGET cache-accounts:alice:session name
1) "Alice"
127.0.0.1:6379> HINCRBY cache-accounts:alice:session credits 30
(integer) 280
127.0.0.1:6379> HMGET cache-accounts:alice:session name credits
1) "Alice"
2) "280"

127.0.0.1:6379> ZADD topchart:pop 1 finneas 2 stephen 3 duncan 
(integer) 3
127.0.0.1:6379> ZADD topchart:pop 4 t.t.r.
(integer) 1
127.0.0.1:6379> ZREVRANGE topchart:pop 0 -1 WITHSCORES
1) "t.t.r."
2) "4"
3) "duncan"
4) "3"
5) "stephen"
6) "2"
7) "finneas"
8) "1"
127.0.0.1:6379> ZINCRBY topchart:pop 10 finneas
"11"
127.0.0.1:6379> ZREVRANGE topchart:pop 0 -1
1) "finneas"
2) "t.t.r."
3) "duncan"
4) "stephen"
```

---
