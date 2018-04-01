
## Fabio - A Stateless Load Balancer
> at Hashiconf EU 2016
> by Frank Schroeder (fschroeder@ebay.com / @magiconair)

[github](https://github.com/eBay/fabio)


> Service already know which routes to accept, just push to service registry.

```

  [Firewall]
      |
      |
  [./fabio]---->[consul]
      |   \ ____/' /'
      |   ,X      /
      |, /  \,   /
    [foo]   [bar]

```

> Consul doesn't support free-form metadat, so `tags` used
> use `urlprefix-host/path` tag

---

### Config

> sample of config language that Fabio generates

* add/edit/delete routes to fix broken deployments

```
route add svc-a /foo http://1.1.1.1:5000/
route add svc-b /bar http://2.2.2.2:6000/
route add svc-c i.co/css http://3.3.3.3:7000/

## can amend broken routing table if needed
route del svc-a
route add svc-a /fooz http://1.1.1.1:5000/
```

* dynamic traffic shaping, can be used for Canary releases

```
## send 5% traffic to red version svc-a
route weight svc-a /foo weight 0.05 tags "red"
```

---

### v1.2rc1

* cmd line args

> all properties can be specified in config file, as env vars or on cmd line

```
proxy_addr=:9999 ./fabio
FABIO_PROXY_ADDR=:9999 ./fabio
./fabio -proxy.addr=:9999
```


* certificate sources

> fabio reads PEM certs for TLS and client cert auth from following sources
> * file (single cert, no refresh)
> * path (multiple cert, periodic refresh)
> * HTTP Server (multiple certs, periodic refresh)
> * Consul KV (multiple certs, auto-refresh)
> * Vault (multiple certs, periodic refresh)


* vault support

> running it with Vault support providing certificate path for polling

```
### adding cert to Vault
# vault write secret/fabio/certs/fabiolb.io cert=@fabio-server-cert.pem key=@fabio-server-key.pem

VAULT_ADDR='http://127.0.0.1:8200' VAULT_TOKEN=7654321 ./fabio -proxy.cs 'ca=certs;type=vault;cert=secret/fabio/certs;refresh=1s' -proxy.addr ':999;cs=certs' -proxy.strategy rr
```


* SNI support

> Server Name Identification support will enable multiple Domain Name resolution at same port to respective backend service.

---

### What's next

* support > 65k outbound connections [github:fabio#102](https://github.com/eBay/fabio/issues/102)

> for enabling it to run in front of several long running websocket connections


* refactor `urlprefix-tag` [github:fabio#111](https://github.com/eBay/fabio/issues/111)

> inflexible, limited to URIs, switch to `fabio key=value key=value` like dns metadata


* support for additional backends (etcd/mesos/docker/swarm/gcp/k8...)

> for multiple backends
> mesos/marathon  : registry + ZK
> docker API      : registry only
> docker swarm    : registry + KV store
> kubernetes      : registry + etcd
> gcp             : registry only
> skyDNS          : registry only

> to maintain
> * single binary
> * zero-conf
> * manual overrides

---

Step towards multiple backends
> * API to push routes into consul
> * add more service discovery modules
> * support different KV stores

---

In prod since Sed'2015 serving

* Kijiji Italy (1200 req/sec)
* Marktplaats (8k req/sec)
* Admarkt (NL,DK,CA) (5k req/sec)

---
---
