
## Docker Network Performance in Public Cloud

> at Contaner Conf LDN 2015 by Arjan Schaa

### Tools

* [qperf](https://linux.die.net/man/1/qperf)

```
## for qperf server
docker run -dti -p 4000:4000 -p 4001:4001 arjanschaaf/centos-qperf -lp 4000

## for qperf client
docker run -ti --rm arjanschaaf/centos-qperf <IPAddress> -lp 4000 -ip 4001 tcp_bw tcp_lat
```


* [iperf3](https://github.com/esnet/iperf)

```
## for iperf3 server
docker run -dti -p 5201:5201 -p 4001:4001 arjanschaaf/centos-qperf -lp 4000

## for iperf3 client
docker run -ti --rm arjanschaaf/centos-iperf3 -c <IPAddress> -t 300 -P 128
```

---
---
