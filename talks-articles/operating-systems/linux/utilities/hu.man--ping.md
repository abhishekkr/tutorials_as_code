
## Ping

We've all used `ping` sometime to see if we can reach a particular network address, to discover our connectivity or its reachability.

Just to know some switch would be more powerful

* just view summary with `-q` option


* if have multiple interface, check connectivity from specific nic using `-I`

```
ping -Ieno01 $FQDN_OR_IPADRESS
```


* only send limited ping packets with `-c`

```
ping -c2 8.8.8.8
```


* ping a broadcast address with `-b`

```
ping -b 192.168.1.0
```


* if you are collecting ping result as data, collect it with timestamp using `-D`

```
ping -D $FQDN_OR_IPADRESS
```


* report outstanding requests before sending next with `-O`


* manage interval between sent ping packets, default is 1sec

```
ping -i 5 $FQDN_OR_IPADRESS
```


* manage IP Time-to-Live

```
ping -t $TTL
```


* wait for response only for certain seconds using `-W`


* wait for ping to finish only for certain seconds using `-w`


* make ping record route

```
$ ping -R 192.168.0.1
PING 192.168.0.1 (192.168.0.1) 56(124) bytes of data.
64 bytes from 192.168.0.1: icmp_seq=1 ttl=64 time=7.75 ms
RR:   192.168.0.2
  192.168.0.2
```

---
---
