
## mtr

a network diagnosting tool, to replace tools such as ping and traceroute, purposely sending packets with low TTL to identify bad or overloaded link

* `-4` ipv4 only and `-6` ipv6 only

* `-r` and `-c` to enable report mode and perform it for fixed count, using `-w` instead of `-r` gets you hostname as well

```
$ mtr -r -c2 8.8.8.8
Start: Wed Apr 19 13:55:43 2017
HOST: some.host.name              Loss%   Snt   Last   Avg  Best  Wrst StDev
  1.|-- whatever.home.router       0.0%     2  127.0  66.0   5.0 127.0  86.3
  2.|-- 172.23.100.201             0.0%     2  171.9 203.9 171.9 235.9  45.3
  3.|-- ???                       100.0     2    0.0   0.0   0.0   0.0   0.0
```

* different output formats
> * `-l` : raw, optimal for archival
> * `-x` : xml
> * `-c` : csv
> * `-g` : gtk
> * `-t` : curses

* different field formats
> * `-n` : doesn't change IP to DNS
> * `-b` : show IP and DNS both
> * `-e` : include mpls information from ICMP headers

* different request tweaks
> * `-s PACKETSIZE` : set packet size in bytes used for probing, inclusive of IP and ICMP headers
> * `-a ADDRESS` : bind source address, doesn't apply to DNS requests
> * `-f NUM` : first TTL to start with, default is 1
> * `-m NUM` : maximum TTL, default is 30
> * `-u` : use UDP instead of ICMP echo
> * `-T` : use TCP SYN instead of ICMP echo; can use `-P` to provide target port number as well; and `-Z sec` for socket timeout

* environment variable `MTR_OPTIONS` can be assigned for default set of switches required

---
---
