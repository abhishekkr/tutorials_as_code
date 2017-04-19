
## tcpdump

> uses libpcap to capture packets

```
$ tcpdump -h
tcpdump version 4.7.4
libpcap version 1.7.4
OpenSSL 1.0.2j-fips  26 Sep 2016
Usage: tcpdump [-aAbdDefhHIJKlLnNOpqRStuUvxX#] [ -B size  ] [ -c count  ]
    [ -C file_size  ] [ -E algo:secret  ] [ -F file  ] [ -G seconds  ]
    [ -i interface  ] [ -j tstamptype  ] [ -M secret  ] [ --number  ]
    [ -Q in|out|inout  ]
    [ -r file  ] [ -s snaplen  ] [ --time-stamp-precision precision  ]
    [ --immediate-mode  ] [ -T type  ] [ --version  ] [ -V file  ]
    [ -w file  ] [ -W filecount  ] [ -y datalinktype  ] [ -z command  ]
    [ -Z user  ] [ expression  ]
```

> require sudo

* to check what all interfaces can be captured

```
$ sudo tcpdump -D

1.docker0 [Up, Running]
2.wxp11a0 [Up, Running]
3.tun0 [Up, Running]
5.any (Pseudo-device that captures on all interfaces) [Up, Running]
6.lo [Up, Running, Loopback]
7.virbr0 [Up]
```


* to caputture at all

```
$ sudo tcpdump -i any

## to capture specific number of packets
$ sudo tcpdump -i any -c 5
```


* to avoid reverse hostname resolution `-n`

```
$ sudo tcpdump -i any -c 5 -n
```


* to just capture 96bytes per packet, see what all you want (just headers, all)

```
$ sudo tcpdump -i any -c 5 -n -s96
```


* say capture tcp traffic with destination port 49952, `-t` to avoid timestamp printing on each line

```
$ sudo tcpdump -i any -c 50 -n tcp and dst port 49952 -t
```


* checking all at port80, port53; `-nn` avoids port naming as well

```
yy$ sudo tcpdump -i any port 80 -c 50 -nn -t
$ sudo tcpdump -i any port 80 -c 50 -nn -t
```


* saving pcap

```
$ sudo tcpdump -i any -c 500 -n -w capture.pcap
```


* reading pcap files

```
$ sudo tcpdump -i any -c 5 -n -r capture.pcap
```


* filter expressions

```
## any packet to or from 10.0.0.2
$ sduo tcpdump -i any -n host 10.0.0.2 -c5

## any packet from 10.0.0.2
$ sduo tcpdump -i any -n src host 10.0.0.2 -c5

## any packet between 10.0.0.2 <-> 10.0.0.3
$ sduo tcpdump -i any -n host 10.0.0.2 and host 10.0.0.3 -c5

## any packet from 10.0.0.3 for port 80
$ sduo tcpdump -i any -n src host 10.0.0.3 and port 80 -c5

## any packet from 10.0.0.3 for port 80|443
$ sduo tcpdump -i any -n "src host 10.0.0.3 and (port 80 or port 443)" -c5

## public network heading packets
$ sudo tcpdump -i any -n -c100 "src net 192.168.0.0/16 and not dst net 192.168.0.0/16 and not dst net 10.0.0.0/8 and not dst net 172.0.0.0/8"

## for mac-addesss
sudo tcpdump -i any ether host 00:XX:AA:11:00:DD  -n c100 -e

## any ipv6
sudo tcpdump -i any ip6

## tcp specific with syn flags
sudo tcpdump -i any "tcp[tcpflags] & tcp-syn != 0"

## tcp specific with sreset flags
sudo tcpdump -i any "tcp[tcpflags] & tcp-rst != 0"
```

* `-XX` displays more details in hex/ASCII about packets
* `-A` displays more details in ASCII about packets

* `-v`, `-vv`, `-vvv` with gradual increasing verbosity of packet details

* `-q` minimal packets details

* `-K` to not verify checksums, if done at hardware

* `-ttt` prints timestamp as delta between current and previous dump


* to print all IPv4 HTTP packets to and from port 80, i.e. print only packets that contain data, not, for example, SYN and FIN packets and ACK-only packets.  (IPv6 is left as an exercise for the reader.)

```
$ sudo tcpdump -i any 'tcp port 80 and (((ip[2:2] - ((ip[0]&0xf)<<2)) - ((tcp[12]&0xf0)>>2)) != 0)'

```


* To print IP packets longer than 576 bytes sent through gateway snup:

```

$ sudo tcpdump -i any 'gateway snup and ip[2:2] > 576'
```


* To print all ICMP packets that are not echo requests/replies (i.e., not ping packets):

```
$ sudo tcpdump -i any 'icmp[icmptype] != icmp-echo and icmp[icmptype] != icmp-echoreply'
```


---
---
