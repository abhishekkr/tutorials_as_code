
> rhel-base machines provide now with `firewalld`

> at LinuxFest NW 2014
> by, Gary Smith (EMSL)

## IPTables Tips and Tricks
> more than just ACCEPT and DROP

### Scenario: Avoid Locking Yourself Out

* Tip#1 Take Backup of IPTables cfg before you ever start

```
BACKUP_IPTABLE="/opt/backup/working-iptables="`date +%F`
iptables-save > $BACKUP_IPTABLE

ln -sf $BACKUP_IPTABLE /opt/backup/latest-iptable

## then can restore using iptables-restore
```

> have `cron` configured to restore `latest-iptable` every 10minutes, and backup existing as `changed-iptable`


* Tip#2 Have an IPMI/KVM console ready
> connect to IPMI port if it's physical node
> if it's VM, log-in to Console session


* Tip#3 Put specific rules at top and generic at bottom
> * so something like allowing SSH from a Jumpbox from a single NIC at very top
> * whitelist your IP at the top for all; remember `-I`inserts it as first rule and `-A` appends


* Tip#6 Know what your policy does
> draw a flowchart if you must

---

### Scenario: Setting up Workstation with Restrictive Policy

* Tip#1 Set default policy as DROP
> then Whitelist only what you need
> but something to take care

```
## accept related/established connections
-I INPUT 1 -m state --state RELATED,ESTABLISHED -j ACCEPT
-I OUTPUT 1 -m state --state RELATED,ESTABLISHED -j ACCEPT

## allow all on Loopback
-A INPUT -i lo -j ACCEPT
-A OUTPUT -o lo -j ACCEPT

## allow outbound DHCP request
-A output -o eth0 -p udp --dport 67:68  --sport 67:68 -j ACCEPT

## allow inbound ssh
-A INPUT -i eth0 -p tcp -m tcp --dport 22 -m state --state NEW -j ACCEPT

## allow outbound SMTP
-A OUTPUT -i eth0 -p tcp -m tcp --dport 25 -m state --state NEW -j ACCEPT

## allow outbound DNS
-A OUTPUT -i eth0 -p udp -m udp --dport 53 -j ACCEPT

## allow outbound PING
-A OUTPUT -i eth0 -p icmp -j ACCEPT

## allow outbound NTP
-A OUTPUT -i eth0 -p udp --dport 123 --sport 123 -j ACCEPT

## outbound HTTP/HTTPS
-A OUTPUT -i eth0 -p tcp -m tcp --dport 80 -m state --state NEW -j ACCEPT
-A OUTPUT -i eth0 -p tcp -m tcp --dport 443 -m state --state NEW -j ACCEPT

## default drop policy
*filter
:INPUT DROP [0:0]
:FORWARD DROP [0:0]
:OUTPUT DROP [0:0]

COMMIT
```

---

### Scenario: Block access to a portal say `Facebook.com`

* get block of address for that domain

```
host -t a $DOMAIN_NAME
```

* convert range to CIDR notation, say `192.168.250.0/24`

* block it

```
iptables -A OUTPUT -p tcp -i eth0 -o eth1 \
 -d 192.168.250.0/24 -j DROP
```

---

### Scenario: Block/Allow access for time window

* block outgoing

```
iptables -A OUTPUT -p tcp -m multiport --dport http.https \
 -i eth0 -o eth1 \
 -m time --timestart 12:00 --timestop 13:00 \
 -d 192.168.250.0/24 -j ACCEPT
```

* similar to incoming traffic with `INPUT` identifier

---

### Scenario: Limit connections with IPTables

* limit number of connection to a single IP

```
iptables -A INPUT -p tcp --syn \
  -m multiport --dport 80,443 \
  -m connlimit --connlimit-above 20 \
  -j REJECT  --reject-with-tcp-reset
```


* limit number of connection in a time window

```
## there would be bug in below commands, fix it
iptables -A INPUT -p tcp \
  -m multiport --dport 80,443 \
  -m state d--state NEW -m recent --set

iptables -A INPUT -p tcp \
  -m multiport --dport 80,443 \
  -m state d--state NEW -m recent --update --seconds 100
  --hitcount 10 -j DROP
```

---

### Monitor whats happening with `iptables`

* watch on iptables verbose

```
watch -n2 'iptables -nvL | grep -v -E "0\s*0"'
```


* a [perl script](http://www.perlmonks.org/?node_id=513732), more comprehensive display

* `FWReport`, a log parser and reporting tool

* `afterglow` to visualize iptable logs, also `psad` and `graphviz`

---
---
