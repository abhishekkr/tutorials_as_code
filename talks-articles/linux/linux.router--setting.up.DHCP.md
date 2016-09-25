
## Linux Router - on RHEL7
> SSN Network

### Setting DHCP Server

* get `dhcp` package


#### Get the machine a static IP

* `ip a` to check all interfaces available

* in `rhel-base`, required interface config under `/etc/sysconfig/network-scripts/ifcfg-eno007`
> set `BOOTPROTO=static`
> set `ONBOOT=yes`
> set `IPADDR=$WHATEVER_IP`; example 10.100.2.1
> set `NETMASK=$WHATEVER_NETMASK`, example 255.255.255.0

* `ifdown eno007 && ifup eno007 && ip a`, to check it's up


#### Get DHCP Service Working

* Modify DHCP Conf

```/etc/dhcp/dhcpd.conf
# multiple options, RTFM

## get dynamic dns to work
ddns-update-style interim;

## helps woth PXE
allow booting;
allow bootp;

authoritative;

## assuming host can update their hostnames
ignore client-updates;

set vendorclass = option vendor-class-identifier;

subnet 10.100.2.0 netmask 255.255.255.0 {
  interface eno007
  option routers                  10.100.2.1;
  option domain-name-servers      10.100.0.1;
  option domain-name              "example.com";
  option subnet-mask              255.255.255.0
  range                           10.100.2.100 10.100.2.254;
  filename                        "/pxelinux.0"
  default-lease-time              21600;
  max-lease-time                  43200;
  next-server                     10.100.2.1
}
```

*  re-load service dhcp

```
systemctl enable dhcpd
systemctl restart dhcpd
systemctl status dhcpd -l
```

---

### Setting firewalls and NAT

> this tut is not using `firewalld`, no need fo that

```
yum -y install iptables-services iptables-utils

systemctl disable firewalld
systemctl stop firewalld
systemctl enable iptables
systemctl start iptables
```

* `cat /etc/sysconfig/iptables`, can be edited manually or via `system-config-firewall`

* `iptables -t nat -nvL` to see current NAT table

* make it masquerade to enable routing

```
iptables -t nat -A POSTROUTING -o eno007 -j MASQUERADE

iptables-save > /etc/sysconfig/iptables
```


* add following to `/etc/sysconfig/iptables`

```
-A FORWARD -i eno007 -o eno008 -m state --state RELATED,ESTABLISHED -j ACCEPT
-A FORWARD -i eno008 -o eno007 -j ACCEPT
```


* enable ip forwarding

```
echo 'net.ipv4.ip_forward = 1' | sudo tee -a /etc/sysctl.conf

sud sysctl -p
```

---

### Setting SSH and PAM

* create a user

```
useradd bob

usermod -aG wheel bob
```


* in `/etc/pam.d/su`, append following for sudo access just being in `wheel` group

```
auth    sufficient    pam_wheel.so  trust use_uid
```


* `sudo -l -U bob`, might say access to `ALL (ALL)`


* get `policycoreutils-python`, to use semanage

```
semanage -a -t ssh_port_t -p tcp 30717
```


* in `/etc/ssh/sshd.conf`
> update `PORT 30717`
> update `PermitRootLogin no`
> restart `systemctl restart sshd && systemctl status sshd`


* update `iptables` conf for `port 22` to `port 30717`
> `iptables-restore < /etc/sysconfig/iptables`


---
---
