
#### Arista 3 - Demo

[source](https://youtu.be/nubBlEplCEQ?t=395)

```
show interfaces

### EOS also has 'bash' command
### can run own linux services on top of it; like 'ntp', 'sflow' with some or no tweak
```

for HA redundant network design
* 2 boxes `MLAG` (Multi-chassis Link AGgregation) together can coordinate and work together.
* Leaf nodes can be `MLAG` pairs
* Switches can be part of a group, then subgroups can be devided

```
## ssh to switch

## response of interface enabled with vmtracer
show vmtracer interface

show vmtracer vm

## can connect to a group of switches using 'session' command

## can create dynamic VLAN and assign VMs
```

##### Upgrading Switches in MLAG keeping machine online

* Use `MLAG` stable switch-over to take out A-side or B-side of switch but maintain half-connectivity. The switches will boot with newer configuration and reconnect to `MLAG domain`

```
## to check s/w version
show version | grep software

## blow away serve config, to simulate what happens on connecting a new box straight from manufacturing
## forcing 'zero-touch provisioning'
write erase

## make the switch restart
reload power now


```

* make sure command `reload` and similar are only available to restrictied users not `group: all`

* when switch reboot in tasks like above it boots back in `zero-touch provisioning` mode

> instead of booting up and connecting to a VLAN
> boot up, detect no config, put every interface as router interface
> then DHCP from every interface looking for boot script, continue till get response
> then download, provision and reload
> recommend using `HTTP` (instead of `*FTP`)
> HTTP lets push metadata, while pushing boot-scripts can also push data (like switch MAC address, serial number, OS, device type, LLDP - Link Layer Discovery Protocol neighbours)

* can do awesome stuff with these `boot-scripts` with linux base
> can put a `curl` to download extensions (OpenVPN client), make it connect to from it's management network to connect back to your operation center

##### Advanced Event Monitoring

* every change gets logged in a rotating buffer

```
## current route set-up
show ip route

## show all routing changes that have gone in place
## may-be your neighbour adverised some route and withdrew it later
show event-monitor route

## event-monitor can be directly queries in it's sqlite interface
event-monitor interact
```

---
---
