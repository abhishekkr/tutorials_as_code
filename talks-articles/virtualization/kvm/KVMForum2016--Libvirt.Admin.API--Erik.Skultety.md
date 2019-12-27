
## Libvirt Admin API - A Different Kind of Management for libvirt

> by, Erik Skultety

[wiki](http://wiki.libvirt.org)

Virtualization toolset enabling utilize capability of underlying library.

Has a `C Library`, `libvirtd daemon as backend` and a `virsh cli frontend`.

```
  [virsh]     [virt-manager]    [OpenStack]    [oVirt]
     '|'               '|'            '|'         '|'
      '-----------------'--------------'-----------'
                            |,
                         [libvirt] => a middleware layer
                            '|'
      ,------------------------------------------,
      |,          |,       |,         |,         |,
  [Qemu/KVM]    [LXC]    [ESX]    [OpenVZ]     [Xen]

```

Provides secure remote management of `virtual machines`, `virtual networks`, `storage`.

---

### Libvirtd

> means for remote management of VM, stores VM's state safely

> originally required restart on every config change, persistent config only in `libvirtd.conf`


#### Libvirtd-admin

It's a new set of APIs in an independent library.
Binary `virt-admin` is packaged together, it's based on virsh command.

Allows live configuration of daemon at runtime.
Provides monitoring (threadpool monitoring, client monitoring).

#### Libvirtd-admin ~ Thread Pool Management

Server load might cause all workers thread to be occupied.Next request would be blocked.
Identify which one of libvirtd's server is hitting limit on active workers and increase worker threads.

```
# virt-admin srv-list
```

For, what is a `Server`?

* detour to libvirt internals below

```
        public    driver               public    driver
        API\     /API                  API \   / API
             ,----------------,             ,----------------,
             |  | libvirt     |             |  | libvirt     |
             |  |    [ QEMU ] |             |  | ,->[ QEMU ] |
             |  |    [  LXC ] |             |  | |  [  LXC ] |
      ,------>--\    [  Xen ] |             |  |/   [  Xen ] |
      |      |  |\   [OpenVZ] |          ,-->--/    [OpenVZ] |
      |      |  | |  [  test] |          |  |  |    [  test] |
      |      |  | '->[remote]-|-,        |  |  |    [remote] |
      |      '----------------' |        |  '----------------'
      |                         |        |
      |                         \        '------,
      |qemu://host/system        \  RPC         |
      '-------[APPLICATION]       '----------->[libvirtd]

```

* libvirtd internals

```
  ,------------------,
  |server admin (new)| 
  |[server UNIX ]    |
  |[[root-only] ]    |
  |[[unix sock] ]    |
  '------------------'                              ,---------------------,
     '|'                                            |drivers              |
      |              [libvirtd]-------------------->|    [storage     ]   |--->
      |                  '|'                        |    [network     ]   |
      |                   |                         |    [hypervisors ]   |
    ,----------------------------------------------,'---------------------'
   | server libvirtd                               |                                     
   | [=thread=pool==============================]  |                                          
   |     '|           '|        '|          '|     |                                          
   |      |            |         |           |     |                                          
   | [service Unix][svc Unix ][svc TCP ][svc TLS ] |                                          
   | [[RO Unix ]  ][[RW Unix]][[TCP/IP]][[TCP/IP]] |                                          
   | [[Socket  ]  ][[Socket ]][[Socket]][[Socket]] |                                          
   '-----------------------------------------------'

```

* can get/set threadpool runtime info

```
virt-admin srv-threadpool-info libvirtd
virt-admin srv-threadpool-set libvirtd
```


#### Libvirtd Underneath Jobs, when request arrives

* event loop extracts raw data from file descriptor

* deserializes message header to create job for worker threads (decoder header + message payload)

* worker thread deserializes message payload to extract procedure arguments

* dispatch registered method associated with RPC

* serialize results and send a reply back to client


#### Libvirtd-admin ~ Client Management

* how many clients can be connected, setting the limit

```
virt-admin srv-clients-info libvirtd
virt-admin srv-clients-set libvirtd --max-clients --max-unauth-clients
```

* policy enforcement by 3rd party applications

* list all connections, force disconnection for it

```
virt-admin srv-clients-list libvirtd

virt-admin client-disconnected libvirtd <ID>
```

* provide detailed info about connected clients like IP (PID, GID, UID), timestamp, authentication, readonly connection

```
virt-admin client-info libvirtd <ID>
```


#### Libvirt-admin ~ Logging

Daemon or domains enter an invalid state, but

* debugging is not enabled

* changing the config requires daemon restart

* we want to inspect current state

* modify the logging settings (logging level and outputs) on the fly, provide a custom set of filters

```
virt-admin dmn-log-info libvirtd

virt-admin dmn-log-define libvirtd --level --filters --outputs
```

---
---
