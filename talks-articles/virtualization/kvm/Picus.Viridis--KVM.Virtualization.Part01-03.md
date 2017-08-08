
* KVM ~ Kernel-based Virtual Machine.

* Full virtualization solution on Linux x86 kernel w/ virtualization extensions Intel VT and AMD-V.

* Loadable kernel module `kvm.ko` provide core virtualization infrastructure
> then processor specific module `kvm-intel.ko` or `kvm-amd.ko`

* Each virtual machine has private virtualized h/w.

* QEMU achieves near native performance by running guest code directly on host CPU.

* Verify kernel's CPU Virtualization extension

```
egrep 'vmx|svm' /proc/cpuinfo --color

## svm ~ AMD, vms ~ Intel
```

---

### Running KVM instances from Live ISO

* Package required
> * kvm
> * qmeu-kvm
> * libvirtd

* Get a LiveCD ISO

* Create Virtual disk
> create expand-able virtual disk

```
mkdir $KVM_BASE_DIR/vm
cd $KVM_BASE_DIR/vm

qemu-img create -f qcow2 hdd1.qcow2 -o preallocation=off 5G

qemu-img create -f qcow2 hdd1.qcow2 -o preallocation=metadata 5G
```


* Run VM method.1

```
kvm \
  -name livecd-debian \
  -monitor vc \
  -no-quit \
  -m 1024 \
  -usb \
  -drive media=disk,if=ide,index=0,file=hdd1.qcow2,snapshot=off \
  -drive media=disk,if=ide,index=1,file=hdd2.qcow2,snapshot=off \
  -drive media=cdrom,if=ide,index=2,file=debian-live-7.6.0-amd64-kde-desktop.iso
```

> -m is MB RAM
> available `if` under `-drive` are ide, scsi, sd, mtd, floppy, pflash, virtio
> snapshot=off/on, after poweroff vm changes on disk is persisted or lost
> `CTRL+ALT+2` -switch to text console
> `CTRL+ALT+1` -switch to graphic console


* Run VM method.2

```run-debian-vm.sh
#!/bin/bash

command=kvm

parameters=(
  -name debian
  -monitor vc
  -no-quit
  -m 1024
  -usb
  -drive media=disk,if=ide,index=0,file=hdd1.qcow2,snapshot=off
  -drive media=disk,if=ide,index=1,file=hdd2.qcow2,snapshot=off
  -drive media=cdrom,if=ide,index=2,file=debian-live-7.6.0-amd64-kde-desktop.iso
    )

for ((i=0; i<${#parameters[@]}; i++)); do
  command="${command} ${parameters[i]}"
done

eval $command
```


* Run VM method.3
> install `screen` and vnc-viewer

```run-debian-vm-this-way.sh
#!/bin/bash

command=kvm

parameters=(
  -name debian
  -monitor stdio
  -vnc :1
  -m 1024
  -usb
  -drive media=disk,if=ide,index=0,file=hdd1.qcow2,snapshot=off
  -drive media=disk,if=ide,index=1,file=hdd2.qcow2,snapshot=off
  -drive media=cdrom,if=ide,index=2,file=debian-live-7.6.0-amd64-kde-desktop.iso
    )

for ((i=0; i<${#parameters[@]}; i++)); do
  command="${command} ${parameters[i]}"
done

eval $command
```

```
screen -mdS livecd run-debian-vm-this-way.sh

vnc :1
```

---

### Creating Virtual NAT
> without libvirt

* get `vde2` package
> VDE is a virtual switch utility that can connect multiple VMs together

* get virtual network interface for subnet, say `192.168.80.0/24`

```
netstat -r
ifconfig

vde_tunctl -t kvm-nat

ifconfig kvm_nat 192.168.80.1/24 up

ifconfig kvm-nat

netstat -r
ifconfig
```


* get virtual switch for subnet

```
vde_switch -tap kvm-nat -sock /tmp/kvm-nat -mod 666 -daemon
```


* NAT forwarding

```
echo 1 > /proc/sys/net/ipv4/ip_forward

iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE
```


* configuring VM to use this virtual network

```run-debian-vm-this-way-with-virt-nic.sh
...
command=kvm

parameters=(
      ...
      -net nic,macaddr=52:54:00:00:00:05
      -net vde,port=5,sock=/tmp/kvm-nat
      ...
    )
...
```

> configuring nic in VM from 192.168.80.2-254 range

---

### Creating Bridge networking for VMs

* require `vde2` and `bridge-utils` installed

* create virtual interface and switch as before, by name `kvm-br`, then

```
vde_tunctl -t kvm-br
vde_switch -tap kvm-br -sock /tmp/kvm-br -mod 666 -daemon

ifconfig kvm-nat
ifconfig eth0 0 promisc up
ifconfig kvm-br promisc up

brctl addbr br0
brctl addif eth0
brctl addif kvm-br

ifconfig br0 192.168.0.20/24 up
route add default gw 192.168.0.1

brctl show
```

* running VM with it

```run-debian-vm-this-way-with-br0.sh
...
command=kvm

parameters=(
      ...
      -net nic,macaddr=52:54:00:00:00:05
      -net vde,port=5,sock=/tmp/kvm-br
      ...
    )
...
```

---
---
