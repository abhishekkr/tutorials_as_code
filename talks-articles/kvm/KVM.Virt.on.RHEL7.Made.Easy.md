
> notes from a PDF published by Dell Inc. by same name

## KVM Virtualization in RHEL 7 Made Easy

### Quick Intro

* KVM is linux kernle-module allowing user-space programs access h/w virt features of Intel and AMD processors.

* VMs run as user-space processes.

* (popularly) Uses QEMU for I/O h/w emulation.
> QEMU needs section of its own. A user-space emulator capable of mimicing variety of guest processors on host processors.

* Managed via `libvirt` API and tools
> Some popular tools used here are `virsh`, `virt-install`, `virt-clone`.

* Has concept of `overcommitting`, allocate more than available
> VMs only used what they need, allowing other VMs to use what they need in what's not used.
> Obviously the total amount actually used can't exceed what's available, no unions.

---

### Getting Ready

Traget hardware (or virtualized emulation of it) should support Virtualization Technology at BIOS.

```
grep -c -E 'svm|vmx' /proc/cpuinfo
```
* if above commands output comes ZERO
> you need to enable `Virtualization Technology` in `Processor Settings` under BIOS
> if can't, get the hell out of here

* here `vmx` points for Interl and `svm` for AMD

* install required packages, 'rhel-base' reference

```
sudo yum -y install qemu-kvm libvirt libvirt-python libguestfs-tools virt-install
```

* start+enable `libvirtd` service

```
sudo systemctl enable libvirtd && \
  sudo systemctl start libvirtd
```

* make sure `kvm` and `kvm_intel` (or whatever processor) kernel modules are loaded

```
## following should output '1', else need to load them manually
lsmod | awk '{print $1}' | grep -c '^kvm_intel$'
lsmod | awk '{print $1}' | grep -c '^kvm$'
```

---

### Networking

* follow [libvirtd bridge interfaces](../linux/Libvirtd.Networking.md) guide instead of this

* by default, VMs only have n/w access to other VMs on same access and the host itself

* now creating the bridge, so VM can have access to LAN

```/etc/sysconfig/network-scripts/ifcfg-em1

BRIDGE=br0
```

```/etc/sysconfig/network-scripts/ifcfg-br0

DEVICE="br0"
## BOOTPROTO could be dhcp/static
BOOTPROTO="dhcp"
IPV6INIT="yes"
IPV6_AUTOCONF="yes"
ONBOOT="yes"
TYPE="Bridge"
DELAY="0"
```

```/etc/sysctl.conf
net.ipv4.ip_forward = 1
```

```
## reload sysctl
sysctl -p /etc/sysctl.conf

## systemctl restart NetworkManager
systemctl restart network
```

---

### Firewalld

* until RHEL6, default packet filtering and forwarding service is `iptables`

* in RHEL7, default service is `firewalld` to talk to kernel packet filter

---

### SELinux

if using `SELinux` in `Enforcing` mode

* apply VT image security context

> say if wanna use a non-default dir for VM images other than `/var/lib/libvirt/images`

```
mkdir /vm-images

yum -y install policycoreutils-python

semanage fcontext --add -t virt_image_t '/vm-images(/.*)?'
```

> verifying it

```
semanage fcontext -l | grep virt_manage_t
```


* restore security context

```
restorecon -R -v /vm-images
```

> verify it

```
ls -aZ /vm-images
```


* if gonna export directory as samba or nfs share, these contexts are required as well

```
setsebool -P virt_use_samba 1
setsebool -P virt_use_nfs 1
```

---

### Creating VMs

> it can run in interactive or non-interactive mode

```
virt-install \
  --network bridge:br0 \
  --name vm1 \
  --ram=1024 \
  --vcpus=1 \
  --disk path=/vm-images/vm1.img,size=10 \
  --graphics none \
  --location=http://my.server.com/pub/rhel7/install-x86_64/ \
  --extra-args="console=tty0 console=ttyS0,115200"
```

> `--location` can point to CD images
> `--graphics` none makes VNC not launched

> `--extra-args` used to pass kernel boot parameters to OS installer
> can also be sed to pass Kickstart file for non-interactive installations

```
virt-install \
  --name vm1 \
  --ram=2048 \
  --vcpus=2 \
  --disk path=/vm-images/vm1.img,size=15 \
  --cdrom /root/RHEL-7.0-20140507.0-Server-x86_64-dvd1.iso
  --extra-args="ks=http://my.server.com/pub/ks.cfg console=tty0 console=ttyS0,115200"
```

---

### Cloning VMs

* suspend vm to be cloned

```
virsh suspend vm1
```

* now clone

```
virt-clone \
  --connect qemu:///system \
  --original vm1 \
  --name vm1-clone \
  --file /vm-images/vm1-clone.img
```

* resumme original vm and start the clone
> virt-clone before takes care to generate new MAC address

```
virsh resume vm1

virsh start vm1-clone
```

---

### Manage VMs

#### Common Usages

* list all VMs on host

```
virsh list --all
```


* show VM info

```
virsh dominfo vm1
```


* show vCPU/mem usage of running VMs

```
virt-top
```


* show VM disk partitions

```
virt-df vm1
```


* stop/start vm

```
virsh shutdown vm1

virsh start vm1
```


* make vm to autostart on reboot, then disable autostart

```
virsh autostart vm1

virsh autostart --disable vm1
```


#### Getting access to VMs console

* without X-server, connecting to VMs serial console might be only way to log-in
> append following param `console=tty0 console=ttyS0,115200` to kernel boot parameter `/etc/grub.conf`

```
virsh console vm1
```


#### Attach storage devices to VM

* identify the device name of your storage device after you plug it in on host, say `/dev/sdb`
> vdb being device name to map inside VM
> can mount device to more than one VM

```
virsh attach-disk vm1 /dev/sdb vdb --driver qemu --mode shareable
```

* to detach

```
virsh detach-disk vm1 vdb
```


#### GUI Tools

gui tools available are

* `virt-viewer`, launches VNC window to VM
* `virt-manager`, can manage VMs here

---

### Changing VM Parameters

* Memory

> example.1

```
virsh dominfo vm1 | grep memory ##view

virsh setmem vm1 524288         ##set in KB
```

> example.2

```
virsh shutdown vm1

virsh edit vm1
## change xml node value
#### <memory>2097152</memory>

## restart VM from updated config file
virsh create /etc/libvirt/qemu/vm1.xml
```


* vCPUs

```
virsh shutdown vm1

virsh edit vm1
## change xml node value
#### <vcpu>2</vcpu>

virsh create /etc/libvirt/qemu/vm1.xml
```


* disk capacity

```
dd if=/dev/zero of=/vm-images/vm1-add.img bs=1M count=10240

virsh shutdown vm1

virsh edit vm1
```

> add a `<disk>` block, to xml

```
  <disk type='file' device='disk'>
    <driver name='qemu' type='raw' cache='none' io='threads'/>
    <source file='/vm-images/vm1-add.img'/>
    <target dev='vdb' bus='virtio'/>
    <address type='pci' domain='0x0000' bus='0x00' slot='0x06' function='0x0'/>
  </disk>
```

> device name be sequential to existing, a unique `slot` in address tag of all devices

```
virsh create /etc/libvirt/qemu/vm1.xml
```

---


#### Destroy VM

```
virsh shutdown vm1

virsh destroy vm1

virsh undefine vm1

rm /vm-images/vm1*.img
```

---
---
