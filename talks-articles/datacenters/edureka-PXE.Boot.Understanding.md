
## PXE Boot Understanding

> Edureka

### PXE ~ Preboot Execution Environment

* to allow to boot over a network and install OS using remote environment
* h/w server ability to boot over network instead of other media like DVD/USB.

PXE depends upon 3 protocols
* DHCP (when server starts it needs an IP to be reacha ble, start searching for boot server)
* TFTP (TFTP Server will be acting as network based boot server, pull pre-boot kernel from here)
* NFS (NFS Server will have entire media content for OS installation)

```
   ,--,.                                 _______________.           PXE Boot Server configured
   | o||                                /________________\  Switch  with CentOS Sx
   |__|/-------------------------------[__::___::___::____]         1. DHCP Server
                                          ||   ||   |               2. TFTP Server
  PXE Linux Server   _____________________||   ||   \               3. NFS/FTP/HTTPD server
  192.168.10.1      |   ___________________|  / |    \              (store installing files)
  DHCP/TFTP/FTP/NFS |  |         ____________/  /     |
           _________/  |        /             _/      |
      ,--,'       ,--,,/      ,/-,        ,--/        |--,
      [||] o      [||] o      [||] o      [||] o      [||] o
      '--' ^      '--' ^      '--' ^      '--' ^      '--' ^
      client1     client2     client3     client4     client5

```

* Popularly used currently to install OS on Bare Metal, install Hypervisor.

---
