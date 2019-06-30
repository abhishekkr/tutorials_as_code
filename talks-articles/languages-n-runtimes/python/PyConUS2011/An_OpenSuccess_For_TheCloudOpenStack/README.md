### An Open Success For The Cloud OpenStack

> [source](http://pyvideo.org/video/393/pycon-2011--an-open-success-for-the-cloud--openst)

##### OpenStack :  2 major components Swift (Data Storage Engine) & Nova (Compute Engine)

* Glance    : Image managing system (VM images get stored and boot from here)
* Burrow    : Messaging System
* Horizon   : Dashboard (DJango WebUI)

>         OpenStack Instance handled by RackSpace in Nasa is 'Nebula'

>         Over 50 Companies involved in OpenStack (Scalr, Citrix, NTT, Opscode, Piston, FathomDB, CloudScaling, CloudKick)

##### Nova

* RPC      : JSON, Carrot, AMQPLib, Rabbit
* Tornado  : Simple Object Storage; initially used built-in capability from Torando (like  FileSystem S3 from AWS)
* Twisted  : Ported lots of stuff from Tornado to Twisted
* Eventlet : To get newbies involved on project, already using inline generators 
* Gevent?  : May be porting to Gevent from Eventlet

[GitHub Nova](https://github.com/openstack/nova)

[Main Website](http://openstack.org)
[QuickStart](http://ansolabs.com/deploy)
[Speaker : Andy Smith](http://term.ie)

* KeyStone : Identity Management
* Quantum  : Network Management
* Cinder   : Block Device
