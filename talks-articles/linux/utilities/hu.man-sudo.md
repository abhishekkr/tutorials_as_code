
## sudo

* edit only using `visudo` for single-user edit lock and syntax check


* make sudo user always enter password
> also be made by aliasing `sudo` to `sudo -k`

```
## Defaults timestamp_timeout=0       ## uncomment this to apply to all
Defaults:someusername timestamp_timeout=0
```


* allow all commands under a path, do end the path with slash
> it is not recursive

```
someusername ALL=(root) /opt/bin/
```


* disable user to use arguments

```
someusername ALL=(root) /bin/ls ""
```


* allow user to only run specific full commands, say only start/restart/status on a service not stop

```
someusername ALL=(root) /etc/init.d/httpd start, /etc/init.d/httpd restart, /etc/init.d/httpd status

someotherusername ALL=(root) /usr/bin/passwd someusername, /usr/bin/passwd otherusername
```


* report all unauth sudo attempts, can use `logwatch`

```
logwatch --print --service sudo --range all
```

---
---
