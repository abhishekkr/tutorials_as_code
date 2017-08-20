
#### A Unix Utility You Should Know About:
## lsof

[source](http://www.catonmat.net/blog/unix-utilities-lsof/)
> Dec'23 2009

* list all open file handles (duplicated if shared among procs)

```
sudo lsof
```


* info about open handles on a path, multiple paths can be provided

```
sudo lsof /etc/xdg/xfce4/xinitrc
```


* all open file handles from a dir, recursively

```
sudo lsof +D /etc
```


* all open file handles by a user/groupID/process name

```
## user
sudo lsof -u gooduser

## groupID
sudo lsof -g 10

## users
sudo lsof -u gooduser,betteruser
sudo lsof -u gooduser -u betteruser

## process
sudo lsof -c chromium

## process regex
sudo lsof -c chrom

## processes
sudo lsof -c chrom -c fire

## user OR process
sudo lsof -c chrom -u gooduser

## user AND process
sudo lsof -a -c chrom -u gooduser

## users EXCEPT root
sudo lsof -u ^root
```


* all open file handles by process' PID

```
## pid
sudo lsof -p 1

## pids
sudo lsof -p 100,110,120

## all EXCEPT a PID
sudo lsof -p ^100
```


* list network connections

```
## all
sudo lsof -i

## all tcp
sudo lsof -i tcp

## all udp
sudo lsof -i udp

## using a port
sudo lsof -i :22
sudo lsof -i :ssh   ## given found under /etc/services

## using specific port
sudo lsof -i udp:1194
sudo lsof -i tcp:80

## mixing it up with other options, say by a user
sudo lsof -a -u gooduser -i
```


* NFS (network file system) file handles

```
sudo lsof -N
```


* all Unix domain socket files

```
sudo lsof -U
```


* all file handles associated to descriptor numbers

```
## opened as fd# 2
sudo lsof -d 2

## fd range from 5-10
sudo lsof -d 5-10

## there are special values
#### like for memory-mapped files
sudo lsof -d mem
#### programs loaded in mem and executing
sudo lsof -d txt
```


* only output PIDs of processes filtered with other params

```
sudo lsof -d txt -t
```


* repeat listing of file handles until interrupted

```
## repeat every 1 second, of network connection file handles by gooduser
sudo lsof -r 1 -u gooduser -i -a
```

---
---
