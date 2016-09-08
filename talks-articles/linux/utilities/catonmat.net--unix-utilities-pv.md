
#### A Unix Utility You Should Know About:
## pv (pipe viewer)

[source](http://www.catonmat.net/blog/unix-utilities-pipe-viewer/)
> Dec'23 2009

To get an indication of streaming data across linux pipe, visual indication of it.

```
sudo {yum,dnf,apt-get} install -y pv
```

* `pv` can be used to have visual indication of how much data has been piped, with what speed and how much more time it might take.


* example, gunzip a big file
> act like `cat` with a progress bar

```
## without pv, blackbox on progress
gzip -c verybig.log > verybig.log.gz

## with pv, you'll get desired progress state
pv verybig.log | gzip > verybig.log.gz
```


* can pipe together `pv` to gather in-between progress as well
> creating `named` stream with `-N`
> `-c` ensuring `pv` don't garble each-others output
> here second `pv` is unaware of ETA as don't have a handle on source

```
## can read how fast read from disk and output
pv -cN source verybig.log | gzip | pv -cN gzip > verybig.log.gz
```


* pack whole dir of file in tarbal

```
## this again wouldn't have ETA as no idea of complete size
tar -zcf - $DIR_TO_TAR | pv > ${DIR_TO_TAR}.tgz
```


* whole dir with `pv` handling ETA
> can provide `pv` with source size

```
## this again wouldn't have ETA as no idea of complete size
tar -cf - $DIR_TO_TAR | pv -s $(du -sb $DIR_TO_TAR | awk '{print $1}') | gzip > ${DIR_TO_TAR}.tgz
```


* copying large failes over network using `nc`
> can use `-s` from last example here for ETA

```
## machine-IGotFiles
tar -cf - $DIR_TO_CP | pv | nc -l -p $PORT_FOR_NC -q 5

## machine-INeedFiles
nc machine-IGotFiles $PORT_FOR_NC | pv | tar -xf -
```


* just time how fast can machine read from `/dev/zero`

```
pv /dev/zero > /dev/null
```

---
---
