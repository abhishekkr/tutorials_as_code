
## df

> * name infers `disk free`; remports filesystem disk space usage
>
> * uses 1K blocks by default, if `POSIXLY_CORRECT` env is set it uses 512-bytes blocks

*  if argument is not device, but device node with mounted filesystem then unmounted filesystems are not accounted for

* include pseudo/duplicate/inaccessible filesystems - `df -a`

* for human readable sizes - `df -h`

* to list total usage - `df --total`

* to list inode usage instead of block - `df -i`

---
---
