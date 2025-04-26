
## blkid

> * name infers block id
>
> * to locate/print block device attributes
>
> * its basic listing is `LABEL`, `UUID`, **filesystem** `TYPE`, for partition `PARTLABEL` and `PARTUUID`

* to get basic details of all block devices

```
blkid
```

* to get details of a specific device

```
blkid /dev/$deviceId
```

* to list `MINIMUM_IO_SIZE`, `PHYSICAL_SECTOR_SIZE` and `LOGICAL_SECTOR_SIZE`

```
blkid -i /dev/$deviceId
```

* to just list one filtered by known value, `blkid -l -t LABEL="OS"`

* to lookup devie by its UUID, `blkid -U $UUID`

* to list device path if known the device label `blkid -L $DEVICE_LABEL`

* can also list all known filesystems to machine

```
blkid -k
```

* can make it switch to low-level device probing and not depend on cache using `blkid --probe`

* probe for specifc types as `blkid --probe -n vfat,ext3 /dev/sdb` or `blkid --probe -n nominix /dev/sdb`

---
---
