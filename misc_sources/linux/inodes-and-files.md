## Unix Inodes and Files
##### from ThoughtBot 'Lunchtime Lightning'

> Inodes are data-structure that represent metadata of a file, stored on disk and references the location of actual blob.

#### Metadata Fields

* who owns/access; user and group ownership; access permissions
* type of file; regular, directory, character device, block device, FIFO pipe
* access times; file accessed and modified, inode modified
* number of hard links to file
* addresses of disk blocks containing data
* file size
* doesn't have file-paths

> example inode
> * owner: abc
> * group: abc,xyz
> * type: regular file
> * permissions: rwx-r-xr-x
> * accessed: Date-Time-stamp
> * modified: Date-Time-stamp
> * inode: Date-Time-stamp
> * size: ijk bytes
> * (disk addresses)

---

#### Use in commands

* get inode number of files
```
ls -li /some/path
```

* list a file by inode number
```
find . -inum <inode-number>
```

---

#### References:

_Papers_
* "A fast file system for Unix" by Marshall Kirk McKusick, William N. Joy, Smauel J. Leffer & Robert S. Faby
* "The Unix time-sharing system" by Dennis Ritchie and Ken Thompson

_Books_
* "The Design of the Unix OS" by Maurice J. Bach
* "The Unix Programming Environment" by Brian W. Kernighan & Rob Pike
* "Advanced Programming of Unix Environment" by W. Richard Stevens & Stephen A. Rago

---
---
