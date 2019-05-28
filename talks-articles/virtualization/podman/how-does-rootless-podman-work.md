
## How does rootless Podman work?

[source](https://opensource.com/article/19/2/how-does-rootless-podman-work)

* current linux distros include a version of `shadow-utils` using `/etc/subuid` and `/etc/subgid` files to determine UIDs and GIDs available for a user namespace

* `useradd` program automatically adds 65536 UIDs per user added; for existing users can update but be careful to not overwrite

* Podman defaults mapping root in container to current UID, then maps ranges to allocated UIDs/GIDs in `/etc/subuid` and `/etc/subgid`

* any item from outside user namespace owned by UID/GID not mapped into user namespace appears to belong to user configured in `kernel.overflowuid` sysctl (by default 35534 for `nobody`)

* [buildah](https://buildah.io/) commands provide a sub-command `unshare` to put you in same user namespace that Podman runs in without entering container's filesystem

```
buildah unshare ls -ld $HOME  ## shall show owner as root not current user, as from within container perspective
```

* Podman currently allows mounting of `procfs`, `sysfs`, `tmpfs`, `fusefs` and bind mounts

* Podman mounts container storage; then mounts `/proc`, `/sys` and few `tmpfs` creating device in container

* to user networking other than host, uses [slirp4netns](https://github.com/rootless-containers/slirp4netns) to setup `User mode networking for unprivileged network namespace`

---

