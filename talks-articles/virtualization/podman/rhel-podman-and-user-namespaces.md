
## Podman and User Namespaces

> [source](https://opensource.com/article/18/12/podman-and-user-namespaces)

* part of [libpod](https://github.com/containers/libpod) library, enable users to manage pods/containers/container-images

* can have UID 0 inside container and UID 1000 inside; even if container process breaks out wouldn't have much access


### Example

* `sudo bash -c "echo Test > /tmp/test ; chmod 600 /tmp/test"` to create a file owned/modifiable by root

* following to volume mount `/tmp/test` into container running user namespace map `0:100000:5000`

```
sudo podman run -ti -v /tmp/test:/tmp/test:Z --uidmap 0:100000:5000 fedora sh
```

> * telling map a range of 5000 UIDs outside container, starting with `100000` to `104999`
>
> * to a range `0-4999` inside container
>
> * since real `UID=0` is not mapped into container, any file owned by root treated as owned by nobody
>
> * even with `CAP_DC_OVERRIDE` in container it can't override this protection, user namespace capabilities ain't same as on host
>
> * even if processes could somehow enter another container, they would not have those capabilities if the container uses a different range of UIDs
>
> * selinux would also limit what would happen if a container process broke out of container


### Podman `top`

* reports user process running as root inside container but as UID 100000 on host (HUSER)

```
sudo podman run --uidmap 0:100000:5000 -d fedora sleep 1000

sudo podman top --latest user huser
ps -ef | grep sleep

sudo podman run --uidmap 0:200000:5000 -d fedora sleep 1000

sudo podman top --latest user huser
ps -ef | grep sleep
```


### Problems with user namespaces

* Podman can use different user namespaces on same image via auto chown-ing built into [containers/storage](https://github.com/containers/storage); chowns all files in image to UIDs mapped in user namespace and creates a new image

* Podman build/commit reverse shift and push image with all files chown-ed back to UID=0

* new OverlayFS feature in Kernel 4.19 called `metadata only copy-up`, allows to not copy up lower layers for unchanged content if mounted with `metacopy=on`

* Podman can use `--userns=auto` to automatically pick unique user namespace, or env var `PODMAN_USERNS=auto`

---
