
## Podman: A Secure way to run containers

[source](https://opensource.com/article/18/10/podman-more-secure-way-run-containers)

> [Podman](https://podman.io/) uses fork/exec model (vs. a client/server model) for running containers.

* linux allows `audit` to watch over security events on a system, say to check if someone modified a file like `/etc/shadow` can use `auditctl -w /etc/shadow`

> now even after action like `touch /etc/shadow`, can see events with `ausearch -f /etc/shadow -i -ts recent`

* kernel keeps track of actual user using `loginuid` field stored at `/proc/self/loginuid`

> it can only be set once user logs into the system and no process is allowed to reset it; even becoming another user wouldn't change it

* container process under `podman` retians `loginuid`, under `docker` it doesn't

* a docker launched in privileged mode, if leaves something unwanted can't be traced back to actual user

* Podman can run as non-root user and also re-use socket activation

---
