
## Monzo - Building a Bank with Kubernetes

> at CNCF 2016, by Oliver Beattie

* using MicroServices with 150+ components

---

`resilient`

### linkerd

> built around Twitter's Finagle

* run `linkerd` on each host

* run it as daemon-set, each service talks to it's local copy of `linkerd`

* connection scales to instances of `linkerd` not services

* easy retries of requests

* service flow is `ELB to EdgeProxy(auth,log,uuid) to Linkerd`

---

`secure`

### network isolation

* using network policy from [Project Calico](https://www.projectcalico.org/)

```
apiVersion: extensions/v1beta1
kind: NetworkPolicy
metadata:
  name: super-secure-zone
spec:
  podSelector:
    matchLabels:
      zone: super-secure
ingress:
  - from:
    - podSelector:
      matchLabels:
        zone: super-secure
    ports:
      - protocol: tcp
```

implemented at low-level by iptables

---

### QnA

* use `HashiCorp's Vault` for Secret Management

* looking into dynamic routing via `linkerd`, doing static at the moment

---
---
