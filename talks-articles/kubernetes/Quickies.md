
[Cloud Native Computing Foundation](https://cncf.io/)

### Good Links on Ideology:

* [Google on the Inevitable Future of Container-Based Architectures](http://thenewstack.io/google-inevitable-future-container-based-architectures/)

> Concept of offering Computing instead of Computers.
> Encapsulated environment in which application or pieces of it could be placed.

---

* [](http://thenewstack.io/google-learned-borg-container-management/)

> Decoupling OS and Container Images.
> Application Oriented Architecture. APIs for Container Management, treating them as code not spec.

> * Don't make container system manage 'port numbers'
> > Borg assigning unique port-numbers to containers made need for something more than DNS. Kubernetes assign unique IP per pod aligning app-identity to network-identity.

> * Don't just number containers, give them 'labels'

> * Careful with Ownership, Label-Ambiguity

> * Don't expose raw states. Avail validation, defaulting and versioning.

---

## Kubernetes 1.1 got

* Ingress API for HTTP Load Balancing (BETA)

* Autoscaling (got Horizontal, working on Vertical)

* Batch Jobs (coming - Shard numbers, Scheduled Jobs, Workflows)

* Resource Overcommit (Memory Overcommit)

* IPTables Kube Proxy

* New Kubectl tools

* Rolling update improvements, wait for a pod to be healthy then proceed

> 1million QPS, 1000+ nodes*

---
---
