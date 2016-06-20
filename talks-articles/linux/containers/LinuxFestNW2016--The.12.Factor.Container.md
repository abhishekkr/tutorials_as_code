
## The 12 Factor Container
> Linux Fest North West 2016, @caseywest

> **edited on some points as seen fit by self understanding**

### Best Practices

* One codebase tracked in revision control, many deploy.

* Use the environment and/or feature flags.

* Explicitly declare and isolate dependencies.

* Declare version numbers of upstream dependencies.

* Depend on base images for default filesystem and runtimes.

* Store config in the OS environment.

* Treat backing services as attached resources.

* Connect to network attached services using connection info from environment.

* Strictly separate `build`, `install`, `deploy` and `run` stages.

* Build immutable images, then run those images.

* Respect the lifecycle: build, run, destroy.

* Execute app as one or more stateless processes.

* Scheduling LRPs by distributing them across a cluster of physical hardware.

* Export services via port binding. Allow environment running container to tell you at what port to run the service, export.

* Scale out via process model. Horizontal scale by adding instances.

* Maximize robustness with fast startup and graceful shudown.

* Keep all envrionments as similar as possible. Run containers in development to production.

* Treat logs as event streams. Log to `stdout` and `stderr`, can use tools to stream these to elsewhere from container.

* Run admin/management tasks as one-off processes.

* Re-use app images with specific entrypoints for tasks.

---


### Anti-Patterns

* Building separate images for staging and production, baking in configuration breaks promotion of artefacts across environments.

* Tags for `dev` and `prod`, different environment.

* `latest` version, no fixed point.

* `config.yml|xml|json`, volume mounting configuration properties breaks portability.

* Hard-coded feature flags.

* Local disk, don't assume. Portability impact.

* Install on deploy.

* NFS

* Hot load data in memory outside, just use redis/memcache.

* Don't ssh to prod env looking for log files.

* Random log-files all over the system.

---


### Cloud Anti-Patterns

Bragaining

* Containers prepared just like other virtual machines, bake everything in. Run it like a process.

* No automated Continuous Delivery.

* Works on my machine. Dev just YOLO-ing shit to prod. Collaborative container build. Process by devs. Security and cloud-y by ops.

* Cram monolith into a container, call it a microservice.

* BI-Modal IT. Some manual, some automated.

* Creating microservices talking to same data source.


Depression

* Made 200 microservices and no jenkins. Backlog of liability.

* Automated pipeline, release twice a year.


Acceptance

* All software sucks.

* Respect CAP. Design and plan around.

* Respect Conway's Law

* Small batch size work for replatforming

* Automate everything

---

> You are not 'not doing' packaging and versioning of builds when baking containers. Just doing it at a different level.

> Secret Management

> Ceph, Gluster

---
---
