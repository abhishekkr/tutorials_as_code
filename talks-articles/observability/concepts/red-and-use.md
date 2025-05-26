
## USE (Utilization, Saturation, Errors) and RED (Rate, Errors, Duration) for Metrics

Monitoring can be a daunting task due to the vast amount of potential data points that can be available as Metrics. USE & RED are approaches to identify the quick wins and a general approach.

* USE is focused on system contained service behavior, giving insight to your platform that power whatever's running. Utilization of cpu/mem/network resources and their relevant internal metrics. Level of Saturation of these resources, what component adds on to it at what level. Errors to signal app issues to identify at what level of utilization can be achieved with what kind of tuning.

> U as % of time a resource is in use. S as resource's queue of work. E for error count.
> By Brendan Gregg (DTrace).

* RED is focused at request-driven systems like webapps. Rate of requests, and it's correlation with performance. Errors as one of the key parameters for perf and Duration (latency) as the other.

> R as req/sec. E as failed request count. D as response latency.
> By Tom Wilkie (Google).

* Google also uses _The Four Golden Signals_ as core: Latency, Traffic, Errors, Saturation.

---
