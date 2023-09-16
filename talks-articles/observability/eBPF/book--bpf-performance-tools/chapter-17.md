
## Chapter.17 Other BPF Perf Tools

### PCP w/ pmc-pmda-bcc, Grafana

* `pmc-pmda-bcc` is [PCP](https://pcp.io) Perf Metric Domain Agent extracting live perf data via eBPF in-kernel programs using BCC. Available via [Grafana PCP plug-in](https://grafana-pcp.readthedocs.io/en/latest/quickstart.html).

* `pmcd` (Perf Metric Collector Daemon) is central PCP component coordinates metric collection from agents.

* `pmwebd` is REST gateway to `pmcd` instance for web-ui.

> [RHEL's Doc](https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/8/html/monitoring_and_managing_system_status_and_performance/setting-up-graphical-representation-of-pcp-metrics_monitoring-and-managing-system-status-and-performance) on settin-up this setup.


### Cloudflare eBPF Prometheus Exporter

* [ebpf\_exporter](https://github.com/cloudflare/ebpf_exporter) to export custom eBPF metrics.


### kubectl-trace

* [kubectl trace](https://github.com/iovisor/kubectl-trace) plug-in allowing scheduled execution of bpftrace programs on K8s cluster.

```
kubectl trace run node/ip-10-0-0-41 -f /usr/share/bpftrace/tools/vfsstat.bt

kubectl trace get

kubectl trace logs -f $KUBECTL_TRACE_NAME_FROM_GET
```


### Other Tools

* `Cillium`, `Sysdig`, `Android eBPF`, `osquery eBPF`, `ply`.

---
