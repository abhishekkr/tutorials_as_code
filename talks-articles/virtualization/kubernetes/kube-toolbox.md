
## Kube Toolbox

### Autoscalers

* [Cluster Autoscaler](https://github.com/kubernetes/autoscaler/tree/master/cluster-autoscaler), automatically adjust nodes based on Pod states and have Gotchas for manages Kube Engines

* [Horizontal Pod Autoscalar](https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale-walkthrough/), available in Kubernetes via `kubectl`

---

### Kubernetes Usage Metrics

* [kube-resource-report](https://github.com/hjacobs/kube-resource-report): reports cluster and pod resource requests vs usage, generates static html

* [Karl Stoney's Grafana Dashboard](https://karlstoney.com/2018/07/07/managing-your-costs-on-kubernetes/): manage cost in public cloud, tested with GCP for Grafan cost dashboard

* [CoreOS operator framework metering](https://coreos.com/blog/introducing-operator-framework-metering):

* [GKE Usage Metering](https://cloud.google.com/blog/products/containers-kubernetes/gke-usage-metering-whose-line-item-is-it-anyway), [Cluster Usage Metering](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-usage-metering)

---

### Kubernetes Web UIs

* [K8Dash](https://github.com/herbrandson/k8dash), single cluster only without CRD support

* [Kubernator](https://github.com/smpio/kubernator), single cluster view only

* [Octant](https://github.com/vmware/octant), extensible platform for developers

* [kube-ops-view](https://github.com/hjacobs/kube-ops-view), only shows cluster nodes and pods for a quick overview

* [Kubernetes Web View](https://codeberg.org/hjacobs/kube-web-view/); kubectl for web

---
