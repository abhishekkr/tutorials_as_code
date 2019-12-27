
## Anthos

> sources: [cuelogic.com](https://www.cuelogic.com/blog/what-is-anthos-the-multicloud-platform-by-google)

* it's a hybrid multi-cloud platform from Google


### What is Anthos

* is not a single product but multiple services under an umbrella

* have services catering to cloud migration, application modernization, multi-cloud and hybrid cloud management

* it's OSS built on Google Kubernetes Engine (GKE)

* can be run on-premise, AWS, Azure; in the same way as on GCP... so cloud agnostic


### Building Blocks of Anthos

* GKE is at core as CNC, other tech than Kubernetes are also at play. Distributed infra can be managed in hybrid cloud settings.

* GKE On-Premise operating as virtual appliance on VMWare vSphere, support for KVM and Hyper-V is WIP

* Istio providing federated network management, seamless integration with ACI/Cisco/NSX/Google's Andromeda.

* Velostrata allows cloud migration converting existing VMs to Pods.


### Anthos Configuration Management

* is a secure, version-controlled central cache for everything about config and admin

* Stackdriver; for central logging, tracing and monitoring system

* GCP Cloud Interconnect; high-speed connectivity between cloud infra and DataCenters

* GCP Markeplace; applications like Gitlab and Cassandra are readily deplyable in a single click

---
