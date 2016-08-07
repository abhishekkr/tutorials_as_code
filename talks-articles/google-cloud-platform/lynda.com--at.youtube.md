## Google Cloud Platform
> by lynda.com

[try out what, will it cost](https://cloud.google.com/products/calculator)


### Category of Services

* Compute
* Storage
* Other Services (Prediction:to:DNS)


#### Compute

* Virtual Machines
* Containers
* Virtual Services
* Hosted Applications

> AppEngine, Compute Engine, Container Engine


#### Storage

**File Services**
* Google Cloud Storage (files)
* Archival Storage (nearline)

**Data Services**
* NoSQL offerings
* RDBMS offerings
* Hadoop (related technologies)

> Cloud Storage, Cloud Datastore, Cloud SQL, Cloud Bigtable
> Big Query, Cloud Dataflow, Cloud Pub/Sub


#### Other Services

* Machine Learning (Prediction API)
* Translation API
* Networking Services (Firewall, DNS, LoadBalancing, Scaling)
* Management and Monitoring (Logs, Alerts, Dashboards)

> Cloud Endpoints, Translate API, Prediction API


---

### Benefits as a Cloud Provider

#### Pricing

* Google Cloud got a generous free tier. Automatic usage discounts.
* Mostly their services are often standby warm or cached making it quick.
* Services are scalable, saving time for people using them.


#### Scalability

* default is 'autoscale'
* most probably you wouldn't run out of resources, Google Scale
* but make price estimation a part of your planning and testing


#### Compliance

* 3 categories: audits, resources, security
* supports most of Industry recognised standards as ISO 27001, PCI DSS v3.0, AICPA SOC2, SOC 3.
* supports HIPAA covered customers with agreement


#### Tools and Libraries

* not as many options available as for some competitors
> * Google Tools
> * 3rd Party Tools
> * Console/GUIs
> * Command line utilities (gcloud)
> * Google APIs (python, golang, java, dart)

* Google SDK, got integration in Android Sudio and a plug-in for Eclipse
* Cloud Repositories, git remote repository on GCP (can use GitHub)
* Container Registry, proivdes secure Docker image storage on GCP
* Source Code Tools, can browse/edit/troubleshoot code in Google Dev Console after pushed to Cloud Repositories
* Cloud Debugger, inspect state of your app at a specific code location without log statements or stopping/slowing app
* Cloud Playground, run services like GAE, CloudStorage and CloudSQL right from browser
* Cloud Security Scanner, crawls your webapp testing as many user inputs and event handlers possible


---

### Info to know before you start using

* need to access an account obviously
> * sign-in at [cloud.google.com](https://cloud.google.com)
> * go to [developer's console](https://console.developers.google.com)

* Regions and Zones {same as AWS and others}
> * 'regions' as in different wider geographic sections as 'Central US', 'Western Europer', 'Eastern Asia'
> * 'zones' are specific locations under 'regions' with data-centers; not all provide all features

Here one start with idea of 'Project'; the constructs of it.
These are identified by 'id', 'name' and 'number'.

* create a project on 'dev-console'; provide 'any-project' name and can select things like regions under advanced options.
> able to take a note of Project-ID and Project-Number on project overview page at 'dev-console'.
> Project Dashboard provide lots of task options


#### billing

> * global billing, the main settings for account and projects under it
> * individual billing
> * test account {$300 for 60days}
Need to be sure which Project's service being configured, can edit quota of services individually.


#### Security

> * permissions; at './Permissions' can add members (e-mail IDs) as owner, editor or viewer which they need to accept
> * credentials; service accounts can be generated to be used as editor or viewer from './APIs and Auth/Credentials'
> * consent screen; at './APIs and Auth/Consent Screen'
> * push; at './APIs and Auth/Push' for WebHook Notifications to push notification of details of Google Cloud usage and stuff to any of your service. Need a registered domain.

* 'APIs & Auth/Credentials'
> * OAuth (Create New Client IDs); types of Web-App, Service-Account or Installed-App
> * Public API access (Create New Keys): types Server-key, Browser-key, Android-key, iOS-key
>
> Web-App OAuth a/c let's you conigure 'Consent Screen'. Also configure javaScript origins and redirect allows.


---

#### Working with 'gcloud'

> * Download [GCP SDK](https://cloud.google.com/sdk)
> * Start 'gcloud' from console in project folder
> * Need authenticate 'gcloud' to GCP
> * List or set your projects manually to be sure

Installing
```
curl -Lk https://sdk.cloud.google.com | bash    ## installing
## can `export CLOUDSDK_CORE_DISABLE_PROMPTS=1` for silent install
## can later disable reporting via `gcloud config set --scope=user disable_usage_reporting true`

exec -l $SHELL                                  ## reloading shell
gcloud version                                  ## confirm installation
gcloud auth login                               ## authenticate to GCP

## to config current project
gcloud config set project $PROJECT_ID

## list main details of current configs
gcloud info
```

---

#### Walkthroughs and Samples

* Sample Code
> * [GCP Docs](https://cloud.google.com/docs)

* Tutorials
> *

* GitHub Repositories
> * [GCP Sample Apps, Libraries](https://googlecloudplatform.githu.io)

* Community Code: StackOverflow
> * [SO: Google CloudStorage](http://stackoverflow.com/questions/tagged/google-cloud-storage)
> * [Recognized Experts](https://developers.google.com/experts/all/technology/google-cloud-platform)

---

#### Third-Party Resource

> * RightScale; for template based management of cloud services and other things
> > [plan for cloud](https://planforcloud.rightscale.com) allows to view predicted costs across multiple cloud providers for planned usage

---

### Google Computer Services

* IaaS
* PaaS
* Containers

#### Google Compute Engine (GCE)

Services around IaaS
Can create using Web-Dev-Console, also provides command for 'gcloud' equivalent command.


#### Click-to-Deploy

Creating one or more vm as base machines for applications, databases, etc. {like AMIs}
This option is available under './Deploy and Manage/Click to deploy' section. There are pre-built images by Google and other vendors.


#### GCE Resources

There are resources associated with GCE Services.

* Storage and Information
> Images (of VM)
> Snapshots (of VM at a point in time)
> Disk or OS versions/type
> Groups or Metadata

Global Resources are available within any zone of same 'higher-level' project.
Regional Resources within a region for a project.
Zone Resources are available within a zone for a project.
Instance Resources are just for specific instances.


####  Using Google AppEngine

Load-balances under available quota, it's auto-scale.
Has configurable Version control, Security scans, Memcache, Search and few.


####  Using Endpoints

Is implementation of GAE often used with Android development for infastructure, auth, storage, mail and others.

Can't view them in Dev (web)Console, available via console tools and APIs.
These can be added as a backend in Android Studio and similar.


#### Using Containers

Dockers, Kubernettes


#### Managed VMs / Sandbox

GAE managed VMs. Partially managed VM using Docker runtimes.
Standard Google runtimes for 'services' or custom runtimes.
When need more programmatic control like background threads or 3rd-party binaries.
More at [documentation](https://cloud.google.com/appengine/docs/managed-vms).

---

### Google Storage Services

* File storage: Warm(active) or Cold(archival --like Nearline here as Glacier from AWS)
* Database Storage: small, medium, large or huge {workloads, sql/nosql, data-warehousing, OSS/Propreitry}
* Decision Factors: Capacity, cost and speed.

#### Using CloudStorage

Analogous to S3, uses buckets. Three types of classess from
> * Standard
> * Durable Reduced Availability
> * Cloud Storage Nearline (archival, analogous to AWS Glacier)

> Let's you import AWS S3 bucket data. Provides on-demand I/O.


#### Using CloudSQL

> * Based on MySQL
> * Default max size is 250GB/instance, can request upto 500GB/instance.
> * Can pick an instance with required RAM in any region.
> * Provide Backup settings, binary log.
> * Can be activated during traffic, always on or just off.
> * Filesystem replication can be synchronous or asynchronous.
> * There is charge on every hour of IPv4 assignment of unused time.


#### Using Cloud Datastore

NoSQL like storage
> * uses Paxos algorithm to understand resolving of data, for autoscaling reliable storage
> * uses typed entites and associated properties, lightly typed schema
> * powerful query using GQL, indexed properties

You create an entity under `Cloud Datastore`, a `namespace` and `kind`.
Entity can have set of properties with key and value, that can be (or not) indexed as required.


#### Using BigQuery

On Console, goto 'BigQuery' under 'BigData'.
> * For Read-Only Queries
> * You can write ANSI SQL queries for upto 100K rows/second.
> * Multiple visualization partners. [3rd Party](https://cloud.google.com/bigquery/third-party-tools)
> * Can use cached results.
> * Interactive query start immediately but more quota restrictions. Batch start late but less costly.


#### Using BigTable

> * NOSQL Storage, BigData usage with Read-Write requirement.
> * Supports Apache HBase 1.0, best for workload '>1TB'.
> * Programmable APIs to consume the service.
> * 30K QPS Performance, 30 MBPS Throughput.


#### Introducing Pub-Sub, Dataproc, Dataflow

Cloud Pub/Sub
> * The concept of `Topic`, producers send message to a topic and consumers subscribe to it.

Cloud Dataproc
> * Run Hadoop ecosystem, provision Hadoop clusters to connect to Datastores.
> * Run Hadoop Hive, Spark, or Pig jobs on your cluster.

Cloud Dataflow
> * Big ETL (Extract, Transform and Load)
> * To move daa among different Google Cloud Data storage service in programmatic manner.


#### Using Bime to visualize BigQuery results

Big-Query 3rd Party Tool for Vizualization (other than Tableau, Qlik, Looker)


#### Understanding Google Storage Services and Pricing

Use calculator to get an idea.

---

### Understanding Other Services

#### Networking Services

Include Networks, External IPs, Firewall, Routes, LBs, Cloud DNS, VPNs
> * resource scopes range from global, regional, in zones per instance.
> * `default` network, adjusting services around it; create more for isolation.
> * External IP addresses can be attached to regions, attached is not billed, just allocated is billed.
> * can build `firewalls` programmatically
> * `routes` allowing you to communicate between `networks`, set priority
> * `LBs` distributing traffic across healthy instances; can use instance pools, session affinity, rules, health check
> * `DNSs`, creating a zone and records within it
> * `VPNs`, a secured set of service within; a site-to-site IPSec VPN


#### Monitoring Services

> * need to enable traces, for live performance analysis
> * `logging` is turned on by default, can be filtered, can be exported (say to IoT)
> * dashboards and alerts; can manage incidents


#### Source Code Services

> * free private Git code repository
> * edit and debug in cloud


#### Other APIs

Prediction API
> * ML for non-deterministic query on data
> * it's enabled by default, need to turn-on OAuth
> * can predict using hosted or training model
> * need to use Programmable API to create model

Genomics API
> * design to process Genomic data in cloud

---

### GCloud Command Line Tool

* [gcloud reference](https://cloud.google.com/sdk/gcloud/reference)

* gcloud `alpha|beta|preview`, get latest via `gcloud components update`

#### Old vs New

```
## old
gcutil addinstance my-instance --wait_until_running

## new
gcloud compute instances create my-instance --zone us-central1-a
```

#### Common `gcloud` commands

```
gcloud version

gcloud components list

gcloud auth login

gcloud config set project $PROJECT_ID

gcloud config list
```

* can use gcloud command from `reference` link on the web-console

---

* [GCP Python Playground](http://cloud-playground.appspot.com/playground)
> gives simple python based Google Cloud Platform AppEgnine playground.

* SublimeTextEditor with Python for GCP, if got no fav editor

---

### GCP Tools Summarized

* command-line : gcloud (Google Cloud SDK)

* IDEs : Android Studio (GCP Support), Eclipse with Google Plugin

* Web-based : Cloud Playground, Codenvy IDE (work best in Chrome)

* Others
> * Security Scanner
> * a REST Tool

---

### Use Reference Architecture

* [WebApp on GAE](https://cloud.google.com/solutions//architecture/webapp/)

```
                                     ,------[Cloud Storage]
                                    |
  [clients]------>[Google LB]---->[GAE]----->[Cloud SQL      ]<----[GAE]
                                    | \      [Cloud Datastore]       |'
                                    |  '---,                         |
                                    |,      \                        |
                                [Memcache]   [Task Queues]-----------'
```


* [Mobile Apps and Games](https://cloud.google.com/solutions//architecture/mobileandgames/)

```
  ,----------------+-----------------[Google Cloud Messaging]
  |                |                          |'
  |  (Apple Push Notif. Service)<-------------|
  |     |,         |                          |
  |   [iOS]------, |   [Cloud Endpoints]-->[GAE:]-->[Task Queues]-->[GCE:]
  |   CLIENTS    | |     |'                   |                      |
  '---[Android]--|-+-----'       ,------------|                      |-------,
                 | |             |,           |,                     |,      |
                 | |         [Memcache]  [Cloud Datastore]  [Cloud Storage]  |
                 | |                                          |'             |,
                 '-+------------------------------------------'     [Big Query]

 : -> autoscaling
```


* [RealTime Stream Processing](https://cloud.google.com/solutions//architecture/streamprocessing/)

```
(these are clients)
 [Devic]                     ,----->[GCE]
 [Farms]                     |        |,
     '----->[Google LB]--->[GAE]    [GCE]--->[Cloud Storage]
                                      |      [Cloud SQL    ]
                                      |,            |'
                                  [BigQuery]      [GAE]
                                      |'            |'
    [Query and Visualize]-------------'-------------'
```

---
---
