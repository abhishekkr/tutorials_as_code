## OReilly TimeSeries Databases
> by Ted Dunnings and Ellen Friedman


---

#### Chapter.1 TimeSeries Data - Why Collect It

Someone interested in when a thing occured; rate of change or just order of event groups. You can co-relate events and condition that co-occur.
[Unusable when]
* Just interested in current value; like speedometer for driver
[Usable when]
* Mapping engine parameters, speed and acceleration with location indexed by time.
* Wanna co-relate increase in atmospheric CO2 to climate changes; rate of increase in atmospheric CO2 (collected since 1958)
* New methods for building new timeseries datastores for handling much larger datasets
* Airplane's BlackBox storing sensors events and data used to reconstruct events that precede a malfunction; improve fuel consumption and best identify schedule for replacements

How best to store, access and analyze this kind of data marks the need of TimeSeries Database.
Via crowd-sourcing and timeseries data-analytics; Optimal shipping routes based on winds and currents. (http://www.naval-history.net)

---

#### Chapter.2 A New World for TimeSeries Database

* NoSQL vs RDBMS
> * NOSQL got simpler coordination capabilities (or none) compared to transactional RDBMS.
> * NOSQL eliminate all or most SQL and complex optimizer required for SQL to be useful.
> * (Apache Drill allows full SQL capability with Developer desinged guarantees over data.)

The modern day data collection rates are overwhelming for traditional RDBMSs.
NoSQL systems typically needed to provide sufficient scalability.
Fine grained longer histories are important to model data for predictive analysis, anomaly detection, back-testing new models, finding long-term trends and correlations.

**Some Examples**

* Stock Trading and Time Series Data
> Exact timing of events is critical. 
> * The Chicago Mercantile Exchange in the US has around 100 million live contracts and handles roughly 14 million contracts per day. This level of business results in an estimated 1.5 to 2 million messages per second. This level of volume and velocity potentially produces that many time series points as well.  And there is an expected annual growth of around 33% in this market.
> * Processes such as algorithmic trading and high-frequency trading by institutions, hedge funds, and mutual funds can carry out large-volume trades in seconds without human intervention.
> Require highly perfromant and scalable
> This fine-grained view is important to meet government regulations for financial institutions and to be able to correlate trading behavior to other factors, including news events and sentiment analytics signals extracted from social me‐ dia. These new kinds of inputs can be very valuable in predictive an‐ alytics.

* Sensors + IoT
> * One of the largest usage for time-series data.
> * Grabage Container fill-up trend to come up with optimal schedule for trucks, maintainance
> * ATM empty schedule for Banks to watch-out for busy days

* Telecom Towers
> * Identify pattern of heavy usage trends; place mini towers to be activated in-time for that

* DataCenters
> trend analyze for outages by looking at factors For example, data on CPU usage, memory residency, IO activity, levels of disk storage, and many other parame‐ ters are all useful to collect as time series.

* Environment Monitoring

** Questions to be asked **
> * What short or long term trends need to be measured (prognostication)
> * How several measurements correlate over period of time (introspection)
> * How do I build a machine-learning model based on the temporal behavior of many measurements correlated to externally known facts? (prediction)
> * Have similar patterns of measurements preceded similar events? (introspection)
> * What measurements might indicate the cause of some event, such as a failure? (diagnosis)

---

#### Chapter.3 Storing and Processing TimeSeries Data

_preferred technology by authors HBase & MapR-DB_

Many parameters impact the decision:
> * How many time-series
> * What kind of data being acquired
> * What rate data is being acquired
> * For how long is data required


* Simplest Datastores - Flat Files
> * File format like Parquet
> * Number of timeseries in a single file increases, fraction of usable data for a timeseries query decreases
> * When paritition time is long, usable data in query decreases in a file
> * Keeping multiple files to tackle it will decrease efficiency of Apache Hadoop / HDFS based systems due to seek time
> * Better to move to a database

* Moving up to a Real DB - Will RDBMS suffice?
> * In RDBMS, details of series are stored in a dimension table; Fact Table storing time, seriesID and value.
> * Star Schema addresses the problem of having lots of different timeseries and can work well upto levels of 100s of millions and billions data points.
> * As of 2014, NASDAQ stock exchange handled billion trades in 3months. Operating conditions of a moderate sized cluster can produce half billion data points a day.
> Recording and Retrieval are different powers. Lower end of these sizes are ok but cost and complexity grow very fats.
> Changing it to NoSQL doesn't fix it quiet well. Real problem might be Star Schema.

* NoSQL DB with Wide Tables
> * Core problem with Star Schema is using one row per measurement
> * One technique maybe storing many values in a row; HBase have unbounded columns until kept to few hundred thousands.
> * Number of Rows to read reduces phenomenally
> * Columns are named by sample time offset within time window
> * Number of samples in each time window should be enough to cause significant decrease in rows


* NoSQL DB with Hybrid Design
> * Collapsing all of data for a row into a blob. Blob is more compressed so lesser to read.
> * In hybrid, some rows have been collapsed, some not

* Going one step further - Direct Blob Insertion Design
> * Keeping data samples in memory when they arrive and into 'restart logs' them as well.
> * Restart logs kept separately on HDFS and help reload in-mem data ingestion pipeline if need restarted
> * At end of time window in-mem structures used to create compressed data-blobs; discard log files
> Writing is much faster as well with it.
> This approach has been used to store 100million datapoints per second into 4-active node of a 10-node MapR-DB cluster on high-perf nodes with 16cores, lots RAM and 12 well-configured disk per node.


* Why Relational DBs aren't quiet right
> * Using blob-style storage, RDBMS will proivde reasonable rates. But running blob style of data-storage, most of the virtues of RDBMS are lost
> * SQL wouldn't be able to process data in any reasonable way; multi-row transaction will be unusable.
> * Although these features wouldn't be accessible but they will make scaling to multi-node configuration tough.
> * Can just an extra node on Apache HBase than moving to high-cost system like Oracle to make any usable system over it.
> As paying a penalty for features not used isn't even enterprise friendly.

* Hybrid Design - Where can I get one
> * OpenTSDB provides a practical implementation of hybrid wide/blob table design

---

#### Chapter.4 Solving a problem you didn't know you had

* Intro to OpenTSDB
OpenTSDB allows high data rates allowing wide row + blob store with HBase providing performance.

* Architecture of OpenTSDB

```
                            [Server(Collector)]    [Server(Collector)]
                                   |                       |
 [Grafana]---REST/HTTP--,      TSD RPC                  TSD RPC
                        |         .|,                     .|,
 [OpenTSDB UI]--HTTP----|------->[TSD]                   [TSD]
                        |            .\,                ./,
 [Scripts]--REST/HTTP---'             [HBase or MapR-DB]

* TSD : Time Series Daemon
```
> TSD lookup the timeseries to which the data gets appended and inserting each datpoint into storage tier as gets received
> Another thread in TSD replaces old rows with blob-formatted version in process known as row compaction

* Direct Blob Loading for High Perf
```
               -----------------------------.
 [collector]-->|-->[Data    ]-->[In-Memory] |
               |   |Gateway ]   [Cache    ] |
 [collector]-->|-->[API     ]        |      |
               |       |            .|,     |    _____________
               |       |        [Blob Maker]|-->[MapR-DB/HBASE]
               |       |                    |    -------------
               -----------------------------
                       |
                 ______|_____
                [Restart Logs]
                 ------------
```

* Rapid Loading of Historical Data
> * Direct Bulk Loader, loads data directly into storage tier in OpenTSDB format. This is highest peformant load path.
> * File Loader, 
> * TSD API for Bulk Loading
> * In-memory buffering for TSD

* Summary of Extensions for Direct Blob Loading

* Accessing Data with OpenTSDB

* Working on a higher level

* Accessing OpenTSDB data using SQL-on-Hadoop Tools

* Using Apache Spark SQL
> Once data is available to Spark SQL can directly take input as RDD (Resilient Distributed Dataset)

* Why not Apache Hive
> Hive need data in a very standardized format

* Nice Dashboards
> * [Grafan](http://grafana.org)
> * [Metrilyx](https://github.com/Ticketmaster/metrilyx-2.0)

---

#### Chapter.5 TimeSeries Data in Practical Machine Learning

* rapid loading of data

* using blob loader for direct insertion

---

#### Chapter.6 Advanced Topics for TimeSeries Databases

* Anomaly Detection
> Train to identify normal behavior


---

#### Chapter.7 What's Next

* IoT, Sensors

* High-Performance TSDB

---
---
