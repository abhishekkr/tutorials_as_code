## An Introduction to Apache Kylin - BI Meets Big Data
> by Fabian Wilckens
> at Berlin Buzzwords 2015

[kylin.io](http://kylin.io)

Extreme OLAP Engine for Big Data.
> OpenSource Distributed Analytics Engine (engineered at eBay) providing SQL interface and multi-dimensional analysis (OLAP) on Hadoop for large datasets.

### OLAP

OLAP : Online Analytical Processing

> If you wanna query like
> How many beers were ordered in Germany on yearly basis?
> How many beers were ordered in Germany on yearly basis, broken down by months?

* OLAP Cubes

```
     Places(Berlin,Hamburg,Munich)
       ^
       |
       |
       +------> Timeline (March,April,May)
      /
     /
    L
 Items (Coffee,Water,Beer)
```

It consists of 2 basic things
* Measures (Numeric Facts)
* Products

---

### Goals

* Sub-second query latency on billions of rows
* ANSI SQL for both analysts and engineers
* Full OLAP capability to offer advanced functionality
* Seamless integration with BI Tools

* Support for High Cardinality nd dimensionality
* High concurrency, 1000s consumers
* Distributed and scale out architecture for large data volumes

---

### Depends on Hadoop Ecosystem

* Hive
> Input source, pre-join star schema during cube building

* MapReduce
> Aggregate metrics during Cube building

* HDFS
> Store intermediate files during Cube building

* HBase
> Store and query data cubes

* Calcite
> SQL parsing, code generation, optimization

```
                     _____________    _____________________
                    [3rd Party APP]  [ SQL Tools           ]
                    [WebApp,Mobile]  [(BI Tools: Tableau..)]
                    [=REST API====]  [===JDBC/ ODBC========]
                        '|             '|
                     ____|.SQL__________|.SQL         Low Latency - Seconds
                    [ REST Server        ]      ,------<>--------,
                    [ Query Engine       ]      |             ____|.___
      ,----<>------>[===Routing==========]<-----'           [  _______]_
      |,                                                     [ [ OLAP    ]
  [<Hadoop Hive>]   [<<Metadata>>>>>>>>>>]                   [_[ CUBE    ]
      |              ____________________                      [_(HBase)_]
      '----@------->[ Cube Build Engine  ]------------@---------'
   Star Schema Data [_(MapReduce...)_____]      Key Value Data

 * Online Analysis Data Flow (---<>---)
 * Offline Data Flow (---@---)
 * OLAP Cube is transparent to users, interact via SQL

```

---

* Thinking of moving to Drill from Hive
* Introducing Spark capabilities

---
---
