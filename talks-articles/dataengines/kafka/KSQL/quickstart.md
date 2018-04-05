
## Quickstart

[source](https://github.com/confluentinc/ksql/tree/master/docs/quickstart#quick-start)

* KSQL queries data in Kafka cluster. So we'll need a full on Kafka cluster with broker and ZK nodes.
> Will need messages in there, can use [this](https://github.com/confluentinc/ksql/blob/v0.5/docs/quickstart/quickstart-non-docker.md#produce-topic-data) for test data.

* Starting KSQL will present with a REPL prompt to run SQL.

#### Create a STREAM and TABLE

If there is a topic `pageviews` with schema `{rowtime, rowkey, viewtime, pageid, userid}`.
And a topic `users` with schema `{rowtime, rowkey, registertime, userid, regionid, gender}`.

* Creating, describe and show Streams

```
CREATE STREAM pageviews_original
  (viewtime bigint, userid varchar, pageid varchar)
  WITH (kafka_topic='pageviews', value_format='DELIMITED');

CREATE TABLE users_original
  (registertime bigint, gender varchar, regionid varchar, userid varchar)
  WITH (kafka_topic='users', value_format='JSON', key = 'userid');

DESCRIBE pageviews_original;
DESCRIBE users_original;

SHOW STREAMS;
SHOW TABLES;

SELECT pageid FROM pageviews_original LIMIT 3;
```

---

#### Write Queries

* by default KSQL reads streams and tables from latest offset

* create persistent query using `SELECT` preceded by `CREATE STREAM` as

```
CREATE STREAM pageviews_female
  AS SELECT users_original.userid AS userid, pageid, regionid, gender
  FROM pageviews_original
  LEFT JOIN users_original ON pageviews_original.userid = users_original.userid
  WHERE gender = 'FEMALE';

DESCRIBE pageviews_female;
```


* create persistent query using `SELECT` preceded by `CREATE STREAM` with condition as

```
CREATE STREAM pageviews_female_like_89
  WITH (kafka_topic='pageviews_enriched_r8_r9', value_format='DELIMITED')
  AS SELECT * FROM pageviews_female
  WHERE regionid LIKE '%_8' OR regionid LIKE '%_9';

DESCRIBE pageviews_female_like_89;
```


* create persistent queery for each region-x-gender combo in tumbling window of 30sec when count is more than 1
> results pushed to kafka topic `pageviews_regions` in Avro format

```
CREATE TABLE pageviews_regions WITH (value_format='avro')
  AS SELECT gender, regionid , COUNT(*) AS numusers FROM pageviews_original
  WINDOW TUMBLING (size 30 second)
  GROUP BY gender, regionid
  HAVING COUNT(*) > 1;
```


* show all persistent queries

```
SHOW QUERIES
```

---

#### Terminate and Exit

* Exiting KSQL doesn't terminate persistent queries.

* Identify query-id using `SHOW QUERIES` and terminate using `TERMINATE <query-id>`

* exit from KSQL using `exit`

---
