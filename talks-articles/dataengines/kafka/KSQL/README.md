
## KSQL

> the streaming SQL engine for Apache Kafka

[source](https://github.com/confluentinc/ksql)

* KSQL 0.5 released Feb/2018 with bug fixes and perf improvements

* Apache 2.0 licenses

* provides simple and completely interactive SQL interface for stream processing on Kafka.

* should be distributed, scalable, reliable and real-time

* allows wide range processing operations like aggragations, joins, windowing, sessionization, etc.

---

* [Quickstart](./quickstart.md)

---

#### examples

* streaming ETL

```
CREATE STREAM page_actions AS
  SELECT userid, page, action
  FROM clickstream c
  LEFT JOIN users u ON c.userid = u.userid
  WHERE u.level = 'GoldStar';
```


* anomaly detection

```
CREATE TABLE possible_fraud AS
  SELECT card_number, count(*)
  FROM authorization_attempts
  WINDOW TUMBLING (SIZE 5 SECONDS)
  GROUP BY card_number
  HAVING count(*) > 3;
```


* monitoring

```
CREATE TABLE error_counts AS
  SELECT error_code, count(*)
  FROM monitoring_stream
  WINDOW TUMBLING (SIZE 1 MINUTE)
  WHERE  type = 'ERROR'
  GROUP BY error_code;
```

---
