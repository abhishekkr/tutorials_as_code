## Building a Forex trading platform using Kafka, Storm andd Cassandra

Here is high level overview of data pipeline built at Insight to handle Forex data for algorithmic trading, viz and batch aggregation jobs.

> [source](http://insightdataengineering.com/blog/Building_a_Forex_trading_platform_using_Kafka_Storm_Cassandra.html)

* [@github](https://github.com/slawekj/wolf)

Established financial institutions use expensive systems for trades like [ulra-low latency direct market access software](http://en.wikipedia.org/wiki/Direct_market_access).

New OSS solution [WOLF](https://github.com/slawekj/wolf) can help with forex trading by visualizing [financial data in realtime](https://www.youtube.com/watch?v=0Q5XMwENRuY#t=23), [executing trade orders with little latency](http://youtu.be/y7Gr5F1FHco?list=UU5KnJYd4JU21Qu8E8GgEexg), and analyze historical events off-line.
It seamlessly integrates with external brokers and data providers.

* Wolf's Architecture

```ASCII

            [:: CLIENTS ::]
      _______,/_________\,_______________
  __  |   [WebUI]---->[Rule API]------- |
 |  | |         \,                    | |
 |  | |       [  Cache  ]             | |
 |B | |           '|'                 | |
 |R | | [:Rule ] [Realtime] [Aggreg-] | |
 |O <-+-[Engine] [Visual- ] [-gator ] | |
 |K | | [______] [-zation_] [_______] | |
 |E | |     '\`    '|`      '/`       | |
 |R | |   [`` MULTIPLEXER``````]<-----' |
 |  | |   [____________________]        |
 |__| |___________'|`___________________|
                   |
      [:::::::: DATA PROVIDER ::::::::::]

```

* Inputs

Wolf processes 2 types of inputs. Updates to conversion rates of seven main currency paires and trading orders from investors.
First type input comes from 'DATA PROVIDER" service. Second type from 'Rule API'.
First type input is extracted from data aggregated by [HistData.com](http://www.histdata.com/), served with resolution of 1tick/millisecond.
Multiplexer is implemented using Kafka. It's a persistent queue, resilient to hardware failures and tunable capacity. Allows buffering.

* Routing data with different velocities of Kafka
There are 3 classes of consumers for events; a 'Rule Engine', 'Realtime Visualization' and 'Batch aggregation service'.
'Rule Engine' pulls every millisecond, not interrupting 'Realtime Visualization' which consumes every 500ms. Aggregator consumes every 15minutes.

* Data pipeline for the rule engine: very fast
Trading orders are in terms of 'if then' rules. Implemented atop 'Storm event processor'.
It allows creating a custom processing flow 'topology' based on rules, like
```ASCII
[RulesStream 2.25ms]-- default: 360: 42% ----,
                                              ===[ExecutionBolt 0.53ms]
[TicksStream 0.79ms]-- default: 500: 58% ----'
```
Storm does serializing, routing and replay of events from source in failure case.


* Data pipeline for the realtime visualization: fast
It aggregates latest updates to market for 4hrs. As events comes sorted by timestamp, 'Cassandra' gets used as it uses sorted string tables.


* Data pipeline for the batch aggregation: slow
It's designed to store all historical events, hundreds of terabytes of data. 'Camus' used to collected data from 'Kafka' and persist to 'Hadoop'. Then 'Hive' to calculate aggreagted views.

* [Flot](http://www.flotcharts.org/) a javascript library plotting real-time series in a browser

---
