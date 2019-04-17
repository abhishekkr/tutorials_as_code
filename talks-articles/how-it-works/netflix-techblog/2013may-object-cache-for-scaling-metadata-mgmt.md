
## Object Cache for Scaling Video Metadata Management

> 8/May/2013 | [source](https://medium.com/netflix-techblog/object-cache-for-scaling-video-metadata-management-c3c17830983e)


### Key requirements to address

* over 100 billion requests/day with low latency

* large data size of 20-30 GB across countries and devices

* work with highly comples metadata processing

* quick start-up times for efficient auto scaling


### Initial cloud deployment

* a server interacts with RDBMS, generates data snapshots periodically and uploads them to S3

* one server processes data per country and generate data snapshots after data correlation, applying business and filter logic

* client-side object cache loads relevant snapshots as per app config and makes data available

* cache is refreshed periodically with deserialization and further optimization on client-side

> with expansion to multi countries, issues like overhead of servers and data duplication happened


### Few changes for multi-region expansion issues

* streamlined server to be structured around collection of countries (islands)

* moved metadata process and de-duplication to server-side

* enabled operationally easier config based ib what metadata an app was interested in

---
