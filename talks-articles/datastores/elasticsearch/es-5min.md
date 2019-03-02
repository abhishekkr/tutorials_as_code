
## ElasticSearch in 5 minutes

[source](http://www.elasticsearchtutorial.com/elasticsearch-in-5-minutes.html)

* Can install and start ElasticSearch service or just pull and start its docker.

> You can access it to check validity of running service. It will return status, build details, few extra info.

```
curl http://localhost:9200
```


### Concepts

* Uses `inverted index`, inverts page-centric data structure (page-\>words) to a keyword-centric data structure (word-\>pages). Uses Lucene for it.

* Document is unit of search and index. Index consists one or more documents and a document consists of one or more fields.

* ES is schema free. Not required to specify schema before indexing documents unless require anything but the most basic fields and operations.


### Indexing Data

* Creating indexes, can be done via json over http or native client.

```
curl -XPUT 'http://localhost:9200/blog/user/bond' -d '{"name": "James Bond"}'

curl -XPUT 'http://localhost:9200/blog/post/1' -d '{"user": "dilbert",
    "postDate": "2011-12-15",
    "body": "Search is hard. Search should be easy." ,
    "title": "On search"
}'

curl -XPUT 'http://localhost:9200/blog/post/2' -d '{"user": "dilbert",
    "postDate": "2011-12-12",
    "body": "Distribution is hard. Distribution should be easy." ,
    "title": "On distributed search"
}'
```

> On success these will return response similar to `{"ok":true, "_index":"blog", "_type":"post", "_id":"1", "_version":1}`.


* Verifying created data

```
curl -XGET 'http://localhost:9200/blog/user/bond?pretty=true'
curl -XGET 'http://localhost:9200/blog/post/1?pretty=true'
curl -XGET 'http://localhost:9200/blog/post/2?pretty=true'
```


### Searching

* Retrieving all posts by user in get query, along with metadata.

```
curl 'http://localhost:9200/blog/post/_search?q=user:bond&pretty=true'
```

* Returns all posts which don't have a term, say `search` in `title`.

```
curl 'http://localhost:9200/blog/post/_search?q=-title:search&pretty=true'
```

* Returns all posts which don't have a term and have other, say not `search` but have `distributed`.

```
curl 'http://localhost:9200/blog/post/_search?q=-title:search%20+title:distributed&pretty=true&fields=true'
```

* Range search on postDate

```
curl -XGET 'http://localhost:9200/blog/_search?pretty=true' -d '
{
  "query": {
    "range": {
      "postDate": {"from": "2011-12-10", "to": "2011-12-12"}
    }
  }
}'
```

---
---
