
## 01. Intro to Efficient Indexing

* an open full-text search engine, need to have a document storage strategy

* stores everything in Lucene, uses inverted index


### Getting Started

* `query1`: Push an example with `example_idx` as index, `eg_item` as Type & `1` as Id in URI:

```
curl -H "Content-Type: application/json" -XPUT \
  "${ES_URL}/example_idx/eg_item/1" \
  -d '{"title": "ES PUT/GET example", "category": "Elasticsearch", "content": "some curl requests", "date": "2021-03-31", "tags": ["elasticsearch", "example", "api"], "updated_by": ["John Doe"]}'
```

* `query2`: Fetch from `example_idx` index:

```
curl -XGET "${ES_URL}/example_idx/_search?pretty"
```


### Understanding document storage strategy

#### The `_source` field

* stores & indexes everything under `_source` field by default

> `_source` is metadata field auto-generated during indexing with Lucene & stores actual JSON doc

* can be updated for field to be search-able but not returned as in `query3` AND vice-versa as in `query5`

* `query3`: updates default mapping (excludes fields from `_source`) && re-run `query1` for re-indexing

```
curl -XPUT "${ES_URL}/example_idx/_mapping/eg_item" -d '{ "eg_item": { "_source": { "excludes": ["date", "updated_by"] }}}'
```

* `query4`: will use 'date' to filter but not return, as all fields are indexed by default

```
curl -XGET "${ES_URL}/example_idx/_search?pretty" -d '{"query": {"range": "date": {"gte": "2021-03-01"}} }}'
```

* similarly can have field returnable but hidden from search

#### Difference in Storable and Searchable field

* can have `_source` field disabled, set property mapping per field, which do have default value

* creating `example_idx` index again

```
curl -XDELETE ${ES_URL}/example_idx

## include_type_name is needed to nest under eg_item, else for default _doc is not needed
curl -XPUT -H "Content-Type: application/json" ${ES_URL}/example_idx?include_type_name=true -d '{
  "mappings": {
    "eg_item":{
        "_source": {"enabled": false},
        "properties": {
          "title": {"type": "text", "store": true},
          "categroy": {"type": "keyword"},
          "content": {"type": "text"},
          "date": {"type": "date", "index": false},
          "tags": {"type": "keyword", "index": false, "store": true}
        }
    }
  }
}'
```

> disabled the `_source` field for `eg_item` type; unless otherwise stated any fields of `eg_item` type are not stored/returned
>
> in this case we want to store `title` & `tags`; so can be returned
>
> `date` & `tags` are not indexed, so not searchable

* Re-index with `query1` again; `query2` shows 1 hits but no `_source` document and `query4` does not get any hits as `date` is not indexed now

* Searching to match `content` which gets indexed, since not disabled

```
# returns hits but no fields shown
curl -H "Content-Type: application/json" -XGET "${ES_URL}/example_idx/_search?pretty" -d'{"query": {"match": {"content": "curl"}}}'

# to get stored fields, gotta provide which one
curl -H "Content-Type: application/json" -XGET "${ES_URL}/example_idx/_search?pretty" -d'{"stored_fields": ["title", "tags"], "query": {"match": {"content": "curl"}}}'

# even providing content in stored_fields just returns title and tags, as store is not enabled for content so that is just indexed
curl -H "Content-Type: application/json" -XGET "${ES_URL}/example_idx/_search?pretty" -d'{"stored_fields": ["title", "content", "tags"], "query": {"match": {"content": "curl"}}}'
```

* 'type', 'store', 'index' allow you what to keep so can be returned & what to make relevant

* although disabling `_source` field takes away bunch of features, like UPDATE mapping API & highlighting


### Analysis

* all of Apache Lucene data is stored as an inverted index

* transformation reqd. by ES to search index, by process Analysis using ES's Index Analysis Module

* module maps to Lucene analyzer, Analyzer generally composed of single tokenizer & 1+ token filters

* ES provides a lot character filter, tokenizer & token filter

> * Example; character filter can strip out html markup, token filter can convert all lowercase;
>
> these can be combined to create custom or built-in analyzer can be used

* Lucene will use analyzer during indexing & query time, so better control at it, will improve performance

* all ES queries are not being analyzed.

* examine importance of analyzer in terms of relevant search

```
## let's index an anime entry for character type
curl -H "Content-Type: application/json" -XPOST \
  ${ES_URL}/anime/character?pretty \
  -d '{"fname": "Baki", "lname": "Hanma", "age": 17}'
## with fname giving 2 tokens
curl -H "Content-Type: application/json" -XPOST "${ES_URL}/anime/character?pretty" -d '{"fname": "Yujiro Ogre", "lname": "Hanma", "age": 45}'
## also fname giving 2 tokens
curl -H "Content-Type: application/json" -XPOST "${ES_URL}/anime/character?pretty" -d '{"fname": "Sugar-Ray", "lname": "Robinson", "age": 50}'

## searching character with firstname 'baki'
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" \
  -d '{"query": {"match": {"fname": "baki"}}}'
```

> since dynamic mapping were created, standard analyzer will analyze all string type fields for search above

* Standard Analyzer is default picked if type are not specified; removes punctuation and splits text like some `fname` above get two tokens

```
## to see how standard analyzer works
curl -H 'Content-Type: application/json' -XGET "${ES_URL}/_analyze?pretty" -d '
{ "analyzer" : "standard", "text" : "Baki Hanma!" } '

curl -H 'Content-Type: application/json' -XGET "${ES_URL}/_analyze?pretty" -d '
{ "analyzer" : "standard", "text" : "Sugar-Ray" } '

## keyword analyzer works as not_analyzed for index in older version; returns input as one
 curl -X GET "localhost:9200/_analyze?pretty" -H 'Content-Type: application/json' -d'
{ "analyzer": "keyword", "text": "James Brown!" }'
## {"tokens": [{ "token": "James Brown!", "start_offset": 0, "end_offset": 12, "type": "word", "position": 0 }]}

curl -X GET "localhost:9200/_analyze?pretty" -H 'Content-Type: application/json' -d'
{ "analyzer" : "fingerprint", "text" : "Quick Brown Foxes!" }'
## {"tokens": [{"token": "brown james", "start_offset": 0, "end_offset": 12, "type": "fingerprint", "position": 0}]}
```

* if you recreate `anime` with explicit mapping for `fname` to be checked as it is

```
## remove index
curl -XDELETE ${ES_URL}/anime

## create with fname to be analyzed as keyword
curl -XPUT -H "Content-Type: application/json" ${ES_URL}/anime -d '{
  "mappings": {
      "properties": {
        "fname": {"type": "text", "analyzer": "keyword"}
      }
  }
}'

## re-index
curl -H "Content-Type: application/json" -XPOST "${ES_URL}/anime/_doc?pretty" -d '{"fname": "James Joseph", "lname": "Brown", "age": 73}'
echo && sleep 1

## now partial search and lowercase wouldn't work
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'{"query": {"match": {"fname": "james"}}}'
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'{"query": {"match": {"fname": "james joseph"}}}'

## full value search like keyword would work
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'{"query": {"match": {"fname": "James Joseph"}}}'
```

* `match_phrase_prefix` would still work for keyword, but means only prefix partials again case-sensitive

```
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'{"query": {"match_phrase_prefix": {"fname": "James"}}}'
```

---
