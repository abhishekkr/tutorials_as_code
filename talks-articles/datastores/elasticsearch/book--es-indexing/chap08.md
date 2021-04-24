
## 08. Improving the User Search Experience

* let's create some sample documents in an index, to re-index for term/phrase suggester

```
ES_URL="http://localhost:9200"

curl -XDELETE "${ES_URL}/movie"
curl -XDELETE "${ES_URL}/cinema"
echo "" && sleep 1

curl -XPOST "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "movie", "_id": 1 } }
{"title": "The Lord of the Rings: The Fellowship of the Ring", "release_year": "2001", "genre": "fantasy"}
{ "index" : { "_index" : "movie", "_id": 2 } }
{"title": "The Lord of the Rings: The Two Towers", "release_year": "2002", "genre": "fantasy"}
{ "index" : { "_index" : "movie" } }
{"title": "The Lord of the Rings: The Return of the King", "release_year": "2003", "genre": "fantasy"}
'

curl -XPUT "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "create" : { "_index" : "movie", "_id": 11 } }
{"title": "The Matrix", "release_year": "1999", "genre": "sci-fi"}
{ "create" : { "_index" : "movie", "_id": 12 } }
{"title": "The Matrix Reloaded", "release_year": "2003", "genre": "sci-fi"}
{ "create" : { "_index" : "movie", "_id": 13 } }
{"title": "The Matrix Revolutions", "release_year": "2003", "genre": "sci-fi"}
'

curl --request PUT 'http://localhost:9200/cinema_term' \
--header 'Content-Type: application/json' \
-d '{
   "mappings": {
       "properties": {
           "title": {"type": "search_as_you_type"},
           "genre": {"type": "search_as_you_type"}
       }
   }
}' ; echo "" && sleep 1

curl --request PUT 'http://localhost:9200/cinema_phrase' \
--header 'Content-Type: application/json' \
-d '{
   "mappings": {
       "properties": {
           "title": {"type": "search_as_you_type"},
           "genre": {"type": "search_as_you_type"}
       }
   }
}' ; echo "" && sleep 1

curl --silent --request POST 'http://localhost:9200/_reindex?pretty' --header 'Content-Type: application/json' --data-raw '{
 "source": {"index": "movie"},
 "dest": {"index": "cinema_term"}
}' | grep "total\|created\|failures"

curl --silent --request POST 'http://localhost:9200/_reindex?pretty' --header 'Content-Type: application/json' --data-raw '{
 "source": {"index": "movie"},
 "dest": {"index": "cinema_phrase"}
}' | grep "total\|created\|failures"

curl -XGET "localhost:9200/cinema_term/_mapping?pretty"
```

### Correction of users' spelling mistakes

#### Suggesters

* `_suggest` API has been **DEPRECATED**

#### Using the `_search` REST endpoint

* checkout [this link](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-suggesters.html) for detailed examples on `phrase` & `completion` capabilities

* can use `suggest` block alongside `query` in `_search` Request; this returns documents also which are independent of suggestions

#### term Suggester

* suggests terms based on `edit distance` i.e. char count to be changed to make suggestion

```
## term suggestion with a query
curl -X POST "localhost:9200/cinema_term/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query" : {
    "match": {
      "title": "Felowship"
    }
  },
  "suggest" : {
    "my-suggestion" : {
      "text" : "Felowship",
      "term" : {
        "field" : "title"
      }
    }
  }
}
'

## multiple term suggestions
curl -X POST "localhost:9200/cinema_term/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "suggest" : {
    "title-x" : {
      "text" : "Felowship",
      "term" : {"field" : "title", "size": 1}
    },
    "title-y" : {
      "text" : "Revplution",
      "term" : {"field" : "title", "size": 1}
    }
  }
}
'
```

* common suggest options are `text`, `field`, `analyzer`, `size` (max suggestion count, default 5), `sort` (default score or frequency), `suggest_mode` (default missing or popular or always)

* other options are `lowercase_terms`, `max_edits`, `prefix_length`, `min_word_length`, `shard_size`, `max_inspections`, `min_doc_freq`, `max_term_freq`



#### phrase Suggester

* following phrase suggester suggests `fellowship of the`, `felowship of the` and `fellowship of tthe` with varying `score`

```
curl -X POST "localhost:9200/cinema_phrase/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query" : {
    "match": {
      "title": "Felowship"
    }
  },
  "suggest" : {
    "my-suggestion" : {
      "text" : "Felowship of tthe",
      "phrase" : {
        "field" : "title",
        "highlight": {
          "pre_tag": "<em>",
          "post_tag": "</em>"
        }
      }
    }
  }
}
'
```

* has options like `gram_size` (1 if field is not using shingle filter), `max_errors`, `size`, `analyzer`, `highlight`, `collate`

* `pre_filter` & `post_filter` can be used to inject synonyms, which allows suggesting `captain america` for `captain usq`

* adds additional logic on `term` suggester to select entire phrases instead of tokens weighted based on `ngram-language` models

* better to have special mapping up front for `n-gram` shingles, [refer API Example here](https://www.elastic.co/guide/en/elasticsearch/reference/current/search-suggesters.html#_api_example)

#### completion Suggester

* provides basic auto-complete instead of spell corrector

* prefix suggester based on `FST` (Finite State Transducer) data structure; stores as part of index during index-ing time thus faster than other suggestions

```
curl -X PUT "localhost:9200/cinema_completion?pretty" -H 'Content-Type: application/json' -d'
{
  "mappings": {
    "properties": {
      "title": {"type": "completion"},
      "genre": {"type": "keyword"}
    }
  }
}
'
echo "" && sleep 1

curl -XPOST "localhost:9200/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "cinema_completion", "_id": 1 } }
{"title": {"input": "The Lord of the Rings: The Fellowship of the Ring", "weight": 3}, "release_year": "2001", "genre": "fantasy"}
{ "index" : { "_index" : "cinema_completion", "_id": 2 } }
{"title": {"input": ["The Lord of the Rings: The Two Towers", "LOTR2"], "weight": 2}, "release_year": "2002", "genre": "fantasy"}
{ "index" : { "_index" : "cinema_completion" } }
{"title": {"input": ["The Lord of the Rings: The Return of the King", "LOTR3"], "weight": 1}, "release_year": "2003", "genre": "fantasy"}
'

curl -XPUT "localhost:9200/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "create" : { "_index" : "cinema_completion", "_id": 11 } }
{"title": {"input": "The Matrix", "weight": 3}, "release_year": "1999", "genre": "sci-fi"}
{ "create" : { "_index" : "cinema_completion", "_id": 12 } }
{"title": [{"input": "The Matrix Reloaded", "weight": 2}, {"input": "The Matrix Revolutions", "weight": 1}], "release_year": "2003", "genre": "sci-fi"}
'
echo "" && sleep 1

curl -X POST "localhost:9200/cinema_completion/_search?pretty&pretty" -H 'Content-Type: application/json' -d'
{
  "suggest": {
    "movie-suggest": {
      "prefix": "LOTR",
      "completion": {"field": "title"}
    }
  }
}
'

## can manage a little mistake, still by prefix
curl -X POST "localhost:9200/cinema_completion/_search?pretty&pretty" -H 'Content-Type: application/json' -d'
{
  "suggest": {
    "movie-suggest": {
      "prefix": "Tje Matrix",
      "completion": {
        "field": "title",
        "fuzzy": {"fuzziness": 1}
      }
    }
  }
}
'
```

* can have `"skip_duplicates": true` option; also can use `regex` in-place of `prefix` option for a pattern of it


### Get suggestions

#### Context Suggesters

* if used context need to be provided while query as well

* if `path` has been set for contexts, can be added as along-side field

```
curl -XDELETE "${ES_URL}/cinema_ctx"

curl -X PUT "localhost:9200/cinema_ctx?pretty" -H 'Content-Type: application/json' -d'
{
  "mappings": {
    "properties": {
      "title": {
        "type": "completion",
        "contexts": [
          {"name": "genre", "type": "category", "path": "genre"},
          {"name": "location", "type": "geo", "precision": 4, "path": "loc"}
        ]
      },
      "loc": {"type": "geo_point"}
    }
  }
}
'
echo "" && sleep 1

curl -XPOST "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "cinema_ctx", "_id": 1 } }
{"title": {"input": "The Lord of the Rings: The Fellowship of the Ring", "weight": 3, "contexts": {"genre": ["fantasy"]}}, "release_year": "2001"}
{ "index" : { "_index" : "cinema_ctx", "_id": 2 } }
{"title": {"input": ["The Lord of the Rings: The Two Towers", "LOTR2"], "weight": 2, "contexts": {"genre": ["fantasy"]}}, "release_year": "2002", "genre": "fantasy"}
{ "index" : { "_index" : "cinema_ctx" } }
{"title": {"input": ["The Lord of the Rings: The Return of the King", "LOTR3"], "weight": 1, "contexts": {"genre": ["fantasy", "action"]}}, "release_year": "2003", "genre": "fantasy"}
'

curl -XPUT "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "create" : { "_index" : "cinema_ctx", "_id": 11 } }
{"title": {"input": "The Matrix", "weight": 3}, "release_year": "1999", "genre": "action"}
{ "create" : { "_index" : "cinema_ctx", "_id": 12 } }
{"title": [{"input": "The Matrix Reloaded", "weight": 2}, {"input": "The Matrix Revolutions", "weight": 1}], "release_year": "2003", "genre": "scifi"}
'

echo "" && sleep 1

curl -X POST "localhost:9200/cinema_ctx/_search?pretty&pretty" -H 'Content-Type: application/json' -d'
{
  "suggest": {
    "movie-suggest": {
      "prefix": "LOTR",
      "completion": {"field": "title", "contexts": {"genre": ["action"]}}
    }
  }
}
'

curl -X POST "localhost:9200/cinema_ctx/_search?pretty&pretty" -H 'Content-Type: application/json' -d'
{
  "suggest": {
    "movie-suggest": {
      "prefix": "Tje Matrix",
      "completion": {
        "field": "title",
        "contexts": {"genre": [
          {"context": "action"},
          {"context": "scifi", "boost": 2}
        ]},
        "fuzzy": {"fuzziness": 1}
      }
    }
  }
}
'
```


### Improving the relevancy of search results

> can use re-scoring to improve query's relevance

#### Boosting the query

* allows to demote results that match a given query; to send irrelevant records to back

```
## sends the Fellowship movie suggestion to last
curl -X GET "localhost:9200/movie/_search?pretty" -H 'Content-Type: application/json' -d'
{
  "query": {
    "boosting": {
      "positive": {"term": {"title": "lord"}},
      "negative": {"term": {"title": "fellowship"}},
      "negative_boost": 0.5
    }
  }
}
'
```

#### Bool query

* `bool` allows combination of nested queries

* provides a `should` occurence type, each matching `should` increases document score

* can configure `minimum_should_match`; also a `boost` can be added to a `should`

```
## pulls Return of King to top, then Fellowship, then rest
curl -H 'Content-Type: application/json' -XGET "localhost:9200/movie/_search?pretty" -d '{
  "query": {
    "bool": {
      "must": [{"match": {"title": "Lord"}}],
      "should": [
        {"match": {"title": {"query": "King", "boost": 2}}},
        {"match": {"title": {"query": "Fellowship"}}}
      ]
    }
  }
}'
```


#### Synonyms

* create a mapping with `tr_synonyms` analyzer

```
curl -XDELETE "localhost:9200/where"
echo "" && sleep 1

curl -H 'Content-Type: application/json' -XPUT "localhost:9200/where" -d '{
  "settings": {
    "analysis": {
      "filter": {
        "tr_synonym_filter": {
          "type": "synonym",
            "synonyms": [
              "tr,turkey",
              "in,india",
              "usa,america"
            ]
        }
      },
        "analyzer": {
          "tr_synonyms": {
            "tokenizer": "standard",
            "filter": [
              "lowercase",
            "tr_synonym_filter"
            ]
          }
        }}
  },
    "mappings": {
      "properties": {
        "place": {"type": "text", "analyzer": "tr_synonyms"}
      }
    }
}'
echo "" && sleep 1

curl -H 'Content-Type: application/json' -XPOST "localhost:9200/where/_doc/1" -d '{"place": "Somewhere in Turkey"}'
curl -H 'Content-Type: application/json' -XPOST "localhost:9200/where/_doc/2" -d '{"place": "Now in India"}'
curl -H 'Content-Type: application/json' -XPOST "localhost:9200/where/_doc/3" -d '{"place": "Why in America"}'
echo "" && sleep 1

## querying with synonym
curl -H 'Content-Type: application/json' -XGET localhost:9200/where/_search?pretty -d '{
  "query": {
    "match": {
      "place": "tr"
    }
  }
}'
```


#### Be careful about the `_all` field

> `copy_to` used on synonym field wouldn't always work with query

---
