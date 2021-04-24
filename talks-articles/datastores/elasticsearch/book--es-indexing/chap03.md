
## 03. Basic Concepts of Mapping

### Basic concepts and definitions

* how a field is tokenized, analyzed and searched is mapping mechanism

#### Metadata Fields

> document has auto-generated metadata fields

* `_source`, original JSON representing the body of document

* **v5** used to have `_all` with all fields included in it; now can use `copy_to` mapping to create a combination field

```
curl -XPUT -H "Content-Type: application/json" ${ES_URL}/anime -d '{
  "mappings": {
      "properties": {
        "fname": {"type": "text", "copy_to": "_fullname"},
        "lname": {"type": "text", "copy_to": "_fullname"},
        "verified_by": {"type": "text"},
        "_fullname": {"type": "text"}
      }
  }
}'

## re-index
curl -H "Content-Type: application/json" -XPOST "${ES_URL}/anime/_doc?pretty" -d '{"fname": "James Joseph", "lname": "Brown", "age": 73, "verified_by": "Soul Society"}'

echo && sleep 1

## fetchs
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'
{"query": {"match": {"_fullname": "brown"}}}'

## misses, since verified_by is not in _fullname
curl -H "Content-Type: application/json" -XGET "${ES_URL}/anime/_search?pretty" -d'
{"query": {"match": {"_fullname": "soul"}}}'
```

* `_timestamp` field used to be there which was enabled, is **deprecated** with suggestion to introduce explicit date fields when needed

* `_ttl` were to allow set time-to-live for documents; **deprecated** with suggestion to use ILM (`Index Lifecycle Management`) for a more high-level control over indices


### Types

> document field must be configured with appropriate data type

#### Object Type

* JSON doc can have inner nested objects, makes them searchable

* using `dot` notation

```
curl -XPOST "localhost:9200/twitter/_doc?pretty" -H 'Content-Type: application/json' -d'
{
  "tweet": {
    "content": "this will be not RTd",
    "posted": {
      "user": {
        "email": "abc@xyz.com", 
        "name": {"fname": "Ab", "lname": "Cc", "handle": "abc"}
      },
      "date": "2021-03-31"
    },
    "hashtags": ["2021"]
  }
}
'
echo "" && sleep 1

## specifying nested object
curl -XGET "localhost:9200/twitter/_search?pretty" -H 'Content-Type: application/json' -d'
{"query": {"match": {"tweet.posted.user.name.handle": "abc"}}}
'
```

* can turn dynamic mapping off for inner objects to avoid malformed objects with incompatible data-type or format

> like specifying if a field is `date` if parse-able, or just string/number for some explicit parse-able data

```
curl -XDELETE ${ES_URL}/twitter
echo "" && sleep 1

curl -XPUT "localhost:9200/twitter?pretty" -H 'Content-Type: application/json' -d'{
  "mappings": {
    "properties": {
      "tweet": {
        "type": "object",
        "properties": {
          "content": {"type": "text"},
          "posted": {
            "type": "object",
            "properties": {
              "user": {
                "type": "object",
                "properties": {
                  "name": {
                    "dynamic": false,
                    "type": "object",
                    "properties": {
                      "handle": {"type": "text"}
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
'

curl -XPOST "localhost:9200/twitter/_doc?pretty" -H 'Content-Type: application/json' -d'{
  "tweet": { "content": "this will be not RTd",
    "posted": {
      "user": { "email": "abc@xyz.com", 
        "name": { "fname": "Ab", "lname": "Cc", "handle": "abc"}
      },
      "date": "2021-03-31"
    },
    "hashtags": ["2021"]
  }
}
'

echo "" && sleep 1
curl -XGET "localhost:9200/twitter/_mapping?pretty"
```

* above dynamic mapping being turned off for `name` object avoids adding of `fname` & `lname` fields in mapping; though they can be searched and received in result

> * `dynamic` can also have value `runtime`, if need new fields to be added as Runtime fields (not indexed, loaded from `_source` at query)
>
> * `dynamic` can also have value `strict`, to throw exception if need avoid additional fields altogether
>
> * can have `dynamic` set to a value in top object, then overridden in inner object for flexibility

* Root object provides a type level configuration for fields that get inherited by all, unless overridden

> like specify format for date fields; these formats are not used for date fields specified in mapping

```
curl -XPUT "localhost:9200/twitter?pretty" -H 'Content-Type: application/json' -d'{
  "mappings": {
    "dynamic_date_formats": ["yyyy-MM-dd", "dd-MM-yyyy"]
  }
}'
```

* `attachment` type allows indexing at a field encoded as BASE64


### The relationship between mapping and relevant search results

> by default now a string field gets mapped as `text` and `keyword upto-256` both

* when using default standard analyzer, searching for `html 5` would make two tokens `[html, 5]` and return documents containing `html`

* date histograms can only be applied on type-date values, so might fail for dynamic mapped fields


### Understanding the schema-less

* don't need to pre-define fields

* once a field has been added, its type can't be changed

---
