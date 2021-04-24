
## 04. Analysis and Analyzers

### Introducing Analysis

* to get the real meaning of data, must analyze.. if a search engine doesn't predict what we're looking for, we use another search engine


### Process of Analysis

* process of transforming data is `analysis` with 2 steps: `tokenizing` and `normalizing`

* generally an analyzer is composed of tokenizer and one/more token filters


### Built-in Analyzers

* `standard`: grammar based tokenization on Unicode Text Segmentation algorithm

* `simple`: breaks at any non-letter, gets rid of non-letter, turns to lowercase

* `stop`: same as `simple` with support for stopwords

* `keyword`: is a `noop` analyzer, returns Input as it is

* `fingerprint`: lowercased & normalized to remove extended characters; then sorted, deduplicated and concatenated into a token (if stop word configured, gets removed)

* `whitespace`: breaks text into terms at every whitespace

* `pattern`: uses a regex to tokenize text

* `language`: aimed at specific language text

#### Building blocks of Analyzer

* token-filter of tokenizer starts breaking text into tokens, once text is passed through character filter

* ES has built-in character filters, custom can be used as well

> * `uax_url_email`, works as standard tokenizer except understands urls/emails as one token

```
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"Âşıklar ölmez!", "tokenizer": "letter"}'

## sends 6 tokens, as BOLD tag gets analyzed
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"i am at <b>abc</b>"}'

## sends 4 tokens, as BOLD tag html_strip filtered
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"i am at <b>abc</b>", "char_filter": ["html_strip"]}'
```


#### Tokenizer

* several built-in tokenizers

* Word Oriented Tokenizers: `standard`, `letter`, `lowercase`, `whitespace`, `uax_url_email`, `classic`, `thai`

* Structured Text Tokenizers used with special text items like Phone-Numbers, etc.

* Pattern replace char filter allows using regex to manipulate characters

```
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"i am at <b>abc@xyz.com</b>", "tokenizer": "uax_url_email"}'

## gives 5 tokens, splits at @ as well
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"i am at abc@xyz.com", "tokenizer": "standard"}'

## gives 4 tokens, as understands email
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d '{"text":"i am at abc@xyz.com", "tokenizer": "uax_url_email"}'
```

#### Token Filters

* accept stream from tokenizer and modify them (like change case, remove stopwords, add synonyms)

* has multiple built-in token filters as

> * `apostrophe`: remove everything in a token after apostrophe including apostrophe; but not the next token
>
> * `asciifolding`: converts characters to their ASCII equivalent
>
> * `length`: to only accept tokens in range of min-to-max char length
>
> * `lowercase` & `uppercase`: normalize token to desired case
>
> * `trim`: removes whitespace surrounding a token like Keyword
>
> * `truncate`: removes characters in token after desired length
>
> * `word_delimiter_graph`: splits token at non-alphanum and can perform token normalization based on certain rules
>
> * `remove_duplicates` can be used if `keyword_repeat` & `stemmer` are in use, to avoid duplicates
>
> * `reverse`: for types where token are better searched based on suffix, like filenames by extension
>
> * `stop`: to avoid certain stopwords
>
> * `limit`: to limit token count, default keeps only first

```
## gives 2 tokens [i, abc@xyz.com]
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d "{\"text\":\"i'm abc@xyz.com\", \"tokenizer\": \"uax_url_email\", \"filter\": [\"apostrophe\"]}"

## gives 1 token [i'm]
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d "{\"text\":\"i'm at abc@xyz.com\", \"tokenizer\": \"uax_url_email\", \"filter\": [{ \"type\": \"length\", \"min\": 3, \"max\": 5 }]}"

## gives truncated token to length 5
curl -H 'Content-Type: application/json' -XGET 'localhost:9200/_analyze?pretty' -d "{\"text\":\"abc@xyz.com\", \"tokenizer\": \"uax_url_email\", \"filter\": [{   \"type\": \"truncate\", \"length\": 5 }]}"
```

* creating a custom analyzer with truncate token filter

```
curl -X PUT "localhost:9200/5_char_words_example?pretty" -H 'Content-Type: application/json' -d'{
  "settings": {
    "analysis": {
      "analyzer": {"lowercase_5_char": {"tokenizer": "lowercase", "filter": [ "5_char_trunc" ]}},
      "filter": {"5_char_trunc": {"type": "truncate", "length": 5}}
    }}}'
```


### What's Text Normalization

* when comparison made at byte-level, similar words wouldn't match

* job of token filters is to make them more searchable

* four major normalization form that exist

> * `NFC` is canonical composition, `NFKC` is compatibility composition
>
> * `NFD` is canonical decomposition, `NFKD` is compatibility decomposition


### ICU Analysis Plug-in

* can be used for Unicode-aware case folding, collation support, and transliteration; [unicode.org report](http://www.unicode.org/reports/tr15/)

* [icu_analyzer](https://www.elastic.co/guide/en/elasticsearch/plugins/master/analysis-icu-analyzer.html) performs uses `icu_normalizer`, `icu_tokenizer` and `icu_folding` using `nfkc_cf(default), or nfkc or nfc`

* `unicode_set_filter` in [ICU Normalization Character Filter](https://www.elastic.co/guide/en/elasticsearch/plugins/master/analysis-icu-normalization-charfilter.html) help set which letters be normalized


### An Analyzer Pipeline

```
[Input]------->[Char Filters]----,
                              [Tokenizer]
[Output]<-----[Token Filters]<---'
```


### Specifying Analyzer for a Field in Mapping

* we can define an index/search analyzer as `analyzer`, or override a separate search analzyer via `search_analyzer` for different fields

* if you use a `whitespace` analyzer for index not search, and try query text `Hi! there` with `hi`.. wouldn't work

```
curl -XPUT -H "Content-Type: application/json" "${ES_URL}/blog" -d'{"mappings": {"properties": {"title": {"type": "text", "analyzer": "simple", "search_analyzer": "standard"}, "content": {"type": "text", "analyzer": "whitespace", "search_analyzer": "standard"} }}}'

## populate some index for blog
curl -XPOST "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "blog" } }
{"title": "First Post", "content": "start wiriting now"}
{ "index" : { "_index" : "blog" } }
{"title": "today is a your'\''s", "content": "well it should have been a better day."}
{ "index" : { "_index" : "blog" } }
{"title": "shall we write", "content": "I guess, that is why we are here."}
'

## wouldn't work as index analyzer gets 2 tokens
curl -XGET "localhost:9200/blog/_search?pretty" -H 'Content-Type: application/json' -d'
{"query": {"match": {"title": "your'\''s"}}}
'

## wouldn't work as whitespace analyzer gets 'here.' token
curl -XGET "localhost:9200/blog/_search?pretty" -H 'Content-Type: application/json' -d'
{"query": {"match": {"content": "here"}}}

curl -H 'Content-Type: application/json' -XGET 'localhost:9200/blog/_analyze?pretty' -d "{\"field\": \"title\", \"text\":\"your's\"}"
```


#### Creating a Custom Analyzer

* by combining char filters, tokinzers, token filters

```
ES_URL="http://localhost:9200"

curl -XDELETE "${ES_URL}/blog"
echo "" && sleep 1

curl -X PUT "localhost:9200/blog?pretty" -H 'Content-Type: application/json' -d'
{
  "settings": {
    "analysis": {
      "analyzer": {
        "my_custom_analyzer": { 
          "char_filter": [
            "html_strip",
            "emoticons"
          ],
          "tokenizer": "punctuation",
          "filter": [
            "lowercase",
            "english_stop",
            "asciifolding"
          ]
        }
      },
      "tokenizer": {
        "punctuation": { 
          "type": "pattern",
          "pattern": "[ .,!?]"
        }
      },
      "char_filter": {
        "emoticons": { 
          "type": "mapping",
          "mappings": [
            ":) => _happy_",
            ":( => _sad_",
            "x( => _angry_"
          ]
        }
      },
      "filter": {
        "english_stop": { 
          "type": "stop",
          "stopwords": "_english_"
        }
      }
    }
  }
}
'
curl -X POST "localhost:9200/blog/_analyze?pretty" -H 'Content-Type: application/json' -d'
{
  "analyzer": "my_custom_analyzer",
  "text": "I\u0027m a :) person, and you?"
}
'

curl -XPOST "${ES_URL}/_bulk?pretty" -H 'Content-Type: application/x-ndjson' -d'
{ "index" : { "_index" : "blog" } }
{"title": "First Post :)", "content": "start wiriting now!!"}
{ "index" : { "_index" : "blog" } }
{"title": "today is a your'\''s", "content": "Hey! :( well it should have been a better day."}
{ "index" : { "_index" : "blog" } }
{"title": "shall we write", "content": "I guess, x( that is why we are here."}
'

curl -XGET "localhost:9200/blog/_search?pretty" -H 'Content-Type: application/json' -d'
{"query": {"match": {"content": "here"}}}
'
```

> used combination filter from [this link](https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-custom-analyzer.html)

* Character Filter

> * HTML Strip Character Filter
>
> * Mapping Character Filter, configured to replace :) with `_happy_` and :( with `_sad_`

* Tokenizer

> * Pattern Tokenizer, configured to split on punctuation characters

* Token Filters

> * Lowercase Token Filter
>
> * Stop Token Filter, configured to use the pre-defined list of English stop words
>
> * ASCII-Folding Token Filter

---

