#!/bin/bash -e

curl -XDELETE 'http://localhost:9200/ratsinfo/'
curl -XPUT 'http://localhost:9200/ratsinfo/' -d '{
  "settings" : {
    "analysis": {
      "filter": {
        "german_stop": {
          "type": "stop",
          "stopwords": "_german_"
        },
        "german_stemmer": {
          "type": "stemmer",
          "language": "german"
        }
      },
      "analyzer": {
        "ratsinfo_analyzer": {
          "filter": [
            "lowercase",
            "asciifolding",
            "german_stop",
            "german_stemmer"
          ],
          "tokenizer": "lowercase"
        }
      }
    }
  },
  "mappings": {
    "pdf": {
      "_source": { "excludes": ["text"] },
      "properties": {
          "file_name": {
              "type": "string",
              "index": "no"
          },
          "session_id": {
              "type": "string"
          },
          "template_id": {
              "type": "string"
          },
          "text": {
              "type": "string",
              "store": false,
              "index_name": "text",
              "analyzer": "ratsinfo_analyzer"
          },
          "part_description": {
              "type": "string",
              "store": true,
              "index_name": "part_description",
              "analyzer": "ratsinfo_analyzer"
          },
          "doc_description": {
              "type": "string",
              "store": true,
              "index_name": "doc_description",
              "analyzer": "ratsinfo_analyzer"
          }
      }
    },
    "session": {
        "properties": {
        }
    }
  }
}'
