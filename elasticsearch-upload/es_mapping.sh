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
        "ratsinfo_stop": {
          "type": "stop",
          "stopwords": ["und", "oder", "der", "die", "das", "des", "dass", "für", "fur", "über", "uber", "zu", "zur", "werd", "wurd"]
        },
        "german_stemmer": {
          "type": "stemmer",
          "name": "german"
        },
        "min_length": {
          "type": "length",
          "min": "3"
        }
      },
      "analyzer": {
        "ratsinfo_analyzer": {
          "filter": [
            "lowercase",
            "asciifolding",
            "german_stemmer",
            "min_length",
            "german_stop",
            "ratsinfo_stop"
          ],
          "tokenizer": "lowercase"
        }
      }
    }
  },
  "mappings": {
    "pdf": {
      "_source": { "excludes": ["text"] },
      "_parent": { "type": "session" },
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
              "analyzer": "ratsinfo_analyzer"
          },
          "session_description": {
              "type": "string",
              "store": true,
              "analyzer": "ratsinfo_analyzer"
          },
          "template_description": {
              "type": "string",
              "store": true,
              "analyzer": "ratsinfo_analyzer"
          },
          "description": {
              "type": "string",
              "store": true,
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
