# Setup instructions

## Get data

Run [Mic's Scraper](https://github.com/Mic92/ratsinfo-scraper) and make data available:

```sh
ln -s .../ratsinfo-scraper/data .
```

Next, convert PDFs to Text:
```sh
./pdftotext.sh
```

Extract addresses with **osm-addrs** or alternatively get [the Dresden dataset](http://astro.github.io/democropticon/dresden.json).

Finally, use **locate-documents** to create `locations.json` files.

## Setup Elasticsearch

Go figure.

## es-upload

### Install script dependencies

```sh
npm i
```

### Upload data into elasticsearch

```sh
node upload.js data
```

## ratskarte2 server

### Install script dependencies

```sh
npm i
```

### Run webserver

```sh
node server.js
```
