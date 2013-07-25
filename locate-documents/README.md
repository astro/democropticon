# Suche Adressen in Dokumenten

## Abhängigkeiten

* Benötigt Adressdaten als JSON von `osm-addrs`
* Braucht `data` von [Mic92s Ratsinfo-Scraper](https://github.com/Mic92/ratsinfo-scraper).
* Indiziert nur `.txt`-Dateien; deshalb alle PDFs mit `pdftotext`
  umwandeln und ggf. OCR anwenden.
  
## Ausführung

```shell
ghc --make -O2 annotate
ln -s ../../ratsinfo-scraper/data .
./annotate ../osm-addrs/dresden.json data
```

Nach Ausführung von `annotate` gibts in jedem Unterverzeichnis
zusätzlich zu `metadata.json` noch eine `locations.json`.
