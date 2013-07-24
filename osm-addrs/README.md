# Filter addresses from OpenStreetMap dumps

## Building

*TODO:* write a `.cabal` file!

```bash
cabal update; cabal install --ghc-options=-O2 xml-conduit aeson
ghc --make -O2 osm-addrs
```

## Usage

First download and decompress a `.osm.bz2` dump from
[Geofabrik](http://download.geofabrik.de/europe/germany/sachsen.html).

Run through pipe view:
```bash
pv -per < sachsen-latest.osm |
	./osm-addrs Dresden > dresden.json
```

## Notes on gnuplot

You may modify the program to output location coordinates suitable for
GNUplot. Display with:
```gnuplot
set xrange [13.6:13.9]
set yrange [38.87:39.02]
plot "locations.data" using 2:(90 - $1) with dots lt rgb "#0000ff";
```
