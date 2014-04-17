#!/usr/bin/env bash

pdfs=$(find data/ -iname *.pdf)
total=$(echo "$pdfs"|wc -l)
i=0
for f in $pdfs
do
    i=$(($i + 1))
    echo $((100*$i/$total))"% $f"
    pdftotext -nopgbrk -enc UTF-8 -eol unix "$f"
done
