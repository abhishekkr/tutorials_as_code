#!/bin/bash
## ruby cmd line switch -n, -p, -i, -e
echo "ones\ntwos\nthrees" > data.txt
ruby -pi.bak -e 'sub(/s\Z/, "")' data.txt
cat data.txt
ls data.txt*
rm data.txt
rm data.txt.bak
