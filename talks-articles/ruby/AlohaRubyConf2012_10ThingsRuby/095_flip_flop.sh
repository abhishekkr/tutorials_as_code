#!/bin/bash

echo "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" > numbers.txt
ruby -ne 'print if $_.to_i % 2 == 0' numbers.txt
rm numbers.txt
