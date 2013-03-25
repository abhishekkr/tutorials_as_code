#!/bin/bash
# perlish/awkish one-liners

echo "Ninja    \$99.99\nVitamix     \$378.95\nBlendtec    \$399.99" > blenders.txt
ruby -ne 'BEGIN { total = 0 };
          END { puts "$%.2f" % total };
          total += $_[/\$(\d+(?:\.\d+)?)/, 1].to_f' blenders.txt
rm blenders.txt
