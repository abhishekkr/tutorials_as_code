#!/usr/bin/env python

import sys
if sys.version_info.major != 2:
    print("Error: Default Python used is Python%s" % sys.version_info.major)
    print("\tSet env variable PYSPARK_PYTHON to Python2 binrary and re-run it.")
    sys.exit(1)

print("ok so its %s" % str(sys.version_info))
