# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import cfg9
import json


config AppCfg9:
  address: string
  port: int

test "can parse dsl":
  var myConf = newAppCfg9()
  myConf.load("tests/sample-dsl.cfg")
  check myConf.address == "http://example.com"
  check myConf.port == 80
