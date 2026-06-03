#!/usr/bin/env lua

local blah = require("blahblah")
local baah = require("blahbaah")

--- ---------------------Modules ----------------------------------------------
print("using module by " .. blah.config.author)

blah.bleh("baa baa")

blah.bowow.wow("yo yo yeeaaah")

--- ---------------------Modules Cached so as Singleton-----------------------
print("[main] blah.config.counter", blah.config.counter)
baah.bleh("oye")
blah.config.counter = blah.config.counter + 1
print("[main] blah.config.counter", blah.config.counter)
baah.bleh("oyeoye")

--- ---------------------------------------------------------------------------
