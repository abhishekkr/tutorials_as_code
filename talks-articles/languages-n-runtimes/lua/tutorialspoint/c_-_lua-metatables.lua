#!/usr/bin/env lua


--- ---------------------Metatables--------------------------------------------
mytable = setmetatable({key1 = "value1"}, 
   { __index = { key2 = "metatablevalue" } })
print(mytable.key1,mytable.key2)


scores = setmetatable({1, 10, 100, 1000}, {
  __tostring = function(mytable)
    sum = 0
    for k, v in pairs(mytable) do
      sum = sum + v
    end
    return "Total score is " .. sum
  end
})
print(scores)

--- ---------------------Metatables Inheritance--------------------------------
Animal = { sound = "yawn" }     -- base class
Animal.__index = Animal 
function Animal:makeSound()
  print(self.sound)
end

Dog = setmetatable({ species = "Canis Lupus Familiaris" }, { __index = Animal }) -- subclass of Animal
Dog.__index = Dog
function Dog:bark()
  print("woof")
end
function Dog:makeSound()    -- override
  print("woof wuuf")
end

Pug = setmetatable({ name = "PUG" }, { __index = Dog })
Pug.__index = Pug 
function Pug:snore()
  print("wooZzz..")
end

local animal = {}
setmetatable(animal, { __index = Animal })

local dog = {}
setmetatable(dog, { __index = Dog })

local pug = {}
setmetatable(pug, { __index = Pug })

animal:makeSound()

print(dog.species, dog.sound)
dog:makeSound()
dog:bark()

print(pug.name, pug.species, pug.sound)
pug:makeSound()
pug:bark()
pug:snore()

--- ---------------------Metatables Chaining-----------------------------------
tbl1 = {k1 = "val1"}
tbl2 = {k2 = "val2"}
setmetatable(tbl1, {__index = tbl2})

local tbl3 = {k3 = "val3"}
setmetatable(tbl3, {
  __index = function(tbl, key)
    print("Checking for missing key '" .. key .. "'")
    return tbl1[key]
  end
})

print(tbl3.k1)
print(tbl3.k2) 
print(tbl3.k3)

--- ---------------------Metatables ReadOnly Proxy-----------------------------
local newUser = { name = "Alice", id = 101 }

local proxyUser = {}
local metatableReadOnly = {
  __index = newUser,
  __newindex = function(_tbl, _k, _v)
    error("Attempt to modify a read-only table", 2)
  end
}

setmetatable(proxyUser, metatableReadOnly)
print(proxyUser.name) 
-- proxyUser.age = 102          -- RO error
-- proxyUser["abc"] = "def"     -- RO error

-- these can be override to Trace/Debug the operations as well

--- ---------------------Metatables Delegation---------------------------------
local delegation = {
  greet = function(self, g, name)
    return "Hello, " .. g .. " " .. name .. "!"
  end
}

local iDelegate = {k1="Alice"}

local metatableDelegation = {
  __index = function(t, key)
    return delegation[key]
  end,
  __call = function(t, ...)
    local targs = {...}
    local val = t[targs[1]]
    if val ~= nil then
      return delegation:greet("User", val)
    end
      return delegation:greet("Dear", ...)
  end
}

setmetatable(iDelegate, metatableDelegation)
print(iDelegate.greet(nil, "Dr.", "Bob")) 
print(iDelegate("Eve"))
print(iDelegate("k1"))
print(iDelegate.k1)

--- ---------------------Metatables Defaults-----------------------------------
local defaults = { width = 100, height = 50 }
local shape = setmetatable({color="Red"}, { __index = defaults })
print(shape.width) 
print(shape.height)
print(shape.color)

--- ---------------------------------------------------------------------------
