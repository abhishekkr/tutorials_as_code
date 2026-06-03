#!/usr/bin/env lua

--- ---------------------Tables Basics----------------------------------------
tblA = {}
print("tblA is " .. type(tblA))

tblA = {1, 2, 3, 4, 5}
print("tblA is " .. type(tblA))

tblA[1] = "abc"
tblA["def"] = "ghi"
print("tblA at 1 is " .. tblA[1])
print("tblA at 'def' is " .. tblA['def'])

altA = tblA
print("altA at 1 is " .. altA[1])
altA[1] = "other datatype altogether"
print("tblA at 1 is " .. tblA[1])
altA = nil
print("altA is ", altA)
print("tblA at 1 is " .. tblA[1])     -- as hex space assigned to both was same
                                    -- altA assigned to nil just changed its pointer
tblA = nil
print("tblA is ", tblA)

tblA = {1, 2, 3, 4, 5}
print("concat tblA:", table.concat(tblA))
print("concat tblA with ,:", table.concat(tblA, ', '))
print("concat tblA with ,2:", table.concat(tblA, ', ', 2))
print("concat tblA with ,2to3:", table.concat(tblA, ', ', 2, 3))

table.insert(tblA, 'abc')
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()

table.insert(tblA, 3, 'def')
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()

table.remove(tblA)
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()

table.remove(tblA, 3)
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()

table.insert(tblA, 3, 30)
table.sort(tblA)
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()

table.sort(tblA, function(n1, n2) return n1 > n2; end)    -- reverse num sort
for idx=1,#tblA do io.write(idx .. ": " .. tblA[idx] .. ", "); end; print()


--- ---------------------Tables  @s Dict/List----------------------------------
wdays = {"Sunday", "Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday"}

weekdays = {[0]="Sunday", [1]="Monday", [2]="Tuesday",[3]="Wednesday",
            [4]="Thursday", [5]="Friday", [6]="Saturday"} -- doesn't allow 4="Thursday"

workdays = {[2] = "Monday", "Tuesday","Wednesday", "Thursday", "Friday"}

print(wdays[2])  -- since default indices start from 1
print(weekdays[1])
-- gives Wednesday.. as while inserting items, Tuesday starts with 1 and then 2 overwrites with Wdnesday
print(workdays[2])

kvlike = {sun="Sunday", mon="Monday", ["tue"]="Tuesday", ["wed"]="Wednesday"}
for k,v in pairs(kvlike) do
  print(type(k) .. " as " .. k .. ": " .. v)
end

--- ---------------------Tables @s Sets---------------------------------------
function Set (list)
   local set = {}
   local result = {}
   for _, value in ipairs(list) do
      set[value] = true
   end
   for k, _ in pairs(set) do table.insert(result, k) end
   return result, set
end

list = {"2", "3", "5", "7", "11", "2", "5", "11"} -- 5 uniq, 8 total
uniqList, numSet = Set(list)

for i, val in ipairs(uniqList) do
  print(i, val)
end
if numSet["1"] then print("1 is in set") else print("1 is not in set") end
if numSet["2"] then print("2 is in set") else print("2 is not in set") end

--- ---------------------Tables Copy BY VALUE---------------------------------
function table.table_copy(t1)
   local t2 = {}
   for k,v in pairs(t1) do t2[k] = v end
   return t2
end

copyList = table.table_copy(list)
copyList[1] = "1"
print("list[1]: " .. list[1], "copyList[1]: " .. copyList[1])
refList = list
refList[1] = "1"
print("list[1]: " .. list[1], "refList[1]: " .. refList[1])

--- table built-in method way to do so
copyList = nil
copyList = { table.unpack(list) }
list[1] = "2"
print("list[1]: " .. list[1], "copyList[1]: " .. copyList[1])

--- above only does SHALLOW CLONE though, doesn't work for nested mess like this
nestedMessOfATable = {abc = { def = { ghi = "GHI"}}, x = { y = { z = "XYZ"}}}

function deepCopy(tbl)
  local copiedTbl
  if type(tbl) == 'table' then
    copiedTbl = {}
    for key, value in next, tbl do
      copiedTbl[deepCopy(key)] = deepCopy(value)
    end
    setmetatable(copiedTbl, deepCopy(getmetatable(tbl)))
  else
    copiedTbl = tbl
  end
  return copiedTbl
end
copiedMess = deepCopy(nestedMessOfATable)

print(nestedMessOfATable.abc.def.ghi, copiedMess.abc.def.ghi)
copiedMess.abc.def.ghi = "CHANGED GHI"
print("[POST EDIT]", nestedMessOfATable.abc.def.ghi, copiedMess.abc.def.ghi)

--- ---------------------Tables: Getting Default Value------------------------
taskStatus = {task1 = "WIP", task2 = "DONE", task3 = "WIP"}

setmetatable(taskStatus, {
  __index = function(self, index)   -- index is the non-existing index
    return "UNDEF"
  end
})

print(taskStatus.task1)
print(taskStatus.task10)

--- ---------------------Tables Inheritance-----------------------------------
Animal = {
  says = {},

  speak = function(self)
    if self.says == nil then return; end
    for _,v in pairs(self.says) do io.write(" "..v) end
    print()
  end
}

function Animal:new(u)
  u = u or {}
  setmetatable(u, self)
  self.__index = self
  return u
end

Cat = Animal:new()
function Cat:new(c, word)
  c = c or Animal:new(c)
  setmetatable(c, self)
  self.__index = self
  self.says = {"meow", word}
  return c
end

bob = Cat:new(nil, "bobcat")
if type(bob.speak) == "function" then
  bob:speak()
end

--- ---------------------Tables - Weak----------------------------------------
--- table variables are reference to memory; when set to nil, memory is GC-d

--- with objects used as Key or Values; upon change GC doesn't auto collect
--- as below
days = {}
key = {"this"}          -- create a key as object
days[key] = "THIS"
print(days[key])
key = {}                -- create second key as object
days[key] = "THAT"      -- override the first key
print(days[key])

collectgarbage()        -- doesn't reclaim first key

--- a table could be set a WEAK using metamethod
--- '__mode' to 'k' for Weak Keys, to 'v' for Weak Values, 'kv' for both weak

local t = {}
setmetatable(t, {__mode = 'k'})

do    -- closure
    local key = {"THIS"}
    t[key] = 123
end
for k, v in pairs(t) do print(k[1], v) end
collectgarbage()  -- object key is removed, as out of scope (local to closure)
print(#t)         -- no key, after GC
for k, v in pairs(t) do print(k[1], v) end

do
    key = {"THAT"}
    t[key] = 123
end
collectgarbage()
for k, v in pairs(t) do print(k[1], v) end  -- key is still valid since Global

--- can be used for memoization; where checking size GC can be run to free up
--- ---------------------------------------------------------------------------
