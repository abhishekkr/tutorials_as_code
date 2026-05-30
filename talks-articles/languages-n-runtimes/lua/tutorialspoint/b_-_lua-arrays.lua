#!/usr/bin/env lua

function printArray(arr)
  for i, v in pairs(arr) do
    print('[' .. i .. ']', v)
  end
end

--- ---------------------Arrays------------------------------------------------
arrAy = {"This", "is", "Lua."}
printArray(arrAy)

arrBy = {}
for i= -1, 1 do       -- initialize array with negative indexes; actually associative table
   arrBy[i] = i *2    -- if iterated with ipairs, would only tackle index:1
end
printArray(arrBy)

--- ---------------------Arrays Multi-Dimensional------------------------------
arrCy = {}
for idx = 1,3 do
   arrCy[idx] = {}
   for jdx = 1,3 do
      arrCy[idx][jdx] = idx*10 + jdx
   end	
end

for i,v in ipairs(arrCy) do
  print('row:', i)
  printArray(v)
end

--- ---------------------Arrays Length-----------------------------------------
print("#arrAy:", #arrAy)
print("#{10,20,30}:", #{10,20,30})
print("#{10,2,nil}:", #{10,2,nil})
print("#{10,2,nil; n=3}:", #{10,2,nil; n=3})    -- size set as 3
print("#{n=1000}:", #{n=1000})                  -- size set as 1000

mixArr = {"abc", "def", "ghi", blh="BLH"}
print('#{"abc", "def", "ghi", blh="BLH"}', #mixArr)
print(mixArr.blh)

mixArr.bleh = "BLEH"    -- resizing array
mixArr[#mixArr+1] = "jkl"

--- ---------------------Arrays Iterating Over Arrays--------------------------
for k,val in ipairs(mixArr) do print(k,val) end
for k,val in pairs(mixArr) do print(k,val) end

function getValues(array)
  local idx = 0
  return function()
    idx = idx + 1
    if idx > #array then return; end
    return idx, array[idx]
  end
end
for k,val in getValues(mixArr) do print(k,val) end

--- ---------------------Arrays Slicing, Sorting, Merging----------------------
--- slice = {table.unpack(arr, startIdx, endIdx)}
arrDy = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34}
sliceDy = {table.unpack(arrDy, 5, 7)}
printArray(sliceDy)

--- sort
arrEy = {"this", "is", "lua"}
table.sort(arrEy)
printArray(arrEy)

arrFy = {{id=101, name='cat'}, {id=201, name='dog'}, {id=15, name='bird'}}
table.sort(arrFy, function(itemA, itemB) return itemA.id < itemB.id; end)
for i,v in pairs(arrFy) do
  print(string.format("[%d] %03d : %s", i, v.id, v.name))
end

--- merge
function tableConcat(t1, t2)
   for i=1,#t2 do
      t1[#t1+1] = t2[i]
   end
   return t1
end

arrGy = tableConcat({1, 3, 5, 7, 9}, {2, 4, 6, 8})
for _, v in pairs(arrGy) do
   print(v)
end

--- ---------------------Arrays: to String-------------------------------------
function arrayToString(arr)
  local text = ""
  for i = 1, #arr do
    text = text .. arr[i]   
  end
  return text
end
print(arrayToString(arrDy))

table.toString = arrayToString  -- can create an alias on existing table module
print(table.toString(arrDy))

--- ---------------------Arrays Metatables-------------------------------------
--- Creating for a 3D Vector datatype
vector3MetaTable = {}                     -- Metatables are tables
function vector3MetaTable.__tostring(t)
  return "X:" .. t.x .. ", Y:" .. t.y .. ", Z:" .. t.z
end

arrHy = {}
setmetatable(arrHy, vector3MetaTable)     -- Attaching metatable to vector3
print(getmetatable(arrHy))

vector3 = {__type = "vector3"}
function vector3.new(x, y, z)
  local self = setmetatable({}, vector3MetaTable)
  self.x, self.y, self.z = x or 0, y or 0, z or 0
	return self
end

print(vector3.new())              -- would call __tostring
print(vector3.new(10))            -- would call __tostring
print(vector3.new(10, 20))        -- would call __tostring
print(vector3.new(10, 20, 30))    -- would call __tostring

--- ---------------------Arrays: Stack, Queue--with Metatables-----------------
--- Stack
function Stack()
  local pushFn = function(self, obj)
    self.count = self.count + 1; rawset(self._stack, self.count, obj)
  end

  local popFn = function(self)
    if self.count < 1 then return nil end
    self.count = self.count - 1; return table.remove(self._stack)
  end

  return setmetatable({_stack = {}, count = 0, push = pushFn, pop = popFn}, {
      __index = function(self, index)
        return rawget(self._stack, index)
      end,
    })
end	

local stack = Stack()
stack:push('A'); stack:push('B')
print(stack.count, ">", stack:pop())
stack:push('C')
print(stack.count, ">", stack:pop())
print(stack.count, ">", stack:pop())
print(stack.count, ">", stack:pop())
print(stack.count, ">", stack:pop())

--- Queue
function Queue()
  local nqFn = function(self, obj)
    self.last = self.last + 1; rawset(self._queue, self.last, obj)
  end

  local dqFn = function(self)
    if self.last < self.first then return nil end
    self.last = self.last - 1
    return table.remove(self._queue, self.first)
  end

  return setmetatable({_queue = {}, first = 1, last = 0, enqueue = nqFn, dequeue = dqFn}, {
      __index = function(self, index)
        return rawget(self._queue, index)
      end,
    })
end	

local que = Queue()
que:enqueue('A'); que:enqueue('B')
print(que.last, ">", que:dequeue())
print(que.last, ">", que:dequeue())
que:enqueue('C')
print(que.last, ">", que:dequeue())
print(que.last, ">", que:dequeue())
print(que.last, ">", que:dequeue())

--- ---------------------Arrays Immutable----using Metatables------------------
--- doesn't work on blocking changing values
function fixedLengthArray(tbl)
  return setmetatable(tbl, {
      __index = tbl,

      __newindex = function(t, k, v)
        error("ReadOnly Array. Permission denied.")
      end,

      __metatable = false
    });
end

arrIy = fixedLengthArray({1, 10, 100})
arrIy[#arrIy] = 200
printArray(arrIy)
--arrIy[#arrIy+1] = 1000      -- would raise the __newindex error
printArray(arrIy)

--- ---------------------Arrays Shuffle----------------------------------------
--- using Fisher-Yates method

function shuffleFY( array )
   local returnArray = {}
   for i = #array, 1, -1 do
      local j = math.random(i)
      array[i], array[j] = array[j], array[i]
      table.insert(returnArray, array[i])
   end
   return returnArray
end

arrJy = { 1, 20, 3, 14, 5, 36}
math.randomseed(os.time() + #arrJy) 
for i = 1, #arrJy do
  io.write(" " .. arrJy[i])
end
print("...")
shuffledArray = shuffleFY(arrJy)
for i = 1, #shuffledArray do
  io.write(" " .. shuffledArray[i])
end
print("...")

--- ---------------------------------------------------------------------------
