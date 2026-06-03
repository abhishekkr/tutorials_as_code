#!/usr/bin/env lua

function printTable(arr)
  for i,v in next, arr do
    print('[' .. i .. ']', v)
  end
end

printTable({"a", "b", c = "C"})

--- ---------------------Iterator Types---------------------------------------
--- Stateless Iterator
function square(iteratorMax, num)   -- this Fn doesn't retain any state
  if num == nil then num = 1 end    -- for the first run,
  if num <= iteratorMax then        -- else do 'square,3,1' at for
    local sq = num * num
    num = num + 1
    return num, sq
  end
  -- when nil gets sent, the iterator ends
end

for i,n in square,3 do print(i .. "#", n) end

--- Stateful Iterator; this also uses Closures
function likeIPairs (tbl)
   local idx = 0
   return function ()
      idx = idx + 1
      if idx <= #tbl then return idx, tbl[idx]; end
      -- when nil, ends the iteration
   end
end

for i, item in likeIPairs({'abc', 'def', 'ghi'}) do print(i, item); end
for i, item in likeIPairs({a = 'jkl', 'mno', 'pqr'}) do print(i, item); end


--- ---------------------Iterator to Read File---------------------------------

function wordIterator(filePath)
  file = io.open(filePath, "r")
  io.input(file)
  local line, pos = io.read(), 1  -- read first line
  return function ()
    while line do
      local s, e = string.find(line, "%w+", pos)
      if s then
        pos = e + 1
        return string.sub(line, s, e)
      else
        line, pos = io.read(), 1 -- read next line
      end
    end
    io.close(file)
    return nil
  end
end

for word in wordIterator("README.md") do
   io.write(word..", ")
end
print()

--- ---------------------Iterator Reverse---------------------------------------
local charfive = {'a', 'b', 'c', 'd', 'e'}
for i = #charfive, 1, -1 do               -- using numeric for
    print(":".. i, charfive[i])
end

function reverseIterator(arr)           -- to use with generic for
   local function reverse(arr,i)
      i = i - 1
      if i ~= 0 then
         return i, arr[i]
      end
   end
   return reverse, arr, #arr+1
end

for idx, chr in reverseIterator(charfive) do
   print(";".. idx, chr)
end

--- ---------------------Iterator with Filter----------------------------------
--- can define custom
table.filter = function(arr, filterIterator)
   local result = {}
   for key, val in pairs(arr) do
     if filterIterator(val, key, arr) then
       table.insert(result, val)
     end
   end
   return result
end

numeros10 = {1, 2, 3, 4, 5, 6, 7, 8, 9}
evenOnlyItems = table.filter(numeros10,
   function(item, _key, _tbl) return item % 2 == 0; end
)
for k,v in ipairs(evenOnlyItems) do
   print(k,v)
end

userdata = {{id=101, name='alice'}, {name='bob'}, {id=103, name='eve'}}
validUsers = table.filter(userdata,
   function(item, _key, _tbl)
      return item['id'] ~= nil
   end
)
for k,v in ipairs(validUsers) do
   print(k,v.id, v.name)
end

--- ---------------------------------------------------------------------------
