#!/usr/bin/env lua

--- ---------------------Strings-----------------------------------------------
strA = "THIS."
strB = 'This.'
strC = [[this.]]
print(type(strA), type(strB), type(strC))  -- all string

--- ---------------------Strings Manipulation----------------------------------
strLua = "Lua Language."

print("Upper/Lower:", string.upper(strB), string.lower(strB))

print("Gsub:", string.gsub(strLua, "ua", "AU"))

print("Find:", string.find(strLua, "ua"))
print("Find with optional start index:", string.find(strLua, "ua", 5))
print("Find with init & pattern match:", string.find(strLua, "u+", 5, true))  -- disables regex
print("Find with init & pattern match:", string.find(strLua, "u+", 5, false)) -- matchs regex

print(string.reverse(strLua))

print(string.format("This is %s. Number: %02d & %0.3f", strLua, 1, 3/4))

-- Byte conversion
strLuaChr1 = string.byte(strLua)     -- First character
strLuaChr2 = string.byte(strLua,7)   -- Seventh character
strLuaChr3 = string.byte(strLua,-7)  -- Seventh character from last
print(string.char(65))               -- Internal Numeric ASCII Conversion
print(string.char(strLuaChr1))
print(string.char(strLuaChr2))
print(string.char(strLuaChr3))

print("Length of strA is ",string.len(strA))

--- ---------------------Strings Concatenation--& table------------------------
print("Concatenated string", strA..strB..strC)

print(string.rep('_-', 10))

numTbl = {1, 3, 5, 7, 9}
print(table.concat(numTbl))

--- ---------------------Strings Loop------------------------------------------
for i = 1, #strLua, 3 do
   local c = strLua:sub(i,i+2)
   print(c)
end

for c in strLua:gmatch '.....' do
   print(c)
end

--- ---------------------Strings to Int----------------------------------------
str = "100"
numA = str + 0
numB = tonumber(str)
print(type(numA), numA, "|", type(numB), numB)

--- ---------------------Strings Split-----------------------------------------
for s in string.gmatch(strLua, "%S+") do
   print("> " .. s)
end

function strSplit (inputstr, sep)
   if sep == nil then sep = '%s' end
   local t={}
   for str in string.gmatch(inputstr, '([^'..sep..']+)') do
      table.insert(t, str)
   end
   return t
end

for idx, val in ipairs(strSplit(strLua)) do print(idx, val) end
for idx, val in ipairs(strSplit(strLua, 'L')) do print(idx, val) end

--- ---------------------Strings if NULL---------------------------------------
strMt = ""
if not strMt or string.len(strMt) == 0 then
   print(strMt,"is empty.")
else
   print(strMt,"is not empty.")
end

if strMt == "" then print("strMt is Empty") end

--- ---------------------------------------------------------------------------
