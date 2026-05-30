#!/usr/bin/env lua

function double_x(x)
    return x+x
end

print("double of 123:", double_x(123))


--- anonymous fn
myprint = function(param)
   print("parameters given is", param)
end
myprint("THIS.")


--- creating alias
double_y = double_x
if double_x(2) == double_y(2) then print("Function Alias Works.") end


--- variable length of arguments
function average_of(...)
    result = 0
    local xargs = {...}
    for _, v in ipairs(xargs) do
        result = result + v
    end
    return result/#xargs
end

average_of(1,2,3,4,5,6,7,8,9,10)


--- ---------------------Multiple Returns-&-Assigning Them---------------------
some_str = "Lua has functions that fun.. oh, that run."
some_substr = "fun"

--- multiple returns from predef fn
idxStart, idxEnd = string.find(some_str, some_substr)
print("start-index:", idxStart, "; end-index:", idxEnd)

n1, n2, n3 = table.unpack({10, 100, 1000})
print("{10, 100, 1000} unpacked to", n1, n2, n3)


--- multiple return fn
function strFind(s, sub)
  local sLen = #s
  local subLen = #sub
  for idx = 0, sLen do
    if (idx + subLen - 1) > sLen then break end
    local toMatch = s:sub(idx, idx+subLen-1)
    if toMatch == sub then return idx, (idx+subLen-1) end
  end
  return -1, -1
end

iStart, iEnd = strFind(some_str, some_substr)
print("start-idx:", iStart, "; end-idx:", iEnd)

--- other cases of matching arguments
iA = strFind(some_str, some_substr)
print("when two returned, latter discarded:", iA)
iB, iC, iD = strFind(some_str, some_substr)
print("when two returned, extra gets nil:", iB, iC, iD)

--- function call being argument, differs when before an arg or last
print("when last is multiple return call:", strFind(some_str, some_substr))
print("when multiple return call:", strFind(some_str, some_substr), "; is before another arg")

--- ---------------------Fn Calls As Arguments---------------------------------
--- forcing single value return
iE, iF = (strFind(some_str, some_substr))
print("when two returned, forced for first only:", iE, iF)

--- forcing all value return in one var
iG = { strFind(some_str, some_substr) }
print("when many returned, one with all using table constructor:", iG[1], iG[2])

--- when func call used in an expression
function gimmeNum() return 10 end
function gimmeNums() return 10, 20, 30 end
function gimmeStr() return 'at' end
function gimmeStrs() return 'at', 'bat', 'cat' end

print("concat string at end:" .. gimmeStr())
print("concat strings at end:" .. gimmeStrs())
print("increment num at end:", 1 + gimmeNum())
print("increment nums at end:", 1 + gimmeNums())

print(gimmeStrs() .. "~concat strings NOT at end")
print("increment nums at start:", gimmeNums() + 1)
print("increment nums in middle:", 1 + gimmeNums() + 9)

--- ---------------------Fn Named & Optional Arguments------------------------------------
function greet(arguments)
  if arguments.greeting then
    print(arguments.greeting .. ' ' .. arguments.name .. '!')
  else
  end
    print('Hello ' .. arguments.name .. '!')
end

greet({name='User', greeting='Hey'})
greet({name='User'})

--- ---------------------Fn Closures-------------------------------------------
function createCountdown(startAt)
   local count = startAt
   return function()    -- a closure
      count = count - 1
      return count
   end
end

countFrom10 = createCountdown(10)
countFrom20 = createCountdown(20)

print(countFrom10())
print(countFrom20()) 
print(countFrom10())
print(countFrom20()) 
print(countFrom20()) 

--- iterate one by one for when needed
function popList(lst)
  local idx, len = 0, #lst
  return function()
    idx = idx + 1
    if idx <= len then return lst[idx] end
  end
end

numeros = {1, 10, 2, 20, 3, 30, 4, 40, 5, 50}
for item in popList(numeros) do
  print(item)
  if item % 4 == 0 then break end
end

--- used for Encapsulation
function newUser(details)
  local user = {id = details.id, name = details.name}

  user.display = function()
    print("[" .. user.id .. "]", user.name)
  end

  user.changeName = function(name)
    user.name = name
  end

  return user
end

john = newUser({id = 1, name = "John Doe"})
john.display()
john.changeName("John Gone")
john.display()

--- higher order Fn
function xTimes(incr)
  return function(x)
    return x * incr
  end
end

double, triple = xTimes(2), xTimes(3)
print(double(10), triple(10))

--- ---------------------Fn Anonymous------------------------------------------
function applyFn(fn, a, b)
  return fn(a, b)
end

forMultiply = applyFn(function(x, y) return x * y end, 2, 4)
forAdd = applyFn(function(x, y) return x + y end, 2, 4)
print("forMultiply: " .. forMultiply, "| forAdd: " .. forAdd)

--- ---------------------Fn in Tables------------------------------------------
RectangleA = {
  length = 10,
  breadth = 5,
}

function RectangleA:area()
  return self.length * self.breadth
end

print(RectangleA:area())
RectangleA.breadth = 10
print(RectangleA:area())

--- ---------------------Fn Tail Calls-----------------------------------------
function factorial_helper(n, accumulator)
  if n == 0 then
    return accumulator
  else
    -- proper tail call shall be the last expression of the branch
    return factorial_helper(n - 1, n * accumulator)
  end
end

function factorial(n)
  return factorial_helper(n, 1)
end

print(string.format("%.0f",factorial(0)))
print(string.format("%.0f",factorial(1)))
print(string.format("%.0f",factorial(5)))

--- ---------------------------------------------------------------------------
