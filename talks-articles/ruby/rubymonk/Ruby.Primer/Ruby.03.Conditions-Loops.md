
=begin
## Bool

```
=end

print """

>> RUBY_VERSION == \"2.3.3\"
=> #{RUBY_VERSION == "2.3.3"}
>> RUBY_VERSION == \"#{RUBY_VERSION}\"
=> #{RUBY_VERSION == "#{RUBY_VERSION}"}

>> RUBY_VERSION.to_i
=> #{RUBY_VERSION.to_i}
>> RUBY_VERSION.to_f
=> #{RUBY_VERSION.to_f}
>> RUBY_VERSION.to_f >= 2 && RUBY_ENGINE
=> #{RUBY_VERSION.to_f >= 2 && RUBY_ENGINE}

>> RUBY_VERSION.to_f >= 2 && RUBY_ENGINE == \"ruby\"
=> #{RUBY_VERSION.to_f >= 2 && RUBY_ENGINE == "ruby"}
>> RUBY_VERSION.to_f >= 2 && (RUBY_ENGINE == \"ruby\" || RUBY_ENGINE == \"jruby\")
=> #{RUBY_VERSION.to_f >= 2 && (RUBY_ENGINE == "ruby" || RUBY_ENGINE == "jruby")}
>> RUBY_VERSION.to_f >= 2 && ! (RUBY_ENGINE == \"ruby\" || RUBY_ENGINE == \"jruby\")
=> #{RUBY_VERSION.to_f >= 2 && ! (RUBY_ENGINE == "ruby" || RUBY_ENGINE == "jruby")}

"""

=begin
```
---

## Conditionals

```
=end

def fooIf(n)
  if n < 0
    puts "negative"
  elsif n == 0
    puts "zero"
  else
    puts "positive"
  end
end

def fooUnless(n)
  unless n < 0
    puts "process it"
  end
end

print """
#{ fooIf 10 }
#{ fooIf 0 }
#{ fooIf -10 }
#{ fooUnless 10 }
#{ fooUnless 0 }

#{ 10 > 0 ? "positive" : "not positive" }

"""

=begin
```
---

## Loops

```
=end

cntr = 1
loop do
  cntr += 1
  if cntr > 10
    break
  end
  print cntr.to_s + " "
end
puts

5.times do
  puts "this will run 5 times"
end


=begin
```

---
=end
