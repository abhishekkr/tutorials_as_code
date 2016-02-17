=begin

## Everything is object

```
=end

puts "2.even? : #{2.even?}"
puts "2.equal? 1 : #{2.equal? 1}"
puts "2.integer? : #{2.integer?}"
puts "2.instance_of? Integer : #{2.instance_of? Integer}"
puts "2.negative? : #{2.negative?}"
puts "2.next : #{2.next}"
puts "2.itself.next : #{2.itself.next}"
puts "2.next.next.next.equal? 5 : #{2.next.next.next.equal? 5}"
puts "2.between?(1,3) : #{2.between?(1,3)}"

twoTo4 = 2.upto 4  #enumerator
puts "twoTo4.next -> twoTo4o.next -> twoTo4.next : #{twoTo4.next} -> #{twoTo4.next} -> #{twoTo4.next}"

puts "\nall methods for an object:\n#{2.methods}\n"

puts """
>> 1.+(3)
=> #{1.+(3)}
>> 2.*(3)
=> #{2.*(3)}
>> 2.**(3)
=> #{2.**(3)}
>> [10,100,100].[](1)
=> #{[10,100,100].[](1)}
"""

=begin
```

---

=end
