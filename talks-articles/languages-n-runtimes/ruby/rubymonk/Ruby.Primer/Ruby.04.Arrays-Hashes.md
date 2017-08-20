
=begin

## Arrays

```
=end

print """

>> [] == Array.new
=> #{[] == Array.new}
>> [1, \'a\', [2, \'b\']]
=> #{[1, 'a', [2, 'b']]}
>> [1, \'a\', [2, \'b\']][1]
=> #{[1, 'a', [2, 'b']][1]}
>> [1, \'a\', [2, \'b\']][-1]
=> #{[1, 'a', [2, 'b']][-1]}
>> [1, \'a\', [2, \'b\']] << \"new-item\"
=> #{[1, 'a', [2, 'b']] << "new-item"}
>> [1, \'a\', [2, \'b\']].push(\"new-item\")
=> #{[1, 'a', [2, 'b']].push("new-item")}

>> arr = [1, \'a\', [2, \'b\']]
=> #{arr = [1, 'a', [2, 'b']]}
>> arr.map{|i| \"[\#{i.to_s}]\"}
=> #{arr.map{|i| "[#{i.to_s}]"}}
>> arr.collect{|i| \"[\#{i.to_s}]\"}
=> #{arr.collect{|i| "[#{i.to_s}]"}}
>> arr.select{|i| i == \"a\" }
=> #{arr.select{|i| i == "a" }}
>> arr.delete_if{|i| i == \"a\" }
=> #{arr.delete_if{|i| i != "a" }}
>> arr.delete 1
=> #{arr.delete 1}
>> arr
=> #{arr}

"""

arr = ["oh", "ok", "wokay"]
print """
>> for i in arr
>>   puts \">> \#{i}\"
>> end
"""
for i in arr
  puts ">> #{i}"
end

print """
>> arr.each do |i|
?>   puts \">> \#{i}\"
>> end
"""
arr.each do |i|
  puts ">> #{i}"
end

=begin
```

---

## Hashes

```
=end

print """
>> card = {
?>     \"name\" => \"random\",
?>     \"phone\" => \"9xyyyyyyyyyz\",
?>     \"address\" => \"ok where now\"
>>   }
=> #{card = {"name"=>"random", "phone"=>"9xyyyyyyyyyz", "address"=>"ok where now"}}
>> card[\"name\"]
=> #{card["name"]}
>> card[\"name\"] = \"someone\"
=> #{card["name"] = "someone"}
>> card[\"street\"] = \"which\"
=> #{card["street"] = "which"}
>> card
=> #{card}

>> card.each do |k,v|
?>   puts \"\#{k} :: \#{v}\"
>> end
"""
card.each do |k,v|
  puts "#{k} :: #{v}"
end

print """
>> card.keys
=> #{card.keys}
>> card.values
=> #{card.values}

>> normal = Hash.new
=> #{normal = Hash.new}
>> normal[:zig]
=> #{normal[:zig]}
>> some_brown = Hash.new(\"brown\")
=> #{some_brown = Hash.new("brown")}
>> p some_brown[:zig]
=> #{p some_brown[:zig]}
>> p some_brown[:fig]
=> #{p some_brown[:fig]}

>> ages = Hash[:chintu, 10, :pintu, 12, :rintu, 8]
=> #{ages = Hash[:chintu, 10, :pintu, 12, :rintu, 8]}

"""

=begin
```

---

=end
