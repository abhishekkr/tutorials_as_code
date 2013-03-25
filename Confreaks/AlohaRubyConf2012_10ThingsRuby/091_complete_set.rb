#!ruby

require 'set'

animals = Set.new
animals << 'cat'
animals.add 'bat'
p animals.add?('rat')
p animals.add?('rat')
p animals

p animals.member? 'tiger'
p animals.subset?(Set['bat', 'cat'])
p animals.superset?(%w['lions', 'tigers', 'bears'].to_set)

ordered = SortedSet.new
nums = (0..10).to_a.shuffle
nums.each{|n| ordered << n }
p nums, ordered
