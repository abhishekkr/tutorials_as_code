#!/usr/bin/env ruby

## Inspired from why's Poignant Guide to Ruby

#1
puts "name"
puts "name".reverse
puts "name".length
puts "name" * 5

#2
puts 10.to_s.reverse
puts "10".to_i.class
puts Array(10) # if on Ruby1.8.7 go .to_a
[]
[1,11,111]
[1,11,111].max
numbers = [1,5,4,3,6,7,8,9,2,0]
print numbers, numbers.sort!, "\n"

#3
poem = "My toast has flown from my hand
And my toast has gone to the moon.
But when I saw it on television,
Planting our flag on Halley's comet,
More still did I want to eat it."
puts poem,''
poem['toast'] = 'honeydew'
puts poem,''
puts poem.reverse,''
puts poem.lines.to_a,''
puts poem.lines.to_a.reverse.join,''
puts poem.include?('my hand'), ''
puts poem.delete("alleys").downcase,''

#4
books = {}
books["Gravity's Rainbow"] = :splendid
books["Gravity's Cloud"] = :quite_good
books["Gravity's Sky"] = :excellent
puts books.length,''
puts books["Gravity's Rainbow"],''
puts books.keys,''
ratings = Hash.new(0)
books.values.each {|rate| ratings[rate] += 1 }
ratings
5.times{print '<>< '}

#5
Dir.entries "./"
print File.read("./README"),"\n"
require 'fileutils'
FileUtils.cp('./README', './README.md')
Dir["./README*"]
File.open('./README.md', 'a') do |f|
  f << 'one new line added'
end
print File.read('./README.md'),"\n"
File.mtime('./README.md')
File.mtime("./README.md")
File.mtime("./README.md").hour

#6
def load_readme( path )
  readme = {}
  File.foreach(path) do |line|
    next unless line.match(/^https?\:\S*$/)
    readme[line.split('/')[-1]] = line
  end
  readme
end
puts load_readme('./README.md'),''


#7
class BlogEntry
  attr_accessor :title, :time, :fulltext, :mood
end
entry = BlogEntry.new
entry.title = "Today Mt. Hood Was Stolen!"
entry.mood = :sick
entry.time = Time.new '2012-12-16 09:18:29 +0530'
entry.fulltext = "I can't believe Mt. Hood was stolen! I am speechless! It was stolen by a giraffe who drove away in his Cadillac Seville very nonchalant!!"
puts entry,''
class BlogEntry
  def initialize( title, mood, fulltext )
    @time = Time.now
    @title, @mood, @fulltext = title, mood, fulltext
  end
end
entry2 = BlogEntry.new("I Left my Hoodie on the Mountain!", :confused, "I am never going back to that mountain and I hope a giraffe steals it.")
blog = [entry, entry2]

#8
blog = [entry, entry2]
puts blog.sort_by{|entr| entr.time }.reverse.collect{|c| c.title }
blog.map { "Bruce Willis" }
puts blog,''
puts entry,''
puts entry.mood,''

require 'date'
Date.new(2009, 10, 13) - Date.new(2009, 10, 11)

require 'fileutils'
FileUtils.remove_file './README.md'
