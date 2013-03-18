#!ruby
#1.9

require 'open-uri'

open('http://rubyrogues.com/feed') do |feed|
  puts feed.read.scan(%r{<title>(\d+\s*RR\b[^<]*)</title>})
end
