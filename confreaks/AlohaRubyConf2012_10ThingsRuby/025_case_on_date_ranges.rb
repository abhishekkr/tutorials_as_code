#!ruby

require 'date'

start_of_conf = Date.new(2012, 10, 8)
end_of_conf = Date.new(2012, 10, 9)

case Date.today
	when Date.new...start_of_conf
		puts 'started'
	when start_of_conf...end_of_conf
		puts 'going on'
	when (end_of_conf + 1)...Date::Infinity.new
		puts 'post conf'
	end