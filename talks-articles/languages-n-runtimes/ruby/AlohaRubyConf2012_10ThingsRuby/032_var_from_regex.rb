#!ruby

if /\A(?<last>\w+),\s*(?<first>\w+)\z/ =~ "Bond, James"
	puts "#{first} #{last}"
end