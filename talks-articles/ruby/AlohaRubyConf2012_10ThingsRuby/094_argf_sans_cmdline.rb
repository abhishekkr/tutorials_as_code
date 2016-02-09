#!ruby

conactenated_files = ARGF.class.new('full_name.erb', 'last_name_first.erb')
conactenated_files.each_line do |l|
  puts l
end
