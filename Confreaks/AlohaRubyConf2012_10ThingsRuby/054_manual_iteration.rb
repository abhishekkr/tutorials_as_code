#!ruby

row = %w[odd even].cycle

puts "<table>"
("A".."E").each do |letter|
  puts %Q{ <tr class="#{row.next}"><td>#{letter}</td></tr> } #row.next
end
puts "</table>"
