headings = [ '1.1 Aaaaa',
             '1.2 Bbbbb',
             '1.3 Ccccc',
             '2.1 Xxxxx',
             '2.2 Yyyyy']

headings.chunk {|heading|
  heading[/\A\d+/] }
.each do |chapter, headings|
  puts "Chapter #{chapter}:"
  puts headings.map{|heading| "   #{heading}"}
end
