#!ruby
# object wraps unix style file args in IO

# ruby [].rb one.log two.txt three.yaml
ARGF.each_line do |l|
  puts "#{ARGF.lineno}: #{l}"
end
