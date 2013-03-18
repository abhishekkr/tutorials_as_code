#!ruby -d
# turn on $DEBUG flag on only when require way extra info

def var
  @var || 40
end

if $DEBUG
  puts "var is %p" % var
end

p var + 2
