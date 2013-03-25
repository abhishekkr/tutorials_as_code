#!ruby

def debug(name, content)
  puts "%s: %p" % [name, content]
end

debug "Num", 42
debug("Objects", {'a' => %w[all the alphabets]})