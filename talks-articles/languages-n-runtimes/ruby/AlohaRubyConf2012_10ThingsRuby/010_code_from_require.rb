#!ruby

# if wanna do any code analysis, placing SCRIPT_LINES__ = {}
# before requiring any script stick all code to it
# as {filename_rb => [line1, line2,....
SCRIPT_LINES__ = { }
require_relative '011'
p SCRIPT_LINES__.keys

# ridiculous church of 80 characters line
if SCRIPT_LINES__.values.flatten.any?{|lyn| lyn.size > 80 }
  abort 'clean-up your code first'
end
