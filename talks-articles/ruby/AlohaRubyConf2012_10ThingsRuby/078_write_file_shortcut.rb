#!ruby

File.write('output.log', "one\ntwo\nthree\n")
puts File.read('output.log'), '====='

File.write('output.log', "one, two and three", 4) #truncate to a pos and append
puts File.read('output.log'), '====='

File.write('output.log', "one\ntwo\nthree\n")
puts File.read('output.log'), '====='
