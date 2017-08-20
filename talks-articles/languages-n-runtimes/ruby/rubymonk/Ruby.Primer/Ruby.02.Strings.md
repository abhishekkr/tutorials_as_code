
=begin
## Strings

```
=end

print """
>> \"\" == ''
=> #{"" == ''}
>> \"abc\" == 'abc'
=> #{"abc" == 'abc'}

>> \"str\".instance_of? String
=> #{"str".instance_of? String}
>> \"str\".length
=> #{"str".length}

>> \"Ruby Version \#{RUBY_VERSION}\"
=> #{"Ruby Version #{RUBY_VERSION}"}
>> 'Ruby Version \#{RUBY_VERSION}'
=> #{'Ruby Version #{RUBY_VERSION}'}

>> \"lisp prolog haskell ocaml ruby python golang rust elixir\".include? \"ruby\"
=> #{"lisp prolog haskell ocaml ruby python golang rust elixir".include? "ruby"}
>> \"lisp prolog haskell ocaml ruby python golang rust elixir\".include? \"java\"
=> #{"lisp prolog haskell ocaml ruby python golang rust elixir".include? "java"}

>> \"jack and jill\".start_with? \"jack\"
=> #{"jack and jill".start_with? "jack"}
>> \"jack and jill\".start_with? \"jill\"
=> #{"jack and jill".start_with? "jill"}
>> \"jack and jill\".end_with? \"jack\"
=> #{"jack and jill".end_with? "jack"}
>> \"jack and jill\".end_with? \"jill\"
=> #{"jack and jill".end_with? "jill"}

>> \"Interactive Ruby REPL\".index \"R\"
=> #{"Interactive Ruby REPL".index "R"}
>> \"Interactive Ruby REPL\".rindex \"R\"
=> #{"Interactive Ruby REPL".rindex "R"}

>> \"xTerm\".upcase
=> #{"xTerm".upcase}
>> \"xTerm\".downcase
=> #{"xTerm".downcase}
>> \"xTerm\".swapcase
=> #{"xTerm".swapcase}
>> \"xTerm\".capitalize
=> #{"xTerm".capitalize}

>> \"Jake\".casecmp(\"jake\")
=> #{"Jake".casecmp("jake")}
>> \"Jake\".casecmp(\"jakes\")
=> #{"Jake".casecmp("jakes")}
>> \"Jake\".casecmp(\"jak\")
=> #{"Jake".casecmp("jak")}
>> \"Jake\".casecmp(\"JAKE\")
=> #{"Jake".casecmp("JAKE")}

>> \"hey,\".chomp
=> #{"hey,".chomp}
>> \"hey, \".chomp
=> #{"hey, ".chomp}
>> \"hey, \n\".chomp
=> #{"hey, \n".chomp}
>> \"hey, \n\r\".chomp
=> #{"hey, \n\r".chomp}
>> \"hey, \r\n\".chomp
=> #{"hey, \r\n".chomp}
>> \"hey, \r\".chomp
=> #{"hey, \r".chomp}
>> \"hey\".chomp(\"ey\")
=> #{"hey".chomp("ey")}


>> \'Jack and Jill\'.split
=> #{'Jack and Jill'.split}
>> \'Jack and Jill\'.split(\"and\")
=> #{'Jack and Jill'.split("and")}
>> \'Jack and Jill\'.split(/a/)
=> #{'Jack and Jill'.split(/a/)}
>> \'Jack and Jill\'.split(/a../)
=> #{'Jack and Jill'.split(/a../)}

>> \"jack\" + \" and\" + \" jill\"
=> #{"jack" + " and" + " jill"}
>> \"jack\" << \" and\" << \" jill\"
=> #{"jack" << " and" << " jill"}
>> \"jack\".concat(\" and\").concat(\" jill\")
=> #{"jack".concat(" and").concat(" jill")}
    
>> \'jack and jill and jane\'.sub(\'and\', \'or\')
=> #{'jack and jill and jane'.sub('and', 'or')}
>> \'jack and jill and jane\'.gsub(\'and\', \'or\')
=> #{'jack and jill and jane'.gsub('and', 'or')}
>> \'jack and jill AND jane\'.gsub(/AND/i, \'or\')
=> #{'jack and jill AND jane'.gsub(/AND/i, 'or')}

>> \'jack and jill\'.match(/a../)[0]
=> #{'jack and jill'.match(/a../)[0]}
>> \'jack and jill\'.match(/a../){|_m| print \"wokay \" + _m}
=> #{'jack and jill'.match(/a../){|_m| print "wokay " + _m} }

>> puts \"it might be and\" if /a../ =~ \"ands\"
=> #{puts "it might be and" if /a../ =~ "ands"}
>> puts \"it might be and\" if /a../ =~ \"and\"
=> #{puts "it might be and" if /a../ =~ "and"}
>> puts \"it might be and\" if /a../ =~ \"or\"
=> #{puts "it might be and" if /a../ =~ "or"}

"""

=begin
```

---

=end
