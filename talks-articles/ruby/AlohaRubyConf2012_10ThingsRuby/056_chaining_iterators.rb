#!ruby

p ("a".."z").each_cons(3)
            .map(&:join)
            .select{|l| l =~ /[aeiouy]/ }
