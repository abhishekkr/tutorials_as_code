#!/usr/bin/env ruby
# instance_eval

instance_eval{ puts self }

'cat'.instance_eval{ puts self } # use receiver
'cat'.instance_eval{ puts upcase } # all methods on self value


#
##
class Thing
  def initialize
    @var = 100
  end

  def self.hello1
    puts self
  end

  private

  def secret
    'not gonna tell'
  end
end
t = Thing.new
puts t.instance_eval{ @var }

begin
  puts t.secret
rescue Exception => e
  puts e
  puts "[+] #{t.instance_eval{ secret }}" # break privacy
end


#
## define methods using it
animal = 'cat'
animal.instance_eval { # 'cat'-->{anonGhostClass}-->String-->Object
  def speak
    puts 'meow'
  end
}
animal.speak

Thing.instance_eval{
  def hello2
    puts self
  end
}
Thing.hello1
Thing.hello2


#
## more, break-in n modify
puts t.instance_eval("@var")
puts t.instance_eval{ @var }
t.instance_eval{ @var = 10 }
puts t.instance_eval{ @var }
