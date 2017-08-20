#!/usr/bin/env ruby
# modules and mixins

module Math
  ALMOST_PI = 22.0/7.0

  class Calculator
    def add(num1, num2); num1 + num2; end
  end

  module Check
    def self.is_even?(num)
      (num & 1) == 0
    end
  end
end

puts Math::ALMOST_PI
puts Math::Calculator.new.add(1, 10)
puts Math::Check.is_even? 1
puts Math::Check.is_even? 10


#
## instance methods from modules in class
module Logger
  def log(msg)
    STDERR.puts msg
  end
end

class Server
  include Logger # anonymous Ghost Class will get included, sharing code with module
end

class Client
  extend Logger # anonymous Ghost Class will get included, sharing code with module
end

msg_queue = 'zmq'
msg_queue.extend Logger

msg_brokr = 'rmq'
class << msg_brokr
  include Logger
end

class Pub
  extend Logger
end


module Logger #inclusion is live
  def log(msg)
    STDERR.puts "[+] #{msg}"
  end
end


Server.new.log 'Socket Busy.'
Client.log 'Socket Closed.'
msg_queue.log 'ZeroMQ'
msg_brokr.log 'RabbitMQ'
Pub.log 'Publish'


#
## hook
module Persist
  module ClassMeth
    def find
      puts 'in find...'
      new
    end
  end

  def save; puts 'in save...'; end
end
class Eg1
  include Persist
  extend Persist::ClassMeth
end
p1 = Eg1.find
p1.save

module Persist2
  def self.included(clas)
    clas.extend ClassMeth
  end

  module ClassMeth
    def find
      puts 'in persist2 find...'
      new
    end
  end

  def save; puts 'in persist2 save...'; end
end
class Eg2
  include Persist2
end
p2 = Eg2.find
p2.save
