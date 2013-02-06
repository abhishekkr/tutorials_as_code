#!ruby

# Lambda Literals

minimal = -> { p :called }
minimal.call

loaded = ->(arg, default = :default, &block) { puts ">>> #{arg} : #{default} : #{block}" }
loaded.call(:arg) { :block }
loaded.call(:arg, 'abcdefg') { :block }
