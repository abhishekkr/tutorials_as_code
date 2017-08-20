#!ruby
#human readable db

require 'yaml/store'

db = YAML::Store.new('accounts.yaml')

db.transaction do
  db['j'] = 100.00
  db['d'] = 100.00
end

db.transaction do
  db['d'] += 200.00
  db['j'] -= 200.00
  db.abort if db['j'] < 0.0
end

db.transaction do
  db['d'] += 20.00
  db['j'] -= 20.00
  db.abort if db['j'] < 0.0
end

db.transaction(true) do
  puts "J: %p" % db['j']
  puts "D: %p" % db['d']
end
