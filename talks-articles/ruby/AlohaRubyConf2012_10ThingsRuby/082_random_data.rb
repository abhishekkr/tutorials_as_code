#!ruby

require 'securerandom'

p SecureRandom.random_number
p SecureRandom.random_number(100)
puts
p SecureRandom.hex(20)
p SecureRandom.base64(20)
p SecureRandom.urlsafe_base64(20)
p SecureRandom.random_bytes(20)
puts
p SecureRandom.uuid
