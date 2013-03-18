#!ruby

params = {var: 100}

p params.fetch(:var)

p params.fetch(:missing, 42)
p params.fetch(:missing) { 40 + 2 }

begin
  params.fetch(:missing)
rescue
  puts  'Missing key fetch gives error, easy to dbg'
end
