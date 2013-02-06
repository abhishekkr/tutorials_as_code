#!ruby

to_s_to_proc = :to_s.to_proc   # or: lambda(&:to_s)

receiver = 255
arg      = 16
puts to_s_to_proc[receiver, arg]
