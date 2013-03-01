#!ruby

def banner(title, msg)
	puts "*"*50, title, "*"*50, msg
end

banner(<<HELLO, <<WORLD)
Hellllloooooo
HELLO
Worldly Objects
WORLD