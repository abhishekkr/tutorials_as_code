#!ruby

str = "Price $24.95"

p str[/\$\d+(?:\.\d+)?/]

p str[/\$(\d+)(?:\.\d+)?/, 1]
p str[/\$(?<dollars>\d+)(?:\.\d+)?/, :dollars]
