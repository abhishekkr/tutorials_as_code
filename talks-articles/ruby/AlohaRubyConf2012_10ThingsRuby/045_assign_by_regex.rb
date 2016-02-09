#!ruby

str = '$24.95 per seat'

str[/\$\d+(?:\.\d+)/] = '$9.99'
puts str

str[/\$\d+(?:\.\d+)(\b)/, 1] = ' USD'
puts str
