=begin
> can run it directly using ruby/irb

## Methods

```
=end

printf "1.next : %s\n", 1.next
printf "1.positive? : %s\n", 1.positive?
printf "1.between?(0,5) : %s\n", 1.between?(0,5)

printf "1.method(\"even?\").call : %s\n", 1.method("even?").call

=begin
```

* now a custom defined method, default returns, default value parameters

```
=end

def return10
  return 10
end

def returnTimes5(some_number)
  5 * some_number
end

printf "return10 == returnTimes5(2) : %s\n", (return10 == returnTimes5(2))

def nothing; end
printf "class of return from blank method: %s\n", nothing.class
printf "class of return : %s\n", return10.class

def greet(name, msg="hello!")
  msg + " " + name
end

printf "greet('jon doe') : %s\n", greet('jon doe')
printf "greet('jane doe', 'hya!') : %s\n", greet('jane doe', 'hya!')

=begin
```

* arraying args

```
=end

def sum(*numbers)
  numbers.reduce(0) {|sum, num| sum + num } # 'inject' is an alias of 'reduce'
end

printf "sum(1,10,100,1000,10000): %s\n", sum(1,10,100,1000,10000)


def dimsum(one, two, three)
    one + two + three
end

dimsum_numbers = [1, 2, 3] # Without a splat, this is just one parameter
printf "dimsum(*dimsum_numbers) : %s\n", dimsum(*dimsum_numbers)


def greet_all(msg, *names)
  for name in names
    printf "%s %s\n", msg, name
  end
end

p "greet_all 'hoye!', 'jack', 'jill', 'jane' :"
greet_all "hoye!", "jack", "jill", "jane"

def introduction(age, gender, *names)
  names.reduce('') {|msg, name| msg + ' ' + name}
  msg + ', who\'s ' + age + ' and ' + gender
end

=begin
```

* can skip braces on last parameter if its a hash

```
=end

def account(uuid, id, options)
  printf "login to '%s' with '%s' identifier; ", id, uuid
  if options.has_key? :password
    printf "use password to login, "
  else
    printf "you are a guest here, "
  end
  p options[:config]
end

account '0x0000', 'jack', password: 'passwd', config: {'sync': true}
account '0x0000', 'jill', guest: 'foo', config: {'sync': false}

=begin
```

---
---
=end
