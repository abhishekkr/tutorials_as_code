=begin
> can run it directly using ruby/irb

## Classes

* Grouping objects

```
=end

printf "1.class : %s\n", 1.class
printf "\"\".class : %s\n", "".class
printf "true.class : %s\n", true.class
printf "[].class : %s\n", [].class
printf "{}.class : %s\n", {}.class
printf "Object.class : %s\n", Object.class
printf "Class.class : %s\n", Class.class
printf "Module.class : %s\n", Module.class

p (1.class == 10.class)

printf "Class of Class: %s\n", 1.class.class

some_num = Object.new
p (some_num.is_a?(Object))

p (1.is_a?(Fixnum))

=begin
```

---

#### Building your own Class

```
=end

class BlankClass
  ## it need to have state and behavior
end

=begin
```
defining some with matter

```
=end

class Address
  def initialize(name, phone)
    @name = name
    @phone = phone
  end

  def name?
    if @name.empty?
      raise "ERROR: Anonymous"
    end
    p @name
  end

  def phone?
    if @phone.empty?
      raise "ERROR: Unreachable"
    end
    p @phone
  end
end

johndoe = Address.new("John Doe", "255.255.255.255")
p johndoe

=begin
```

---
---
=end
