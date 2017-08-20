=begin
> can run it directly using ruby/irb

## Modules

These are supposed to hold only behavior, not state.

This behavior can be provided through a class' object, if required.

```
=end

module VocalActions
  def shout
    'blaah blaah blaah'
  end
end

class Jack
  include VocalActions

  def spy
    'stealthy'
  end
end

class Jill
  include VocalActions

  def shoot
    'bang'
  end
end

puts Jack.new.shout
puts Jill.new.shout

=begin
```

* All 'class' instance belong to 'Class', 'module' instance to 'Module'.
'Module' is superclass to 'Class'.

```
=end

puts VocalActions.class
puts Jack.new.class
puts Jack.new.class.superclass
puts Module.superclass
puts Class.superclass

=begin
```

* NameSpacing for collision protection

```
=end

module Alice
  class Vocals
    def shout
      "whyyyyyyy"
    end
  end
end

module Bob
  class Vocals
    def shout
      "whaaaaaat"
    end
  end
end

puts Alice::Vocals.new.shout
puts Bob::Vocals.new.shout


class Jack
  def blank
    "hey ho"
  end
end

jack = Jack.new
puts jack.shout
puts jack.blank


class Jack
  def shout
    "heyyyyyyy"
  end
end

puts jack.shout
puts jack.blank

=begin
```

* Constants under Modules

```
=end

module Some
  SO_ME = 1
  module One
    SO_ME_ONE = 10
  end
end

puts Some::SO_ME
puts Some::One::SO_ME_ONE

=begin
```

---
---
=end
