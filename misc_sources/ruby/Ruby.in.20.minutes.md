=begin
## Ruby in Twenty Minutes
* [source](https://www.ruby-lang.org/en/documentation/quickstart/)

> Can run this Markdown file for output of code-examples via Ruby or IRB without any change.

#### Simple Tasks

```
=end

print "3+2 -> ",3+2,"\n"
print "3*2 -> ",3*2,"\n"
print "3**2 -> ",3**2,"\n"
print "Math.sqrt(9)-> ",Math.sqrt(9),"\n" # built-in module: Math

=begin
```

#### Methods

```
=end

# simple method
def what_ruby
  print RUBY_RELEASE_DATE," | ",RUBY_PLATFORM,"\n"
  RUBY_VERSION
end
# can call with/out paranthesis
what_ruby == what_ruby()

# method with a param
def ruby_version(ver)
  print "ruby #{ver}\n" # string interpolation
end
# calling methods with params within paranthesis
ruby_version(what_ruby)
# calling methods with params without paranthesis
ruby_version what_ruby

# method with default param value
def ruby_detail(key="description", val=RUBY_DESCRIPTION)
  print "[#{key}:] #{val}\n"
end

ruby_detail
ruby_detail("Ruby Description")
ruby_detail "copyright", RUBY_COPYRIGHT

=begin
```

#### Class
=end

class Chruby
  def initialize(name="ruby", version="2.2.3")
    @name = name
    @version = version
  end

  def change(name, version)
    @name = name
    @version = version
    print "changing ruby to #{@name}-#{@version}\n"
  end

  def install()
    print "installing #{@name}-#{@version}\n"
  end
end

chruby = Chruby.new()
chruby.install

chjruby = Chruby.new
chjruby.change "jruby", "1.9.3"
chjruby.install

print "instance_methods (all from now and ancestor classes): #{Chruby.instance_methods}\n"

print "instance_methods (just the methods defined for Chruby): #{Chruby.instance_methods(false)}\n"

## checking what attribute and methods defined for every object
print "does chjruby responds to 'name': #{chjruby.respond_to?("name")}\n"
print "does chjruby responds to 'install': #{chjruby.respond_to?("install")}\n"


=begin
```

* with attr_accessor
=end

class Chruby
  attr_accessor :name
end

chmri = Chruby.new("mri")
print "#{chmri.name}\n"

## checking if 'name' is available now that an attribute-accessor has been provided
print "does chjruby responds to 'name': #{chjruby.respond_to?("name")}\n"
print "does chjruby responds to 'name=': #{chjruby.respond_to?("name=")}\n"

=begin
```

* More responsible Class
=end

class Chrubies
  attr_accessor :name, :versions

  def initialize(name="ruby", version="2.2.3")
    @name = name
    @versions = versions
  end

  def change(name, version)
    if version.nil? or name.nil?          ## 'if' conditionals
      print "ruby or version detail is missing"
      return
    end
    if @versions.respond_to?("each")
      unless @versions.include? version   ## inverse of 'if' condition
        puts "given '#{version}' version doesn't belong to this '#{@versions.join(', ')}' chrubies set"
        return
      end
    elsif version != @versions
      puts "given '#{version}' version doesn't belong to this '#{@versions}' chruby"
      return
    end
    if name != @name
      puts "given '#{name}' ruby doesn't belong to this '#{@name}' chruby"
      return
    end
    self.install
    print "changing ruby to #{@name}-#{@versions}\n"
  end

  def install()
    if versions.nil? or name.nil?
      puts "no ruby desired for installation"
    elsif versions.respond_to?("each")
      @versions.each do |version|         ## looping if object responds to 'each'
        puts "installing #{@name}-#{version}"
      end
    else
      puts "installing #{@name}-#{@versions}"
    end
  end
end

=begin
```

* Running block only if Ruby script is called directly
=end

if __FILE__ == $0
  jrb = Chrubies.new
  jrb.install
  jrb.change "jruby", "1.9.3"

  jrb.name = "jruby"
  jrb.change "jruby", "1.9.3"

  jrb.versions = "2.2.3"
  jrb.change "jruby", "1.9.3"

  jrb.versions = ["1.9.3","2.2.3"]
  jrb.change "jruby", "1.9.3"

  jrb.change "ruby", "1.9.3"
end

=begin
```

* Can use more traditional looping when don't have an object to loop around
=end

for index in 1..3
  puts "element #{index} of 3"
end

=begin

---

Lot more to explore
> * different control structures
> * use of blocks and 'yield'
> * modules

---
=end
