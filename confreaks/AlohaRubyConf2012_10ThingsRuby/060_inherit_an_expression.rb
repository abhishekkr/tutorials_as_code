#!ruby

def Value(*fields)
  Class.new do
    define_method(:initialize) do |*args|
      fail ArgumentError, 'Wrong #of Args' unless args.size == fields.size
      fields.zip(args) do |field, arg|
        instance_variable_set("@#{field}", arg)
      end
    end
    fields.each do |field|
      define_method(field) { instance_variable_get("@#{field}") }
    end
  end
end

class Name < Value(:first, :last)
  def full
    "#{first} #{last}"
  end
end

ruby = Name.new("Ruby", "MRI")
puts ruby.full
