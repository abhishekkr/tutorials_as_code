#!ruby
# instead of: class Name <Struct.new(:first, :last)....end

Name = Struct.new(:first, :last) do
  def full
    "#{first} #{last}"
  end
end

abc = Name.new('Abc', 'Xyz')
puts abc.full
