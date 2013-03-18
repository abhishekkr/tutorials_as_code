#!ruby
# instead of: class Name <Struct.new(:first, :last)....end

Struct.new('Name', :first, :last) do
  def full
    "#{first} #{last}"
  end
end

abc = Struct::Name.new('Abc', 'Xyz')
puts abc.full
