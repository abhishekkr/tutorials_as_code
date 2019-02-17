class Food
  attr_reader :name, :type

  def initialize(name, type)
    @name = name
    @type = type
  end
end

describe Food do
  it 'creates new food' do
    food = Food.new 'apple', 'fruit'
    expect(food).to have_attributes(name: 'apple', type: 'fruit')
  end
end

## using subject feature to reduce code for above
describe Food.new 'apple', 'fruit' do
  it { is_expected.to have_attributes(name: 'apple', type: 'fruit') }
end
