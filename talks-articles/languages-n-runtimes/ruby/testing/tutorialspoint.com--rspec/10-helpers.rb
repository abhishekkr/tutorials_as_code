class Food
  attr_reader :name, :cooked

  def initialize(name)
    @name = name
    @cooked = false
  end

  def name=(new_name)
    @name = new_name
  end

  def cook_food
    @cooked = true
  end
end

describe Food do
  let (:food) { Food.new 'apple' }

  it 'should creates new food' do
    expect(food).to have_attributes(name: 'apple')
    expect(food).to have_attributes(cooked: false)
  end

  it 'should change food' do
    food.name = 'potato'
    expect(food).to have_attributes(name: 'potato')
    expect(food).to have_attributes(cooked: false)
  end

  it 'should cook changed food' do
    food.name = 'potato'
    food.cook_food
    expect(food).to have_attributes(name: 'potato')
    expect(food).to have_attributes(cooked: true)
  end
end

## can use helper method for changed food flow
def new_changed_food(name)
  food = Food.new 'apple'
  food.name = 'potato'
  food
end

describe Food do
  it 'should creates new food' do
    food = Food.new 'apple'
    expect(food).to have_attributes(name: 'apple')
    expect(food).to have_attributes(cooked: false)
  end

  it 'should change food' do
    food = new_changed_food 'apple'
    expect(food).to have_attributes(name: 'potato')
    expect(food).to have_attributes(cooked: false)
  end

  it 'should cook changed food' do
    food = new_changed_food 'apple'
    food.cook_food
    expect(food).to have_attributes(name: 'potato')
    expect(food).to have_attributes(cooked: true)
  end
end
