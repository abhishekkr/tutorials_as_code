class Edible
  def initialize(foods)
    @foods = foods
  end

  def list_food_items
    @foods.map(&:name).join(', ')
  end
end

RSpec.describe Edible do
  it 'should list the food items as csv' do
    fruits = double('some-edible')
    veggies = double('some-edible')

    allow(fruits).to receive(:name).and_return('apple')
    allow(veggies).to receive(:name).and_return('beans')

    edible = Edible.new [fruits, veggies]
    expect(edible.list_food_items).to eq('apple, beans')
  end
end

RSpec.describe 'simple double' do
  it 'should show loose doubles' do
    dblz = double("sample-dbl")
    allow(dblz).to receive(:foo).and_return(10)

    ## even if stub doesn't get used test passes
  end
end
