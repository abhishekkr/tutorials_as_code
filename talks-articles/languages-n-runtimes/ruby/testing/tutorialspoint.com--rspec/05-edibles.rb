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
    fruits = double('some-edible') ## doubling a some-edible
    veggies = double('some-edible') ## non-existent class

    allow(fruits).to receive(:name) { 'apple' }
    allow(veggies).to receive(:name) { 'beans' }

    edible = Edible.new [fruits, veggies]
    expect(edible.list_food_items).to eq('apple, beans')
  end
end

RSpec.describe 'simple double' do
  it 'should show sample doubles' do
    dbl = double("sample-dbl", foo: 10)
    expect(dbl.foo).to be (10)

    dblx = double("sample-dbl")
    allow(dblx).to receive(:bar).and_return(10)
    expect(dblx.bar).to be (10)
  end

  it 'should show loose doubles' do
    dbly = double("sample-dbl").as_null_object
    expect(dbly.bar.foo).to be (dbly)

    dblz = double("sample-dbl", foo: 100).as_null_object
    allow(dblz).to receive(:foo).and_return(10)
    expect(dblz.foo).to be (10)
  end
end
