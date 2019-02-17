class SomeClass
  attr_accessor :msg

  def initialize
    @msg = "guess what"
  end

  def update_msg(msg)
    @msg = msg
  end
end

describe SomeClass do
  before(:each) do
    puts "for each"
    @some_class = SomeClass.new
  end

  it 'should have default msg' do
    expect(@some_class.msg).to_not be_nil
    @some_class.msg = 'guess this'
  end

  it 'should be able to update msg' do
    expect(@some_class.msg).to_not be 'guess this'
    @some_class.update_msg('guess again')
    expect(@some_class.msg).to_not be 'guess what'
  end
end

describe SomeClass do
  before(:all) do
    puts "for all"
    @some_class = SomeClass.new
  end

  it 'should have default msg' do
    expect(@some_class.msg).to_not be_nil
    @some_class.msg = 'guess this'
  end

  it 'should be able to update msg' do
    expect(@some_class.msg).to eq 'guess this'
    @some_class.update_msg('guess again')
    expect(@some_class.msg).to eq 'guess again'
  end
end
