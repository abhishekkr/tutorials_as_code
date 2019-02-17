RSpec.configure do |cfg|
  cfg.before(:all, :type => :focus) do
    @abc = 10
  end
  cfg.before(:all, :type => :wip) do
    @xyz = 10
  end
end

describe 'some domain', :type => :focus  do
  it 'should say abc'do
    expect(@abc).to eq 10
  end

  it 'should say not say xyz' do
    expect(@xyz).to be_nil
  end
end

describe 'other domain', :type => :wip do
  it 'should say abc'do
    expect(@abc).to be_nil
  end

  it 'should say not say xyz' do
    expect(@xyz).to eq 10
  end
end
