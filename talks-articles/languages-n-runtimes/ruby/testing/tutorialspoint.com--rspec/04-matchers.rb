require 'rspec/autorun'

describe 'Example for matchers' do
  context 'identity matchers' do
    it 'should check equality' do
      a = 'test'
      b = a
      expect(a).to eq 'test'
      expect(a).to eql 'test'
      expect(a).to be b
      expect(a).to equal b
    end
  end

  context 'comparison matchers' do
    it 'should check comparison' do
      expect(1).to be > 0
      expect(1).to be >= 1
      expect(1).to be < 2
      expect(1).to be <= 1
      expect(1).to be_between(1,3).inclusive
      expect(1).to be_between(0,3).exclusive
      expect('test').to match(/TESt/i)
    end
  end

  context 'class/type matchers' do
    it 'should check class, parents and type' do
      expect(1).to be_instance_of Fixnum
      expect(1.2).to be_kind_of Numeric
      expect('test').to respond_to(:length)
    end
  end

  context 'true/false/nil matchers' do
    it 'should check tru-thy, fals-ey, nil' do
      expect(1 == 1).to be true
      expect(1 != 1).to be false
      expect(1).to be_truthy
      expect(nil).to be_falsey
      expect(nil).to be_nil
    end
  end

  context 'error matchers' do
    it 'should check errors' do
      expect {"1" + 1}.to raise_error(TypeError)
      expect {"1" + 1}.to raise_error("no implicit conversion of Integer into String")
      expect {"1" + 1}.to raise_error(TypeError, "no implicit conversion of Integer into String")
    end
  end
end
