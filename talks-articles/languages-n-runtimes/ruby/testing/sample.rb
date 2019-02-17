require 'rspec/autorun'

class String_Op
  def concat(s1, s2)
    "#{s1}#{s2}"
  end

  def concat_with_space(s1, s2)
    "#{s1} #{s2}"
  end

  def prefix_plus(str)
    "+ #{str}"
  end
end

describe String_Op do
  let(:string_op) {String_Op.new}

  context "concatenation operation" do
    it 'concats 2 string' do
      expect(string_op.concat('hel', 'lo')).to eq('hello')
    end

    it 'concats 2 string with space' do
      expect(string_op.concat_with_space('hel', 'lo')).to eq('hel lo')
    end
  end

  it 'prefix string with plus sign' do
    expect(string_op.prefix_plus('this')).to eq('+ this')
  end
end
