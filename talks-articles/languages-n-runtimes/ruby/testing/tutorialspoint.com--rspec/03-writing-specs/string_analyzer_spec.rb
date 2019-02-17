require 'string_analyzer'

describe StringAnalyzer do
  let (:sa) {StringAnalyzer.new}
  context 'input validity' do
    it 'should detect string with vowels' do
      expect(sa.vowels? 'wow').to be true
    end

    it 'should detect string do not have vowels' do
      expect(sa.vowels? 'xyx').to be false
    end
  end
end
