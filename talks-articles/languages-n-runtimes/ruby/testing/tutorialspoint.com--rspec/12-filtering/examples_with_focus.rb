require_relative 'spec_helper_include'

describe 'some domain' do
  it 'should say focus', suspicious: true do
    puts 'focus'
  end

  it 'should say de focus' do
    puts 'de focus'
  end
end
