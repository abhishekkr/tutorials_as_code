module Foo
  class Bar
    attr_accessor :something
  end
end

describe Foo::Bar, some: 'chip' do
  context 'using metadata', thing: 'micro'  do
    it 'should access data from describe' do |example|
      foobar = Foo::Bar.new
      foobar.something = "#{example.metadata[:thing]}#{example.metadata[:some]}"
      expect(foobar.something).to eq('microchip')
    end

    it 'should list all keys for metadata' do |eg|
      puts "\nmetadata keys: " + eg.metadata.keys.join(', ')
    end
  end
end
