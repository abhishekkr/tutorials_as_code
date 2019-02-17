require 'rspec/autorun'

class HelloWorld
  def say_hello
    "Hello World"
  end
end

describe HelloWorld do
  context "When testing HelloWorld class for behavior" do
    it "should say 'Hello World'" do
      hw = HelloWorld.new
      msg = hw.say_hello
      expect(msg).to eq "Hello World"
    end
  end
end
