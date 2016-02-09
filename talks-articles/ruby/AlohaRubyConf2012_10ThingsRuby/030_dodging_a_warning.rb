#!ruby

$VERBOSE = true

class Warn
  def var
    @var ||= 42
  end
end

p Warn.new.var