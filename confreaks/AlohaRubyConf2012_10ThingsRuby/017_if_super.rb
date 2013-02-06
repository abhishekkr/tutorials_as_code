#!ruby

# delegate only if can

class DontDelegateToMe; end
class DelegateToMe; def delegate; ENV['USER']; end; end

module DelegateIfICan
  def delegate
    if defined? super
      "by #{super}"
    else
      'blocked'
    end
  end
end

puts DelegateToMe.new.extend(DelegateIfICan).delegate
puts DontDelegateToMe.new.extend(DelegateIfICan).delegate
