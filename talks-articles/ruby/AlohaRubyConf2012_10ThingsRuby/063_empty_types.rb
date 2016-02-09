#!ruby

module MyNamespace
  module Errors
    # instead of: class MyNamespace < RunTimeError; end
    MyNamespaceError = Class.new(RuntimeError)
    WhateverError = Class.new(MyNamespaceError)
  end
end

p MyNamespace::Errors::WhateverError.ancestors
