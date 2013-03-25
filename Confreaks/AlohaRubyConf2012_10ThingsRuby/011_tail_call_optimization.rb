#!ruby

# Tail Call optimization, no stack too deep

RubyVM::InstructionSequence.compile_option = { tailcall_optimization: true, trace_instruction: false }

eval <<vend
  def factorial(n, result = 1)
    if n == 1
      result
    else
      factorial( n - 1, n * result)
    end
  end
vend

p factorial(30000)
