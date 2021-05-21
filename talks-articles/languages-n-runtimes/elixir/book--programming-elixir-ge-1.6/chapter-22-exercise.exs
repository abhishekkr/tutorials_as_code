defmodule MacrosAndCodeEval1 do
  defmacro myunless(condition, clauses) do ## regular allowed here
    quote do
      if unquote(condition) do
        unquote(Keyword.get(clauses, :else, nil))
      else
        unquote(Keyword.get(clauses, :do, nil))
      end
    end
  end
end

defmodule UseMacrosAndCodeEval1 do
  require MacrosAndCodeEval1

  def run do
    MacrosAndCodeEval1.myunless 1 == 1, do: (IO.puts("there is an error")), else: (IO.puts("there is an unless"))

    MacrosAndCodeEval1.myunless 1 == 2, do: (IO.puts("there is no unless"))
  end
end

UseMacrosAndCodeEval1.run


defmodule MacrosAndCodeEval2 do
  defmacro times_n(num) do
    quote do
      defmacro unquote(:"times_#{num}")(n), do: n * unquote(num)
    end
  end

  defmacro add_n(num) do
    quote do
      def unquote(:"add_#{num}")(n), do: n + unquote(num)
    end
  end
end

defmodule UseMacrosAndCodeEval2 do
  require MacrosAndCodeEval2

  MacrosAndCodeEval2.times_n(5)
  MacrosAndCodeEval2.times_n(10)
  MacrosAndCodeEval2.add_n(5)
  MacrosAndCodeEval2.add_n(10)
end

defmodule UseMacrosAndCodeEval2Test do
  require UseMacrosAndCodeEval2

  def run do
    IO.puts UseMacrosAndCodeEval2.times_5(10)
    IO.puts UseMacrosAndCodeEval2.times_10(5)
    IO.puts UseMacrosAndCodeEval2.add_5(10)
    IO.puts UseMacrosAndCodeEval2.add_10(5)
  end
end

UseMacrosAndCodeEval2Test.run


defmodule MacrosAndCodeEval3 do
  defmacro explain(do: expr) do
    IO.inspect(expr)
    do_explain(expr)
  end

  defp do_explain({op, _meta,
    [{left_op, lmeta, left_args},
     {right_op, rmeta, right_args}]} = _expr) do
    left = do_explain {left_op, lmeta, left_args}
    right = do_explain {right_op, rmeta, right_args}
    block_op(op, left, right)
  end
  defp do_explain({op, _meta,
    [{left_op, lmeta, left_args},
     right]} = _expr) do
    left = do_explain {left_op, lmeta, left_args}
    block_op(op, left, right)
  end
  defp do_explain({op, _meta,
    [left,
     {right_op, rmeta, right_args}]} = _expr) do
    right = do_explain {right_op, rmeta, right_args}
    block_op(op, left, right)
  end
  defp do_explain({:+, _meta, [left, right]} = _expr) do
    "add #{left} and #{right}"
  end
  defp do_explain({:-, _meta, [left, right]} = _expr) do
    "subtract #{right} from #{left}"
  end
  defp do_explain({:*, _meta, [left, right]} = _expr) do
    "multiply #{left} with #{right}"
  end
  defp do_explain({:/, _meta, [left, right]} = _expr) do
    "divide #{left} by #{right}"
  end

  defp block_op(op, blockl, blockr) when not is_integer(blockl) and not is_integer(blockr) do
    {opaction, opassociate} = opcode(op)
    "#{blockl}; then #{opaction} #{opassociate} result of #{blockr}"
  end
  defp block_op(op, blockl, blockr) when is_integer(blockl) do
    {opaction, opassociate} = opcode(op)
    "#{blockr}, then #{opaction} #{blockl} #{opassociate} it"
  end
  defp block_op(op, blockl, blockr) when is_integer(blockr) do
    {opaction, opassociate} = opcode(op)
    "#{blockl}, then #{opaction} #{opassociate} #{blockr}"
  end
  defp block_op(_op, _blockl, _blockr) do
    raise "Oh No!"
  end

  defp opcode(:/), do: {"divide", "by"}
  defp opcode(:*), do: {"multiply", "with"}
  defp opcode(:+), do: {"add", "to"}
  defp opcode(:-), do: {"subtract", "by"}
end

defmodule UseMacrosAndCodeEval3 do
  require MacrosAndCodeEval3

  def run do
    ## add 1 and 2
    IO.puts(MacrosAndCodeEval3.explain do: 1 + 2)
    ## subtract 2 from 1
    IO.puts(MacrosAndCodeEval3.explain do: 1 - 2)
    ## multiply 1 with 2
    IO.puts(MacrosAndCodeEval3.explain do: 1 * 2)
    ## divide 1 by 2
    IO.puts(MacrosAndCodeEval3.explain do: 1 / 2)
    ## multiply 2 with 3, then add 1 to it, then subtract by 4
    IO.puts(MacrosAndCodeEval3.explain do: 1 + 2 * 3 - 4)
    ## multiply 11 with 2; then subtract by result of multiply 3 with 14
    IO.puts(MacrosAndCodeEval3.explain do: 11 * 2 - 3 * 14)
  end
end

UseMacrosAndCodeEval3.run
