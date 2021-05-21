defmodule DontUse do
  def xif(condition, clauses) do
    case condition do
      val when val in [false, nil] -> Keyword.get(clauses, :else, nil)
      _ ->  Keyword.get(clauses, :do, nil)
    end
  end
end

xiff = DontUse.xif 1 == 1, do: :gotthis, else: :notthis
IO.inspect(xiff)
DontUse.xif 1 == 1, do: (IO.puts("this but")), else: (IO.puts("that"))


defmodule MyMacro do
  defmacro one(param) do
    IO.inspect(param)
  end

  defmacro two(param) do
    IO.inspect(param)
    param #"last thing gets returned"
  end

  defmacro three(param) do
    IO.inspect(param)
    #quote do: IO.puts(param) ## wouldn't eval as param have not be eval-d yet
    quote do: IO.puts("say what :) :) :) :) :)")
  end

  defmacro four(param) do
    IO.inspect(param)
    quote do
      IO.puts("say: #{unquote(param)}")
    end
  end

  defmacro five(param) do
    IO.inspect(param)
    quote do
      IO.puts("say: #{unquote(param)}")
    end
  end
end

defmodule TestMacro do
  require MyMacro

  def one do
    MyMacro.one :atom
    MyMacro.one 1
    MyMacro.one [1,2,3]
    MyMacro.one "string"
    MyMacro.one {:a, 1, :b, 2}
    MyMacro.one %{a: 1, b: 2}
    MyMacro.one do: :this
    MyMacro.one do: (a = 1; a*a)
    MyMacro.one do
      :this
    else
      :that
    end
    IO.puts("...")
  end

  def two do
    MyMacro.two :atom
    MyMacro.two IO.puts("blah two")
    IO.puts("...")
  end

  def three do
    MyMacro.three :atom
    MyMacro.three IO.puts("blah three")
    IO.puts("...")
  end

  def four do
    MyMacro.four :atom
    MyMacro.four (b = 2; b*b)
    MyMacro.four IO.puts("blah four")
    IO.puts("...")
  end
end

TestMacro.one
TestMacro.two
TestMacro.three
TestMacro.four


## usable if implementation
defmodule UseThis do
  defmacro iff(condition, clauses) do
    quote do
      case unquote(condition) do
        val when val in [false, nil] ->
          unquote(Keyword.get(clauses, :else, nil))
        _ ->
          unquote(Keyword.get(clauses, :do, nil))
      end
    end
  end
end

defmodule TestUseThis do
  require UseThis

  def iff do
    iffx = UseThis.iff 1 == 1, do: :gotthis, else: :notthis
    IO.inspect(iffx)
    UseThis.iff 1 == 1, do: (IO.puts("this but")), else: (IO.puts("that"))
  end
end

TestUseThis.iff


defmodule MacroBinding do
  #defmacro nobind(name) do
  #  quote do
  #    def unquote(name)(), do: "> #{unquote(name)} <"
  #  end
  #end
  defmacro bind(name) do
    quote bind_quoted: [name: name] do
      def unquote(name)(), do: "> #{unquote(name)} <"
    end
  end
end

defmodule UseMacroBinding do
  require MacroBinding
  #[:xlice, :xob] |> Enum.each(&MacroBinding.nobind(&1))
  [:alice, :bob] |> Enum.each(&MacroBinding.bind(&1))
end

defmodule UseMacroBindingTest do
  require UseMacroBinding
  def run do
    UseMacroBinding.alice |> IO.inspect()
    UseMacroBinding.bob |> IO.inspect()
  end
end

UseMacroBindingTest.run


defmodule Scopex do
  defmacro update_local(val) do
    local = "some val"
    result = quote do
      local = unquote(val)
      IO.puts("End of macro body, local = #{local}")
    end
    IO.puts("In macro definition, local = #{local}")
    result
  end
end

defmodule UseScopex do
  require Scopex

  local = 101
  Scopex.update_local("alice")
  IO.puts("On return, local = #{local}")
end


defmodule Scopey do
  def code_eval do
    quote do: IO.puts(:hello)
    |> Code.eval_quoted()

    quote do: IO.puts(var!(myvar))
    |> Code.eval_quoted([myvar: "s-copey"])

    fragment = Code.string_to_quoted("defmodule M do def add(x,y) do x+y end end")
    IO.inspect(fragment)
    codestr = Macro.to_string(fragment)
    IO.inspect(codestr)
    "[a, a*b, c]" |> Code.eval_string([a: 2, b: 4, c: 6]) |> IO.inspect()
  end
end

Scopey.code_eval


defmodule Strlab do
  defmacro a + b do
    quote do
      to_string(unquote(a)) <> to_string(unquote(b))
    end
  end
end

defmodule UseStrlab do
  IO.puts(100+1)
  import Kernel, except: [+: 2]
  import Strlab
  IO.puts(100+1)
end
