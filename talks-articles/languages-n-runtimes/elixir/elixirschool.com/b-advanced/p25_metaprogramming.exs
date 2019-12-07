## metaprogramming

defmodule SampleMacro do
  defmacro bang(expr) do
    quote do
      !unquote(expr)
    end
  end

  defmacro bangif(expr, do: block) do
    quote do
      if unquote(expr) |> bang(), do: unquote(block)
    end
  end

  def use_bang do
    bang(:false) |> IO.inspect
    bang([1]) |> IO.inspect

    bangif :true, do: IO.puts "yes"
    bangif :false, do: IO.puts "yesss"
  end
end

SampleMacro.use_bang


defmodule DisplayMacro do
  defmacro banner(msg) do
    if System.get_env("loud", "enabled") == "enabled" do
      quote do
        IO.puts("***************** #{unquote(msg)} *******************")
      end
    end
  end
end

defmodule ExampleDisplay do
  require DisplayMacro

  def test do
    DisplayMacro.banner("headline 1")
  end
end

ExampleDisplay.test


defmodule MacroDebug do
  require SampleMacro

  def expand do
    someq = quote do
      SampleMacro.bang(:false)
    end
    otherq = quote do
      SampleMacro.bangif(:false, do: :ohk)
    end

    IO.puts("with expand_once/2")
    someq |> Macro.expand_once(__ENV__) |> Macro.to_string |> IO.puts
    otherq |> Macro.expand_once(__ENV__) |> Macro.to_string |> IO.puts

    IO.puts("with expand/2")
    someq |> Macro.expand(__ENV__) |> Macro.to_string |> IO.puts
    otherq |> Macro.expand(__ENV__) |> Macro.to_string |> IO.puts
  end
end

MacroDebug.expand


defmodule Unhygienic do
  defmacro foo do
    quote do
      bar = 1
      var!(baz) = 1
    end
  end

  def test do
    bar = 10
    baz = 10
    foo()
    IO.puts("bar: #{bar} | baz: #{baz}")
  end
end

Unhygienic.test


defmodule MacroBinding do
  defmacro echo_twice(expr) do
    quote do
      IO.puts(unquote(expr))
      IO.puts(unquote(expr))
    end
  end

  defmacro echo_thrice(expr) do
    quote bind_quoted: [expr: expr] do
      IO.puts(expr)
      IO.puts(expr)
      IO.puts(expr)
    end
  end

  def test do
    echo_twice(:os.system_time)  ## re-eval for time
    echo_thrice(:os.system_time) ## no re-eval for time
  end
end

MacroBinding.test
