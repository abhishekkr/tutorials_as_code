defmodule Chain do
  def counter(prev_pid) do
    receive do
      n ->
        send prev_pid, n + 1
    end
  end

  def create_procs(n) do
    code_to_run = fn(_, send_to) ->
      spawn(Chain, :counter, [send_to])
    end

    last = Enum.reduce(1..n, self(), code_to_run)

    send(last, 0)
    receive do
      final_answer when is_integer(final_answer) ->
        "Result is #{inspect(final_answer)}"
    end
  end

  def run(n) do
    :timer.tc(Chain, :create_procs, [n])
    |> IO.inspect()
  end
end

## elixir -r chain.exs -e "Chain.run(5)"
Chain.run(50_000)

# will fail for
## elixir -r chain.exs -e "Chain.run(500_000)"
### 13:24:34.926 [error] Too many processes
### ** (SystemLimitError) a system limit has been reached
#
# that is just default limit, not a hard limit for VM
## elixir --erl "+P 1000000" -r chain.exs -e "Chain.run(500_000)"
#
#± % elixir --erl "+P 1000000" -r chapter-15/chain.exs -e "Chain.run(500_000)"
#{1596325, "Result is 500000"}
## even now if process fails for limit, that's hard limit for your system
