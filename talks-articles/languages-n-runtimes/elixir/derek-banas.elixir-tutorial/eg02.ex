defmodule Eg02 do
  def main do
    tuple_stuff()
    list_stuff()
    map_stuff()
    pattern_stuff()
    hof_stuff()
  end

  def tuple_stuff() do
    a_tuple = {:James, 35, 120.5}
    IO.puts "a_tuple tuple:#{is_tuple(a_tuple)}"

    b_tuple = Tuple.append(a_tuple, "Bond")
    {_, elem2, _, _} = b_tuple
    IO.puts "James #{elem(b_tuple, 3)} #{elem2} | #{tuple_size(b_tuple)}"

    c_tuple = Tuple.delete_at(b_tuple, 0)
    d_tuple = Tuple.insert_at(c_tuple, 0, :Jane)
    clone_tpl = Tuple.duplicate(0, 7)
    IO.puts "#{elem(d_tuple, 0)} | clone_tpl tuple:#{is_tuple(clone_tpl)}"
  end

  def list_stuff() do
    a_lst = [1,10,100]
    b_lst = [1, 11, 121]
    c_lst = a_lst ++ b_lst
    d_lst = c_lst -- a_lst
    IO.puts "is 11 in d_lst: #{11 in d_lst}"

    [head|tail] = d_lst
    IO.write "Head: #{head} | hd: #{hd(d_lst)}"
    IO.write " | Tail: #{tail} | tl: "
    IO.inspect tl(d_lst)
    IO.inspect tl(d_lst), char_lists: :as_lists

    Enum.each a_lst, fn item ->
      IO.puts ">> #{item} <<"
    end

    words = ["Some", "set", "of", "words"]
    less_words = List.delete(words, "words")
    lesser_words = List.delete_at(less_words, 0)
    more_words = List.insert_at(words, 2, "elements")
    IO.puts "first: #{List.first(more_words)} | last: #{List.last(more_words)}"

    recursive_list_stuff(lesser_words)

    user = [name: "James", uid: 7] # list of key-val pairs
    IO.puts "name: #{user[:name]}"
  end

  def recursive_list_stuff([]), do: nil
  def recursive_list_stuff([item|items]) do
    IO.puts "[+] #{item}"
    recursive_list_stuff(items)
  end

  def map_stuff() do
    capitals = %{"NationX" => "CityP", "NationK" => "CityQ"}
    IO.puts "Capital of NationX is #{capitals["NationX"]}"

    capitalx = %{:nationX => "CityP", :nationK => "CityQ"}
    IO.puts "Capital of NationX is #{capitalx.nationX}"

    capitalt = Map.put_new(capitalx, :nationR, "CityW")
    IO.puts "Capital of NationR is #{capitalt.nationR}"

    capitaly = Map.delete(capitalt, :nationX)
    IO.inspect Map.keys(capitaly)
  end

  def pattern_stuff() do
    a_lst = [11, 12, 13]
    [fst, snd, thrd] = a_lst
    [_, _, thd] = a_lst
    [[f,_], _] = [[fst+1, thrd+2], [snd+3, thd+4]]

    {_, {_, g}} = {:what, {:is, :this}}

    IO.puts "matched | f: #{f} | g: #{g}"
  end

  def hof_stuff() do ## hof - higher order function
    multiply = fn(x, y) -> x * y end ## anon func
    divide = &(&1 / &2) ## shorthand anon func
    IO.puts "2*25: #{multiply.(2,25)} | 25/2: #{divide.(25,2)}"

    # all have 1 arity, uses tuple match
    on_arg_match = fn
      {} -> IO.puts "no arg"
      {x} -> IO.puts "one arg #{x}"
      {x,y} -> IO.puts "two args, #{x} and #{y}"
      _ -> IO.puts "many args"
    end
    on_arg_match.({})
    on_arg_match.({1})
    on_arg_match.({1,2,3})

    with_default_param()
    with_default_param(11, 12)
    with_default_param(a=14)
  end

  def with_default_param(a \\ 10, b \\ 20) do
    IO.puts a * b
  end
end
