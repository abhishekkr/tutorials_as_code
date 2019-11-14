## sigils

### Sigils Overview
#### * alternative syntax for representing and working with literals
#### * Sigil start with '~' followed by a char, there are built-in and new can be created
#
#### list of available Sigils include:
#### * '~C' generate a char list WITH NO escaping or interpolation
#### * '~c' generate a char list WITH escaping or interpolation
#### * '~R' generate RegExp WITH NO escaping or interpolation
#### * '~r' generate RegExp WITH escaping or interpolation
#### * '~S' generate string WITH NO escaping or interpolation
#### * '~s' generate string WITH escaping or interpolation
#### * '~W' generate word list WITH NO escaping or interpolation
#### * '~w' generate word list WITH escaping or interpolation
#### * '~N' generates a 'NaiveDateTime' struct
#
#### list of delimiter include:
#### * '<...>' pair of pointy brackets
#### * '{...}' pair of curly brackets
#### * '[...]' pair of square brackets
#### * '(...)' pair of parentheses
#### * '|...|' pair of pipes
#### * '/.../' pair of forward slashes
#### * '"..."' pair of double quotes
#### * "'...'" pair of single quotes

#### Char List
IO.inspect(~c/2 + 7 = #{2 + 7}/)
IO.inspect(~C/2 + 7 = #{2 + 7}/)


#### Regular Expression
#### * syntax, 'match =~ re'
IO.inspect("Elixir" =~ ~r/elixir/)
IO.inspect("Elixir 4" =~ ~r/Elixir #{2 + 2}/)
IO.inspect("Elixir #\{2 + 2\}" =~ ~R/Elixir #{2 + 2}/)
IO.inspect("Elixir" =~ ~r/elixir/i)
Regex.split(~R/_/, "a_b_c") |> IO.inspect()


#### String
~s/hey #{String.downcase("JANE")}/ |> IO.inspect()
~S/hey #{String.downcase("JANE")}/ |> IO.inspect()


#### Word List
~w/hey #{String.downcase("JANE")}/ |> IO.inspect()
~W/hey #{String.downcase("JANE")}/ |> IO.inspect()


#### NaiveDateTime
#### * useful to create a struct representing 'DateTime' without timezone, useful for pattern matching
NaiveDateTime.from_iso8601("2015-01-23 23:55:02") == {:ok, ~N[2015-01-23 23:55:02]} |> IO.inspect()


### Creating Sigils
defmodule SigilX do
  def sigil_d(number, []), do: number <> number
end
defmodule ExampleSigil do
  import SigilX

  def x, do: ~d/20/ |> IO.inspect()
end
ExampleSigil.x()

####
