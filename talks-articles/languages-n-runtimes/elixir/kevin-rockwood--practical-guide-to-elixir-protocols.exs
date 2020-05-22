#########################################################################
## Kevin Rockwood - A Practical guide to Elixir Protocols (ElixirConf EU)
# github.com/rockwood

defprotocol Blankable do
  def blank?(term)
end

defimpl Blankable, for: BitString do
  def blank?(""), do: true
  def blank?(_), do: false
end

defimpl Blankable, for: Map do
  def blank?(map), do: map_size(map) == 0
end

defmodule Post do
  defstruct [:title, :body]

  defimpl Blankable do
    def blank?(%{body: nil}), do: true
    def blank?(%{body: _}), do: false
  end
end

## iex(1)> Blankable.blank?("f")
## false
## iex(2)> Blankable.blank?("")
## true
## iex(3)> Blankable.blank?(nil)
## ** (Protocol.UndefinedError) protocol Blankable not implemented for nil of type Atom
##     protocol.exs:3: Blankable.impl_for!/1
##     protocol.exs:4: Blankable.blank?/1
## iex(3)> Blankable.blank?(%{body:nil})
## ** (SyntaxError) iex:3: keyword argument must be followed by space after: body:
##
## iex(3)> Blankable.blank?(%{body: nil})
## false
## iex(4)> Blankable.blank?(%Post{body: nil})
## true


## # To apply a default implementation "Any" with fallback to any

defprotocol PrefixStar do
  @fallback_to_any true
  def starify(term)
end

defimpl PrefixStar, for: Any do
  def starify(""), do: "* ..."
  def starify(term), do: "* " <> term
end


## * this can also be achieved by @derive syntax, without fallback

defprotocol BolderMarkdown do
  def bold(term)
end

defimpl BolderMarkdown, for: Any do
  def bold(""), do: "** ... **"
  def bold(term), do: "** " <> term <> " **"
end

defmodule XPost do
  @derive [BolderMarkdown]
  defstruct [:title, :body]
end


## # Expression Problem

defprotocol Area do
  def calc(shape)
end

defmodule Rectangle do
  defstruct length: 0, width: 0

  defimpl Area do
    def calc(%{length: length, width: width}) do
      length * width
    end
  end
end

defmodule Circle do
  defstruct radius: 0

  defimpl Area do
    def calc(%{radius: radius}) do
      :math.pi() * :math.pow(radius, 2)
    end
  end
end

## now new type could be added
#
## also new function as below

defimpl Perimeter, for: Rectangle do
  def calc(%{length: length, width: width}) do
    (2 * length) + (2 * width)
  end
end

defimpl Perimeter, for: Circle do
  def calc(%{length: length, width: width}) do
    2 * :math.pi() * radius
  end
end

defmodule Printer do
  def print(shape) do
    IO.puts Perimeter.cacl(shape)
  end
end


## # Protocols that ship with Elixir
#
# * String.Chars ~ to convert datatypes into string like to_string/1
#
# * Collectable ~ to get values out of a collection
#
# * Inspect ~ pretty print data structs
#
# * IEx.Info ~ prints helpful info on IEX session
#
#  * Enumerable ~ used by Enum & Stream modules (can implement via count/1 member?/2 reduce/3)


## # Protocols in the wild
#
# * Poison ~ (elixir data-struct to json & back)
# > Poison.encode
# > can implement Poison.Encoder for your own datatypes for custom behavior converting to JSON
#
# * Scrivener ~ (pagination, works with Ecto)
# > Scrivener.Paginater implemented for EctoQuery, can be implemented for custom type say on Cache


## # API Integration
#
# * can be implemented using Protocols say like `build_request` & `parse_response`
# * with `send_message` being common for anything


## # If each implementagtion of function will eed same data type use Behavior
#    if not, use Protocols
#
#    * like if message to be sent via different channels, the data-type message remians same, so a behavior would be better
#
#########################################################################
