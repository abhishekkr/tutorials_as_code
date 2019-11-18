## strings
#
#### * are sequence of bytes

hello = <<104, 101, 108, 108, 111>>
IO.inspect(hello)
hello <> <<0>> |> IO.inspect()  ## this concatenation displays string as binary

### charlists
#
#### * internally strings are sequence of bytes, strings are enclosed with double quotes
#### * charlists with single quotes; each value is Unicode codepoint of a character encoded as UTF-8
#
IO.inspect('hello')
IO.inspect(?Z) ## can get character code point using `?<char>`

### graphemes and codepoints
#
#### * codepoints are unicode char represented by one/more bytes depending on UTF-8 encoding
#### * non US ASCII chars need more than one byte, Graphemes consist of multiple codepoints rendered as single character
#### * 'String' module provides `graphemes/1` and `codepoints/1`
#
non_ascii = "\u0082\u0103"
String.codepoints(non_ascii) |> IO.inspect()
String.graphemes(non_ascii) |> IO.puts()


### string functions

#### length/1
String.length("four") |> IO.inspect()

#### replace/3
String.replace("four", "our", "ive") |> IO.inspect()

#### duplicate/2
String.duplicate("four", 4) |> IO.inspect()

#### split/2
String.split("four", "o") |> IO.inspect()


### exercise :: anagrams
defmodule Anagram do
  @spec is?(String.t(), String.t()) :: Boolean.t()
  def is?(word, ordw) do
    sort_string(word) == sort_string(ordw)
  end

  @spec sort_string(String.t()) :: String.t()
  defp sort_string(word) do
    word
    |> String.downcase()
    |> String.graphemes()
    |> Enum.sort()
  end
end

Anagram.is?("hi", "ih") |> IO.inspect()
Anagram.is?("hii", "hey") |> IO.inspect()
##################################################
