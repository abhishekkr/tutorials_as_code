defmodule Buggy do
  def parse_header(<<format::integer-16, tracks::integer-16, division::integer-16>>) do
    IO.puts "format: #{format} | tracks: #{tracks} | division: #{decode(division)}"
  end

  def decode(<< 1::1, beats::15 >>) do
    "♩ = #{beats}"
  end
  def decode(<< 0::1, fps::7, beats::8 >>) do
    "#{-fps} fps, #{beats}/frame"
  end

  def pry_parse_header(<<format::integer-16, tracks::integer-16, division::integer-16>>) do
    require IEx; IEx.pry
    IO.puts "format: #{format} | tracks: #{tracks} | division: #{decode(division)}"
  end

  def fixed_parse_header(<<format::integer-16, tracks::integer-16, division::bits-16>>) do
    IO.puts "format: #{format} | tracks: #{tracks} | division: #{decode(division)}"
  end
end


#header = << 0, 1, 0, 8, 0, 120 >>

#Buggy.parse_header(header) |> IO.inspect()
#"""
#** (FunctionClauseError) no function clause matching in Buggy.decode/1
#
#    The following arguments were given to Buggy.decode/1:
#
#        # 1
#        120
#
#    chapter-14-debug.exs:8: Buggy.decode/1
#    chapter-14-debug.exs:5: Buggy.parse_header/1
#    chapter-14-debug.exs:18: (file)
#    
#"""

#Buggy.fixed_parse_header(header) |> IO.inspect()

#Buggy.pry_parse_header(header) |> IO.inspect()
