defmodule Midi do
  defstruct(content: <<>>)

  defmodule Frame do
    defstruct(
      type: "xxxx",
      length: 0,
      data: <<>>
    )

    def to_binary(%Midi.Frame{
      type: type,
      length: length,
      data: data
    }) do
      <<
        type::binary-4,
        length::integer-32,
        data::binary
      >>
    end
  end

  def from_file(name) do
    %Midi{content: File.read!(name)}
  end
end

defimpl Enumerable, for: Midi do
  def count(content = %Midi{}) do
    frame_count = Enum.reduce(content, 0, fn(_, count) -> count+1 end)
    {:ok, frame_count}
  end

  def member?(%Midi{content: content}, value = %Midi.Frame{}) do
    {:error, __MODULE__}
  end

  def reduce(%Midi{content: content}, acc, foo) do
    do_reduce(content, acc, foo)
  end

  def slice(%Midi{}) do
    {:error, __MODULE__}
  end

  ## this return tuple format are available as documentation for reduce
  ## under Enumerable protocol def
  defp do_reduce(_content, {:halt, acc}, _foo) do
    {:halted, acc}
  end
  defp do_reduce(content, {:suspended, acc}, foo) do
    {:suspended, acc, &do_reduce(content, &1, foo)}
  end
  defp do_reduce("", {:cont, acc}, _foo) do
    {:done, acc}
  end
  defp do_reduce(<<
    type::binary-4, length::integer-32, data::binary-size(length), rest::binary
    >>, {:cont, acc}, foo) do
    frame = %Midi.Frame{type: type, length: length, data: data}
    do_reduce(rest, foo.(frame, acc), foo)
  end
end

defimpl Collectable, for: Midi do
  use Bitwise

  def into(%Midi{content: content}) do
    {
      content,
      fn
        acc, {:cont, frame = %Midi.Frame{}} ->
          acc <> Midi.Frame.to_binary(frame)
        acc, :done ->
          %Midi{content: acc}
        _, :halt ->
          :ok
      end
    }
  end
end

defimpl Inspect, for: Midi do
  def inspect(%Midi{content: <<>>}, _opts), do: "#Midi[<<empty>>]"
  def inspect(%Midi{}, _opts) do
    content = Enum.map(midi, fn frame -> Kernel.inspect(frame) end) |> Enum.join("\n")
    "#Midi[\n#{content}\n]"
  end
end

defimpl Inspect, for: Midi.Frame do
  def inspect(%Midi.Frame{
      type: "MThd",
      length: 6,
      data: <<format::integer-16, tracks::integer-16, division::bits-16>>
    }, _opts) do
      beats = decode(division)
    "#Midi.Header{Midi format: #{format}, tracks: #{tracks}, timing: #{beats}}"
  end
  def inspect(%Midi.Frame{type: "MTrk", length: length, data: data}, _opts) do
    "#Midi.Track{length: #{length}, data: #{Kernel.inspect(data)}}"
  end

  def decode(<< 0::1, beats::15 >>), do: "♩ = #{beats}"
  def decode(<< 1::1, fps::7, beats::8 >>), do: "#{-fps} fps, #{beats}/frame"
end

defmodule TestMidi do
  def run do
    midifile = Path.dirname(__ENV__.file()) |> Path.join("es_ist_ein_ros.mid")
    midi = Midi.from_file(midifile)

    Enum.take(midi, 1) |> IO.inspect()  ## just needs reduce

    Enum.count(midi) |> IO.inspect()

    snd = Enum.at(midi, 2)
    Enum.member?(midi, snd) |> IO.inspect()

    lst = Enum.to_list(midi)
    IO.inspect(lst)

    Enum.into(lst, %Midi{}) |> IO.inspect()
  end
end

TestMidi.run()
