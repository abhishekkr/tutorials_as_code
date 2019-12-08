## behaviors

defmodule ExampleBehavior.Worker do
  @moduledoc false

  @callback init(state :: term) :: {:ok, new_state :: term} | {:error, reason :: term}

  @callback perform(args :: term, state :: term) :: {:ok, result :: term, new_state :: term} | {:error, reason :: term, new_state :: term}
end


defmodule ExampleBehavior.Consumer do
  @behaviour ExampleBehavior.Worker

  def init(opts), do: {:ok, opts}

  def perform(url, opts), do: consume(url, opts)

  defp consume(source, opts), do: IO.puts "consume #{opts[:name]} from #{source}"
end


defmodule ExampleBehavior.Compressor do
  @behaviour ExampleBehavior.Worker

  def init(opts), do: {:ok, opts}

  def perform(content, opts), do: content |> zipit(opts) |> respond(opts)

  defp zipit(content, opts) do
    :zip.create(opts[:name], content)
  end

  defp respond({:ok, path}, opts), do: {:ok, path, opts}
  defp respond({:error, reason}, opts), do: {:reason, reason, opts}
end
