defmodule Distill20Test do
  use ExUnit.Case
  use Plug.Test
  doctest Distill20

  @options Distill20.init([])

  test "/" do
    conn = :get |> conn("/", %{}) |> Distill20.call(@options)

    assert conn.state   == :file
    assert conn.status  == 200
  end

  test "/ping" do
    conn = :get |> conn("/ping", %{}) |> Distill20.call(@options)

    assert conn.state   == :sent
    assert conn.status  == 200
  end

  describe "/stack" do
    test "GET" do
      conn = :get |> conn("/stack", %{}) |> Distill20.call(@options)

      assert conn.state   == :sent
      assert conn.status  == 200
    end

    test "PUSH" do
      conn = :post |> conn("/stack", %{}) |> Distill20.call(@options)

      assert conn.state   == :sent
      assert conn.status  == 200
    end
  end
end
