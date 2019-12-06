defmodule P24ChatTest do
  use ExUnit.Case, async: true
  doctest P24Chat

  @tag :distributed
  test "send_message" do
    assert P24Chat.send_message(:eve@localhost, "hey") == :ok
  end
end
