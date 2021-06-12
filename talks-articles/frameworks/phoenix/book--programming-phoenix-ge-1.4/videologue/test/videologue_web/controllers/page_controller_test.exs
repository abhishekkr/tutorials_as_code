defmodule VideologueWeb.PageControllerTest do
  use VideologueWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Start the videologue"
  end
end
