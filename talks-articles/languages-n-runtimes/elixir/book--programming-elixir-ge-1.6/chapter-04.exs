defmodule Shout do
  def users() do
    content = "some content to be not affected byuse of similar identifier in expression"
    usr = with {:ok, file}    = File.open("/etc/passwd"),
              content         = IO.read(file, :all),
              :ok             = File.close(file),
              [_, uid, gid]   = Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content)
          do
            "Group: #{gid}, User: #{uid}"
          end

    IO.puts usr
    IO.puts content
  end

  def users_handled() do
    with {:ok, file}    = File.open("/etc/passwd"),
         content         = IO.read(file, :all),
         :ok             = File.close(file),
         [_, uid, gid]   <- Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content)
    do
         "Group: #{gid}, User: #{uid}"
    end
  end

  def users_with_else() do
    with {:ok, file}    = File.open("/etc/passwd"),
        content         = IO.read(file, :all),
        :ok             = File.close(file),
        [_, uid, gid]   <- Regex.run(~r/^1xlp:.*?:(\d+):(\d+)/m, content)
    do
        "Group: #{gid}, User: #{uid}"
    else
        nil -> "no relevant user found"
        :error -> :error
    end
  end

  def users_handled_paren() do
    with(
        {:ok, file}    = File.open("/etc/passwd"),
        content        = IO.read(file, :all),
        :ok            = File.close(file),
        [_, uid, gid]  <- Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content)
    )do
        "Group: #{gid}, User: #{uid}"
    end
  end

  def users_do_colon() do
    with {:ok, file}    = File.open("/etc/passwd"),
         content        = IO.read(file, :all),
         :ok            = File.close(file),
         [_, uid, gid]  <- Regex.run(~r/^lp:.*?:(\d+):(\d+)/m, content),
    do: "Group: #{gid}, User: #{uid}"
  end
end

Shout.users
IO.puts Shout.users_handled
IO.puts Shout.users_with_else
IO.puts Shout.users_handled_paren
IO.puts Shout.users_do_colon
