```
# install erlang
sudo pacman -S erlang ## can use suitable: apt-get, dnf, yum

# install elixir
sudo pacman -S elixir ## can use suitable: apt-get, dnf, yum

# install npm
sudo pacman -S nodejs npm ## can use suitable: apt-get, dnf, yum

# install hex package manager
mix local.hex

# cmd to install phoenix archive
mix archive.install hex phx_new 1.4.9

# create a new phoenix project
mix phx.new hello_phoenix

## if wanna avoid nodejs/npm can skip brunch.io
## mix phoenix.server hello_phoenix --no-brunch
cd hello_phoenix
mix ecto.create
npm install && node node_modules/brunch/bin/brunch build
mix deps.get

# start a server
mix phx.server

## can run it from IEx (Interactive Elixir)
## iex -S mix phx.server
## by default runs at http://localhost:4000
```

* add a new channel like `mix phx.gen.channel scuttlebutt`

* for persistence can create a new context like `mix phx.gen.context Chats Message messages from:string msg:text` and `mix ecto.migrate` to run migrations
