```
# install erlang
sudo pacman -S erlang

# install elixir
sudo pacman -S elixir 

# install npm
sudo pacman -S nodejs npm

# install hex package manager
mix local.hex

# cmd to install phoenix archive
mix archive.install https://github.com/phoenixframework/phoenix/releases/download/v1.0.3/phoenix_new-1.0.3.ez

# create a new phoenix project
mix phoenix.new ./hello-phoenix
## if wanna avoid nodejs/npm can skip brunch.io
## mix phoenix.server ./hello-phoenix --no-brunch
cd hello-phoenix
mix ecto.create
npm install && node node_modules/brunch/bin/brunch build
mix deps.get

# start a server
mix phoenix.server
## can run it from IEx (Interactive Elixir)
## iex -S mix phoenix.server
## by default runs at http://localhost:4000
```
