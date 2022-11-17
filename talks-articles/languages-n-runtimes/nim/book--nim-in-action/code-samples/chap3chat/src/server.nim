## server module
import asyncdispatch, asyncnet

type
  Client = ref object
    socket: AsyncSocket
    netAddr: string
    id: int
    connected: bool
  Server = ref object
    socket: AsyncSocket
    clients: seq[Client]

## for string representation in echo
proc `$`(client: Client): string =
  "#" & $client.id & " (" & client.netAddr & ")"

## for msg processing of client
proc handleMsg(server: Server, client: Client) {.async.} =
  while true:
    let line = await client.socket.recvLine()
    if line.len == 0 or line == "$quit":
      echo("Disconnecting.. ", client)
      client.connected = false
      client.socket.close()
      return
    echo(client, ": ", line)
    for c in server.clients:
      if c.id != client.id and c.connected:
        await c.socket.send(line & "\c\l")

proc connLoop(server: Server, port = 7600) {.async.} =
  server.socket.bindAddr(port.Port)
  server.socket.listen()
  echo("started listening at: ", port)
  while true:
    let (clientAddr, clientSocket) = await server.socket.acceptAddr()
    echo("Accepted new client from ", clientAddr)
    let newClient = Client(
      socket: clientSocket,
      netAddr: clientAddr,
      id: server.clients.len,
      connected: true
    )
    server.clients.add(newClient)
    asyncCheck handleMsg(server, newClient)

proc newServer(): Server =
  Server(socket: newAsyncSocket(), clients: @[])

var server = newServer()
waitFor connLoop(server)
