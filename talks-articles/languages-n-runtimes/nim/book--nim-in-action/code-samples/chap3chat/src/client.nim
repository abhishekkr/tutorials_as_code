## client module
import asyncdispatch, asyncnet
import os
import strutils
import threadpool
# local module
import protocol

type
  FlowControl = enum
    LEAVE,
    NEXT,
    STEP

proc showHelp =
  echo("""
  $help to show this help message
  $quit to quit the message loop
  """)

proc specialMsg(msg: string) =
  case msg
  of "$help": showHelp()
  else: showHelp()

proc sendMsg(socket: AsyncSocket, username, msg: string): FlowControl =
  result = FlowControl.STEP
  let message = createMsg(username, msg)
  if msg == "":
    return
  elif msg == "$quit":
    asyncCheck socket.send(message)
    result = FlowControl.LEAVE
  elif msg[0] == '$':
    specialMsg(msg)
    result = FlowControl.NEXT
  else:
    asyncCheck socket.send(message)
    echo("sent.")
    stdout.write(">> ")

proc connect(socket: AsyncSocket, serverAddr: string, port: int) {.async.} =
  echo("Connecting to.. " & serverAddr)
  await socket.connect(serverAddr, port.Port)
  echo("connected.")
  while true:
    let line = await socket.recvLine()
    let parsed = parseMsg(line)
    echo(parsed.username, " said: ", parsed.message)
    stdout.write(">> ")


if paramCount() < 3:
  quit("wrong usage; e.g. client localhost 7600 nickname")

let serverAddr =  paramStr(1)
let serverPort =  parseInt(paramStr(2))
let userName =  paramStr(3)

var socket = newAsyncSocket()
echo("connecting to: " & serverAddr & ":", serverPort)
asyncCheck connect(socket, serverAddr, serverPort)
echo("chat-client started..")

stdout.write(">> ")
var messageFlowVar = spawn stdin.readLine()  # message gets FlowVar[string]
while true:
  if messageFlowVar.isReady():
    case sendMsg(socket, username, ^messageFlowVar)
      of FlowControl.LEAVE: break
      of FlowControl.NEXT: continue
      else: discard
    stdout.write(">> ")
    messageFlowVar = spawn stdin.readLine()
  asyncdispatch.poll()
