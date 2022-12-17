import ../src/model
import ../src/views/message

import os, times

when isMainModule:
  var alice = User(id: "0xa", username: "Alice<>")
  echo renderCreateMessage(alice.username)
  echo renderShowMessage(
    @[Message(id: "ax01", msg: "Whoa first message", userID: "0xa", createdAt: getTime())],
    alice)
