import ../src/model
import ../src/views/user

import os, times

when isMainModule:
  var alice = User(id: "0xa", username: "Alice<>")
  var bobby = User(id: "0xa", username: "Bobby<>")
  echo renderUser(alice, @[])
  echo renderUser(alice, @[], bobby, @[alice.username])
  echo renderUser(alice, @[bobby.username], bobby, @[])
