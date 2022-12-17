import ../src/db
import ../src/model

import os, tables, times

when isMainModule:
  let testDbPath = "/tmp/twit-test.db"
  removeFile(testDbPath)
  changeDb(testDbPath)
  migrate()

  createUser(User(username: "alice"))
  createUser(User(username: "bob"))
  createUser(User(username: "charlie"))

  var alice, bob, charlie: User
  assert not getUserIdByName("fake-alice", alice)
  assert getUserIdByName("alice", alice)
  assert getUserIdByName("bob", bob)
  assert getUserIdByName("charlie", charlie)

  createMessage(Message(msg: "this is pyow", userID: alice.id))
  createMessage(Message(msg: "created in nim", userID: alice.id))
  createMessage(Message(msg: "using jester web framework", userID: bob.id))
  createMessage(Message(msg: "but no orm", userID: charlie.id))

  var twotxt = getMessagesByUserIds(@[alice.id, bob.id, charlie.id], limit=2)
  assert twotxt.len == 2
  var txt = getMessagesByUserIds(@[alice.id, bob.id, charlie.id])
  assert txt.len == 4
  var zerotxt = getMessagesByUserIds(@["fake"])
  assert zerotxt.len == 0

  createSubscription(Subscription(userID: alice.id, subscribedTo: bob.id))
  createSubscription(Subscription(userID: alice.id, subscribedTo: charlie.id))

  var subscriptions = getSubscriptionByUserIds(alice.id)
  var subUsers = initTable[string, string]()
  for sub in subscriptions:
    var u: User
    discard getUsernameById(sub.subscribedTo, u)
    subUsers[sub.subscribedTo] = u.username
  echo("")
  echo(subUsers)
  var subscriptionIds: seq[string]
  for sub in subscriptions:
    subscriptionIds.add(sub.subscribedTo)

  var subIds = getSubscriptionIdsByUserIds(alice.id) 
  assert subscriptionIds.len == subIds.len 

  var subscribedMessages = getMessagesByUserIds(subscriptionIds)
  echo("")
  echo("Messages for " & alice.username)
  for msg in subscribedMessages:
    echo("'" & msg.msg & "' by @" & subUsers[msg.user_id])
