import db_sqlite

import os
import strutils
import uuid4
# local modules
import model
import twit_util


var dbPath = cfgDbPath()
var dbconn = open(dbPath, "", "", "")


proc close*() =
  dbconn.close()


proc changeDb*(newPath: string) =
  close()
  dbPath = newPath
  dbconn = open(dbPath, "", "", "")


proc migrate*() =
  changeDb(cfgDbPath())
  let tbl = "dbmigrations"
  let s = sql("CREATE TABLE IF NOT EXISTS " & tbl & "(id VARCHAR(256) PRIMARY KEY);")
  dbconn.exec(s)
  let rows = dbconn.getAllRows(sql("SELECT id FROM " & tbl & ";"))
  var prevMigrations: seq[string]
  for row in rows:
    prevMigrations.add(row[0])
  for sqlFile in walkFiles(cfgMigrationsPath() / "*.sql"):
    let sqlFilename = splitFile(sqlFile)[1]
    if contains(prevMigrations, sqlFilename): continue
    let s = readFile(sqlFile)
    dbconn.exec(sql s)
    dbconn.exec(sql("INSERT INTO " & tbl & " VALUES (?);"), sqlFilename)
  echo("migrations done.")


## User DML
proc createUser*(u: User) =
  dbconn.exec(
    sql"INSERT INTO user VALUES (?, ?, ?);",
    uuid4(), u.username, atTime()
  )

proc getUsernameById*(userID: string, u: var User): bool =
  if userID.len == 0: return false
  let row = dbconn.getRow(sql"SELECT username, created_at FROM user WHERE id = ?;", userID)
  if row[0].len == 0: return false
  u.id = userID
  u.username = row[0]
  u.createdAt = row[1].parseInt().toTime()
  return true

proc getUserIdByName*(username: string, u: var User): bool =
  if username.len == 0: return false
  let row = dbconn.getRow(sql"SELECT id, created_at FROM user WHERE username = ?;", username)
  if row[0].len == 0: return false
  u.id = row[0]
  u.username = username
  u.createdAt = row[1].parseInt().toTime()
  return true
  


## Message DML
proc createMessage*(m: Message) =
  dbconn.exec(
    sql"INSERT INTO message VALUES (?, ?, ?, ?);",
    uuid4(), m.msg, m.userID, atTime()
  )

proc getMessagesByUserIds*(userIds: seq[string], limit = 25): seq[Message] =
  result = @[]
  if userIds.len == 0: return
  let selectClause = "SELECT id, msg, user_id, created_at FROM message"
  var whereClause: string
  let orderClause = "ORDER BY created_at DESC"
  let limitClause = "LIMIT " & $limit
  var userIdClauses: seq[string]
  for userID in userIds:
    userIdClauses.add("user_id = ?")
  if userIdClauses.len > 0:
    whereClause = "WHERE " & userIdClauses.join(" OR ")

  let rows = dbconn.getAllRows(
    sql(selectClause & " " & whereClause & " " & orderClause & " " & limitClause & " ;"),
    userIds
  )
  if rows.len == 0: return
  for row in rows:
    if row[0].len == 0: continue
    result.add(
      Message(id: row[0], msg: row[1], user_id: row[2], created_at: row[3].parseInt().toTime())
    )


## Subscription DML
proc createSubscription*(s: Subscription) =
  dbconn.exec(
    sql"INSERT INTO subscription VALUES (?, ?, ?);",
    s.user_id, s.subscribed_to, atTime()
  )

proc getSubscriptionByUserIds*(userID: string): seq[Subscription] =
  result = @[]
  if userId.len == 0: return
  let selectClause = "SELECT subscribed_to FROM subscription"
  let whereClause = "WHERE user_id = ?"
  let orderClause = "ORDER BY created_at DESC"
  let rows = dbconn.getAllRows(
    sql(selectClause & " " & whereClause & " " & orderClause & " ;"),
    userID
  )
  if rows.len == 0: return
  for row in rows:
    if row[0].len == 0: continue
    result.add(
      Subscription(userID: userID, subscribedTo: row[0])
    )

proc getSubscriptionIdsByUserId*(userID: string): seq[string] =
  result = @[]
  if userId.len == 0: return
  let selectClause = "SELECT subscribed_to FROM subscription"
  let whereClause = "WHERE user_id = ?"
  let orderClause = "ORDER BY created_at DESC"
  let rows = dbconn.getAllRows(
    sql(selectClause & " " & whereClause & " " & orderClause & " ;"),
    userID
  )
  if rows.len == 0: return
  for row in rows:
    if row[0].len == 0: continue
    result.add(row[0])

proc getSubscriptionNamesByUserId*(userID: string): seq[string] =
  result = @[]
  let userSubIds = getSubscriptionIdsByUserId(userID)
  var tmpU: User
  for subId in userSubIds:
    if getUsernameById(subId, tmpU):
      result.add(tmpU.username)
    
