## protocol module
import json

type
  Message* = object
    username*: string
    message*: string

proc parseMsg*(data: string): Message =
  let jdata = parseJson(data)
  result.username = jdata["username"].getStr("<n0name>")
  result.message = jdata["message"].getStr("...")

proc createMsg*(username, msg: string): string =
  result = $(%{
    "username": %username,
    "message": %msg
  }) & "\c\l"


when isMainModule:
  block:
    # happy path
    let data = """
      {"username": "Jane Doe", "message": "hola!"}
      """
    let jane = parseMsg(data)
    doAssert jane.username == "Jane Doe"
    doAssert jane.message == "hola!"
    # buggy path
    let badData = "not json"
    try:
      discard parseMsg(badData)
    except JsonParsingError:
      doAssert true
    except:
      doAssert false
    echo("passed.")
  block:
    let expectedJson = """{"username":"jane","message":"hola!"}""" & "\c\l"
    doAssert createMsg("jane", "hola!") == expectedJson
