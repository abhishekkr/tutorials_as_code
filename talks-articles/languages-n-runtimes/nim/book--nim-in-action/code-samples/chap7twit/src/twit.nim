import asyncdispatch
import jester
import times
## local imports
import ./db
import ./model
import ./views/general
import ./views/user
import ./views/login
import ./views/message

when isMainModule:
  db.migrate()

settings:
  port = Port(8080)

routes:
  get "/":
    if request.cookies.hasKey("username"):
      var u: User
      let exists = db.getUserIdByName(request.cookies["username"], u)
      if not exists:
        resp renderMain(renderLogin())
      var msgForIds = db.getSubscriptionIdsByUserId(u.id)
      msgForIds.add(u.id)
      let messages = db.getMessagesByUserIds(msgForIds)
      resp renderMain(
        renderShowMessage(messages, u),
        request.cookies["username"]
      )
    else:
      resp renderMain(renderLogin())
  post "/login":
    if @"username" != "":
      var u: User
      let exists = db.getUserIdByName(@"username", u)
      if not exists:
        u.username = @"username"
        db.createUser(u)
      var cookieTime = getTime().utc() + 2.hours
      setCookie("username", @"username", cookieTime)
    redirect("/")
  get "/logout":
    setCookie("username", "")
    redirect("/")
  get "/post-msg":
    if not request.cookies.hasKey("username"):
      redirect("/")
    let username = request.cookies["username"]
    resp renderMain(
      renderCreateMessage(username),
      request.cookies["username"]
    )
  post "/post-msg":
    if not request.cookies.hasKey("username"):
      redirect("/")
    var u: User
    let exists = db.getUserIdByName(request.cookies["username"], u)
    let message = Message(
      userID: u.id,
      msg: @"msg"
    )
    db.createMessage(message)
    redirect("/")
  get "/user/@username":
    var body: string
    if not request.cookies.hasKey("username"):
      redirect("/")
    var myU: User
    let myUsername = request.cookies["username"]
    if not db.getUserIdByName(myUsername, myU):
      redirect("/x")
    let mySubs = db.getSubscriptionNamesByUserId(myU.id)
    if myUsername == @"username":
      body = renderUser(myU, mySubs)
    else:
      var u: User
      if not db.getUserIdByName(@"username", u):
        redirect("/")
      let uSubs = db.getSubscriptionNamesByUserId(u.id)
      body = renderUser(u, uSubs, myU, mySubs)
    resp renderMain(body, myUsername)
