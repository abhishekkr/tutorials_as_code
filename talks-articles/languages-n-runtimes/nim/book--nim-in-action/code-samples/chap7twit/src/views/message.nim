#? stdtmpl(subsChar = '$', metaChar = '#')
#import xmltree, times
##import user      ## local view
#import ../model
#
#
#proc `$!`(text: string): string = escape(text)
#end proc
#
#
#proc renderCreateMessage*(username: string): string =
#  result = ""
<div id="newMessage">
  <span>Write a new message:</span>
  <form action="post-msg" method="post">
    <div>
      <label for="msg">Message:</label>
      <input type="textarea" rows="4" cols="80" name="msg">
    </div>
    <input type="hidden" name="username" value="${$!username}">
    <input type="submit" value="Login">
  </form>
</div>
#end proc
#
#
#proc renderShowMessage*(messages: seq[Message], myU: User): string =
#  result = ""
<div id="messages">
  #if messages.len == 0:
    <div class="alternative">
      <span>You don't seem to have any messages.</span>
    </div>
  #end if
  #for msg in messages:
    # var msgCreatedAt = msg.createdAt.format("yyyy-MM-dd\'T\'HH:mm:ss", utc())
    <div>
      <a href="/${$!myU.username}">${$!myU.username}</a>
      <span>${$!msgCreatedAt}</span>
      <h3>${$!msg.msg}</h3>
    </div>
  #end for
</div>
#end proc
