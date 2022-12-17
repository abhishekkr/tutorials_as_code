#? stdtmpl(subsChar = '$', metaChar = '#', toString = "xmltree.escape")
#import "../model"
#import xmltree
#
#
#proc renderUser*(u: User, subs: seq[string]): string =
#  result = ""
<div id="user">
  <h1>${u.username}</h1>
  <span>${$subs.len}</span>
  <div><a href="/logout">Logout!</a></div>
</div>
#end proc
#
#
#proc renderUser*(u: User, subs: seq[string], myU: User, mySubs: seq[string]): string =
#  result = ""
<div id="user">
  <h1>${u.username}</h1>
  <span>Following: ${$subs.len}</span>
  #if myU.username in subs:
  <span class="tag">follows you</span>
  #end if
  #if u.username notin mySubs:
  <form action="follow" method="post">
    <input type="hidden" name="subscribe_to" value="${u.username}">
    <input type="hidden" name="my_id" value="${myU.id}">
    <input type="submit" value="Subscribe">
  </form>
  #end if
</div>
#end proc
