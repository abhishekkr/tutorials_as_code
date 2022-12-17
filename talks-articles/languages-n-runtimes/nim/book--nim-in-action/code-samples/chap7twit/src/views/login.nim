#? stdtmpl(subsChar = '$', metaChar = '#')
#
#
#proc renderLogin*(): string =
#  result = ""
<div id="login">
  <span>Existing User</span>
  <form action="login" method="post">
    <div>
      <label for="name">Name:</label>
      <input type="text" name="username">
    </div>
    <input type="submit" value="Login">
  </form>
</div>
#end proc
