#? stdtmpl(subsChar = '$', metaChar = '#')
#
#
#proc renderMain*(body: string, username = "guest"): string =
#  result = ""
<!DOCTYPE html>
<html>
  <head>
    <title>Nim's Twit</title>
    <link rel="stylesheet" type="text/css" href="style.css">
  </head>
  <body>
    <div id="topbar">
      <span class="title">Twit</span>
      <span class="atright">
        <span class="pad-rl">
          <a href="/">Home</a>
        </span>
        <span class="pad-rl">
          <a href="/post-msg">Post</a>
        </span>
        <span class="pad-rl">
          <a href="/user/${username}">Profile</a>
        </span>
      </span>
    </div>
    <div id="content">
      ${body}
    </div>
  </body>
</html>
#end proc
