var jsdom = require("jsdom");
var http = require("http");

http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain; charset=utf-8'});
  jsdom.env(
    '<p><a class="the-link" href="https://github.com/tmpvar/jsdom">jsdom\'s Homepage</a></p>',
    [],
    function (errors, window) {
     res.end("Contents of a.the-link: "+window.document.getElementsByClassName("the-link")[0].textContent);
    }
  );
}).listen(8888);

