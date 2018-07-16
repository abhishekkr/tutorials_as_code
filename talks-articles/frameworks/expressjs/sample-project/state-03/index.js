/* sample app */
var express = require('express');
var app = express();

var secrets = require('./secrets.js');

var err404 = `
<h1 style="text-align:center; width:100%">404 | The page is lost, so are you.</h1>
`;

app.all('/', function(req, res){
    res.send("try <a href='/secrets'>Secrets</a>");
});

app.use('/secrets', secrets);

app.get('*', function(req, res){
   res.send(err404);
});

app.listen(3000);
