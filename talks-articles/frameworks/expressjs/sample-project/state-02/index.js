/* sample app */
var express = require('express');
var app = express();

var secrets = require('./secrets.js');

app.all('/', function(req, res){
    res.send("try <a href='/secrets'>Secrets</a>");
});

app.use('/secrets', secrets);

app.listen(3000);
