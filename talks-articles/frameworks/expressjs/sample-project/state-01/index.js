/* sample app */

var express = require('express');
var app = express();

var docRoot = `
<!doctype html>
<html>
<head><title>xpres</title></head>
<body>
<h3>let's try</h3>
</body>
</html>
`;

var secretRoot = `
<!doctype html>
<html>
<head><title>xpres secrets</title></head>
<body>
<h3>secrets</h3>
<div>no one knows</div>
</body>
</html>
`;

app.all('/', function(req, res){
    res.send(docRoot);
});

app.get('/secrets', function(req, res){
    res.send(secretRoot);
});

app.post('/secrets', function(req, res){
    res.send("I don't wanna know.");
});

app.listen(3000);
