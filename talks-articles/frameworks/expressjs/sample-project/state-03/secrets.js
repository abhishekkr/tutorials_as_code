/* routes module */

var express = require('express');
var router = express.Router();

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

router.get('/', function(req, res){
    res.send(secretRoot);
});

router.post('/', function(req, res){
    res.send("I don't wanna know.");
});

router.get('/:secretSet', function(req, res){
    res.send('your secret <b>' + req.params.secretSet + '</b>');
});

router.get('/:secretSet/:secretName[a-z0-9A-Z]{10}[a-z0-9A-Z]+', function(req, res){
    // above pattern matching requires secretName at least 11 chars
    res.send('your secret <b>' + req.params.secretSet + ":" + req.params.secretName + '</b>');
});

module.exports = router;
