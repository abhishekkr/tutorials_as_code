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

module.exports = router;
