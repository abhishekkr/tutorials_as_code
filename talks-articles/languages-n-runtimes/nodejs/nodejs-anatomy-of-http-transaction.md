
## Node.js, Anatomy of HTTP Transaction

> [source](https://nodejs.org/en/docs/guides/anatomy-of-an-http-transaction)

* Stream is to work with streaming data, all stream are instances of EventEmitter. EventEmitter object emit an event, triggering all functions attached to the event synchronously.

> below is a code sample running http server in scratch JS doing some basic request handling; frameworks like `express` & `fastify` ease this up under abstraction

```
/*
 *
 * curl -v -H 'USER-AGENT: mysecretclient' localhost:3000/ -d 'damn'
 * curl -v -H 'USER-AGENT: mysecretclient' localhost:3000/echo -d 'what' -XGET
 * curl -v -H 'USER-AGENT: mysecretclient' localhost:3000/
 *
 */


const http = require('http');

// here function passed to createServer gets called at each request
// const server = http.createServer((req, res) => { foo(..).. });
const server = http.createServer();

var lastBody = [];
// 'server' object here is an EventEmitter, so can add listener later like
server.on('request', (request, response) => {
  if (httpBad(request, response)) {
    console.log("checking request validity");
    return;
  }
  response.on('error', (err) => { console.error(err); });
  if (request.method == 'GET' && request.url === '/echo') {
    request.pipe(response); // simple echo; using ReadableStream pipe to WriteableStream
    return;
  }
  response.statusCode = 200;
  response.setHeader('Content-Type', 'plain/text');
  
  let body = [];
  response.write("What's after\n");
  if (request.method == 'POST' && request.url === '/') {
    request.on('error', (err) => {
        console.error(err);
      }).on('data', (chunk) => {
        body.push(chunk);
      }).on('end', () => {
        body = Buffer.concat(body).toString();
        response.write(">>" + body.toString());
        lastBody = body;
      });
  } else {
      response.write(lastBody.toString());
  }
  response.end();
});


function httpBad(request, response) {
  // request object (instance of IncomingMessage) has data like method, url, headers, etc
  const { headers } = request;
  const userAgent = headers['user-agent']; // all headers would be available on lowercase
  // rawHeaders is available as well
  if (userAgent !== 'mysecretclient') {
    response.statusCode = 404;
    response.setHeader('Content-Type', 'application/json');
    response.end('{"error": "bad-request"}');
    return true;
  }
  return false;
}

server.listen('3000', '127.0.0.1', () => {
  console.log(`Server running at http://127.0.0.1:3000/`);
});
```

---

