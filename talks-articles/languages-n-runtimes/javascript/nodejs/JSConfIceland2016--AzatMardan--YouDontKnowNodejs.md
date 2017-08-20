
## You Don't Know Nodejs
> at JSConf Iceland 2016
> by Azat Mardan

**5 core features you don't know**

### Why Use Node

* w/o callback structure

```
print 'something'
print 'more'
sleep 1000 ## nothing happens here
print 'something more'
```

* node 'sleep', scheduling in future with callback

```
console.log('something')

setTimeout(function(){
      console.log('something more')
    }, 1000)

console.log('more')
// can spin-up more tasks while wait on earlier
```

---

### non-blocking I/O

```
           ,-----------------------------,
 |R|       | ,,==    /\   D   ,--------, |
 |E| :) -> | ||event ||   E   | POSIX  | |  non-blocking i/o
 |Q|       | ||loop  ||   L   | ASYNC  | |  /
 |U| :) -> | =>      || <=E=> |THREADS |----\   <=====>
 |E|       | ||single||   G   |(=>)(=>)| |   \  |'''''|
 |S| :) => | ||thread||   A   |(=>)(=>)| |   /  |_____|
 |T|       | ||      ||   T   |        |----/
 |S| :) => | \/ =====''   E   '--------' |
           '-----------------------------'
```

* single-threaded, so at least easier on race conditions
> not good though, unless follow multiple worker process model
> but then you'll need to take care of risks with concurrency model

---

### Blocking JS to non-blocking

* blocking code, synchronous

```
var fs = require('fs');

var content = fs.readFileSync('accounts.txt', 'utf8')
console.log(contents)
console.log('hey')

var content = fs.readFileSync('ip.txt', 'utf8')
console.log(contents)
console.log('ho')
```

* non-blocking

```
var fs = require('fs')

fs.readFile('accounts.txt', 'utf8', function(error, contents){
      console.log(contents)
    })
console.log('hey')

fs.readFile('ip.txt', 'utf8', function(error, contents){
      console.log(contents)
    })
console.log('ho')
```

---

### Node !== BrowserJS

* creating `global` variables, no `window` in `nodejs`
> work with modules, get path to script
> create `global` or `GLOBAL`

```
global.propertyName
__propertyName

global.__filename
global.__dirname

global.module // to export
global.require()  //exported to require

global.process
process
process.pid
process.versions
process.arch
process.argv
process.env
process.uptime()
process.memoryUsage()
process.cwd()
process.exit()
process.kill()
```

---

to avoid `Callback Hell`, can use patterns like
* Promises
* Async/await
* Generator

or, better

### Events

Part of core and supported by most of core modules.

NodeJS Observer Pattern
* Subject
* Observers (listening) on a subject
* Evetn Trigger

```
var events = require('events') // to create observer
var emitter = new events.EventEmitter()

// attach event listener
emitter.on('done', function(results){
  console.log('Done:', result)
  })
```

* using event emitters

```
var events = require('events') // to create observer
var emitter = new events.EventEmitter()

emitter.on('knock', function(){
  console.log('Who is there?')
  })

emitter.on('knock', function(){
  console.log('Naah')
  })

emitter.emit('knock')
```

---

### Example EventEmitter

* inheriting from EventEmitter

```job.js
var util = require('util')
var Job = function Job() {
  //...
  this.process = function() {
    //...
    job.emit('done', {completedOn: new Date() })
  }
}

util.inherits(Job, require('events').EventEmitter)
```

```weekly.js
var Job = require('./job.js')
var job = new Job()

job.on('done', function(details){
  console.log('Job was completed at', details.completedOn)
  job.removeAllListeners()
  })

job.process()
```

* things with Event Listener

```
emitter.listeners(eventName)
emitter.on(eventName, listener)
emitter.once(eventName, listener)
emitter.removeListener(eventName, listener)
```

---

### Streams

speed to slow with large data, abstractions for continuous chunking of data

4 types
> * readable
> * writable
> * duplex
> * transform

#### Readable Streams

* listen from stdin, use data and end events

```stdin.js
process.stdin.resume()
process.stdin.setEncoding('utf8')

process.stdin.on('data', function(chunk){
  console.log('chunk: ', chunk)
  })

process.stdin.on('end', function(chunk){
  console.log('-----finito')
  })
```

* got new interface `read()`

```
var readable = getReadableStreamSomehow()

readable.on('readable', () => {
    var chunk
    while(null !== ( chunk = readable.read() )) {
      console.log('got %d bytes of data', chunk.length)
    }
  })
```


#### Writable Streams

```
process.stdout.write('whatever')
```

* http, client can start receiving data right away

```
const http = require('http')

var server = http.createServer( (req, resp) => {
  req.setEncoding('utf8')
  req.on('data', (chunk) => {
      transform(chunk) //defined somewhere
      })
  req.on('end' () => {
      var data = JSON.parse(body)
      res.end()
      })
  })

server.listen(3000)
```

#### Piping data from one stream to another

```
var r = fs.createReadStream('file.txt')
var z = zlib.createGzip()
var w = fs.createWriteStream('file.txt.gz')
r.pipe(z).pipe(w)
```

---

### `buffer` to work with binary data

```
Buffer.alloc(size)
Buffer.from(array)
Buffer.from(buffer)
Buffer.from(str[, encoding])
```

* when working with filesystem, by default data is in buffer

```buf.js
var buf = Buffer.alloc(26)
for(var i = 0; i < 26; i++) {
  buf[i] = i + 97
  }
console.log(buf)
console.log(buf.toString('utf8'))
```

* buffer conversion

```
buf.toString('ascii')
buf.toString('ascii', 0, 5)
buf.toString('utf8', 0 5)
buf.toString(undefined, 0, 5) // defaults to utf8
```

* `fs` module, data by default is a buffer

```
fs.readFile('/etc/passwd', function(err,data){
  if(err) return console.error(err)
  console.log(data)
  });
```

---

### Cluster Model

master start workers, worker do the job

```
var cluster = require('cluster') //core module

if(cluster.isMaster){
  for(var i=0; i<numCPUs, i++){
    cluster.fork()
  }
} else if (cluster.isWorker){
  //do the work now
}
```

* other alternatives
> * strong-cluster-control
> * [pm2](https://github.com/Unitech/pm2), easy to integrate

* spawn vs fork vs exec

```
require('child_process').spawn() // large data, stream, no new V8 instances
require('child_process').fork()  // new V8 instances, multiple workers; only with nodejs
require('child_process').exec()  // buffer, async, all data at once; usually for small commands
```

---

### Handle Async Errors

* try-catch bad in async mode

* listen to all `on error` events

* listen to `uncaughtException`

* use domain or `AsyncWrap`; domain is deprecated in 4.0

* notify (optional)

* exit and restart process

---

### C++ Addons

```hello.cc
#include <node.h>

namespace demo {
  using v8::FunctionalCallbackInfo;
  using v8::HandleScope;
  using v8::Isolate;
  using v8::Local;
  using v8::Object;
  using v8::String;
  using v8::Value;
  
  void Method(const FunctionalCallbackInfo<Value>& args) {
    Isolate* isolate = args.GetIsolate();
    args,GetReturnValue().Set(String::NewFromUtf8(isolate, "capital one"));
  }

  void init(Local<Object> exports){
    NODE_SET_METHOD(exports, "hello", Method);
  }

  NODE_SET_METHOD(addon, init)
} //namespace demo
```

```binding.gyp

{
  "targets" : [
    {
      "target_name": "addon",
      "sources": ["hello.cc"]
    }
  ]
}
```

```
## to do compilation; https://github.com/nodejs/node-gyp
node install -g node-gyp

node-gyp configure
node-gyp build
# check for compiled .nodejs in ./build/Release
```

```hello.js
var addon = require('./build/Release/addon')
console.log(addon.hello())

//node hello.js
```

---

## References

* [nodejs patterns](http://webapplog.com/node-patterns-from-callbacks-to-observer)

* [github azat-co nodejs patterns](https://github.com/azat-co/node-patterns)

* [buffer](http://bit.ly/1leAcZ1)

* [stream](https://github.com/substack/stream-adventure)
* [stream handbook](https://github.com/substack/stream-handbook)

---
---
