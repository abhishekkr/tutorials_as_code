
## Node.js, Domain Module Postmortem

> Significant overhead is a deterrant for using domains. Without domains, perf is way greater for built-in http benchmark.

* `domain` module intercepts unhandled error via internal/external binding

```
var domain = require('domain');
var EventEmitter = require('events').EventEmitter;

var emitterX = new EventEmitter();
var child = domain.create();

const handleError = function(err) {
  console.error(`domain handled error: ${err.message}`);
};

/* external binding */
child.on('error', handleError)
// emitterX.emit('error', new Error('not to be intercepted.'))
child.add(emitterX);
emitterX.emit('error', new Error('to be intercepted.'))
emitterX.removeAllListeners('error');
child.remove(emitterX);

/* internal binding */
// child.on('error', handleError);
child.run(function() {
    fs.stat('/tmp', (err, stats) => { // shall raise error for 'fs' itself
      if (err) {
        throw err;
      }
    });
});
```

* if a module (like below) `enter()` into a domain but never `exit()`; all uncaught exceptions might leave problem hard to debug

```
const d = require('domain').create();
d.on('error', () => {
  /* doesn't actually reflect on error */
});
d.enter();
```

* if `error` handler was set on emitter & async calls have multi-module depth with one of them not including error handler.. domain creator will be catching unexpected exceptions going unnoticed by debugger

* nested domains can cause issues with error bubbling up correctly; error propagation is tricky


### API Gaps

* APIs using `EventEmitter` can `bind()` & error callback can `intercept()`; APIs implicitly binding to active domain must be executed inside of `run()`


### Resource cleanup on Exception

* On failure within domain API, attempt proper & complete resource cleanup. The more resources in play, greater the sharing & cleanup complexity.

* Resource propagation is complex as well with multiple async domains.

---

