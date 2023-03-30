
## Node.js Guides

> Remaining docs short enough to be looked over together.

### Node.js Debugging Guide

* `node --inspect script.js` exposes an inspector client over Websocket at `ws://127.0.0.1:9229/${UUID}`; if connected process even after exit would wait for debugger to disconnect

* it could be exposed via `ssh -L 9229:localhost:9229 ${SERVER_USER}@${SERVER_IP}` on your local to remotely debug a server running app


### Easy Profiling Node.js Apps

* built-in profiler uses `V8's` sample-based [profiler](https://v8.dev/docs/profile) as `NODE_ENV=production node --prof script.js`

* this generates a tick file with `isolate-0x..log` filename form, can `node --prof-process $TICKFILE.log > processed.txt` to get info from it


### Diagnostics: Flame Graphs

* npm package `0x` can help do flame graph diagnostic in single command by running script as `0x script.js`; `-o` switch immediately opens flamegraph in browser, `-F` to output HTML.. this is not production friendly

> allows `--kernel-tracing` (default: false); at servers can use `--collect-only` to just capture stacks & later `0x --visualize-only ${PID}.0x`

* [this](https://nodejs.org/en/docs/guides/diagnostics-flamegraph) also explains how to use `perf` util for this


### Buffer

* Buffer represent fixed-length bytes, a subclass of `Uint8Array`, primarily useful to be sent over I/O channels

* Used like `const { Buffer } = require('node:buffer'); const buf = Buffer.from([1, 2, 3]);`


### Diagnostics: User Journey

* `node --inspect app.js` or `node --inspect-brk script.js`; then connecting to dev-tools instance in chrome gives access to Heap profiles

* `heap-profiler` package can be used for programmatic profiling

* Use linux tool `prof` to check on poor performance, or other similar tools


### Security Best Practices

* `npm audit` to check for vulnerability; `npm config set ignore-scripts true` to globally disable executing arbitrary scripts; `npm ci` to enforce lockfile

* `--secure-heap=n` to limit max heap allocation size

* `--frozen-intrinsics` to recursively freeze all built-in JS objects & functions

* `--disable-proto` to avoid Prototype Pollution Attacks

* `--policy-integrity` to avoid Policy mutations; using [policy mechanims with integrity checking](https://nodejs.org/api/permissions.html#integrity-checks)


### Don't Block the Event Loop (or Worker Pool)

* Keep work associated with each client at any time *small*. For callbacks on Event Loop and tasks in Worker Pool, this will bring best performance. To keep threads shuffling on what can be done, for best throughput.

* Post requiring modules & registering callbacks, Node.js apps enter Event Loop executing required callback at client requests. These shall have non-blocking async requests.

* Worker Pool (on `linuv`) handles I/O (dns, fs) or CPU (crypto, zlib) intensive tasks. Apps/module s using C++ add-on can submit other tasks to worker pool.

> Event Loop does pay minor setup costs for calling APIs on Worker Pool, but minor compared to keeping them in main thread.

> [Sam Roberts (IBM)'s talk with more details](https://www.youtube.com/watch?v=P9csgxBgaZ8)

* Pay attention to Time Complexity of callbacks; use `node-re2` module to avoid vulnerable regex which might slow down due to lot of backtracking

* Don't use synchronous APIs in  a Server callbacks

* `JSON.parse` & `JSON.stringify` are potentially expensive op; `JSONStream` & `bfj` packages offer async APIs

* Could partition call as below to lower cost on each event-loop call for an op

```
// O(n); 
function avgSync(n, callback) {
  let sum = 0;
  for (let i = 0; i <= n; i++)
    sum += i;
  let avg = sum / n;
  callback(avg);
}

// O(1)
function doAvgAsync(n, callback) {
  var sum = 0;
  function help(i, cb) {
    sum += i;
    if (i == n) {
      cb(sum);
      return;
    }

    // Asynchronous recursion, schedule next operation asynchronously.
    setImmediate(help.bind(null, i+1, cb));
  }

  // Start the helper, with CB to callback
  help(1, function(sum){
      var avg = sum/n;
      callback(avg);
  });
}

function avgAsync(n, callback) {
  let result;
  doAvgAsync(n, callback);
  return result;
}


avgSync(5, console.log);
avgAsync(5, console.log);
```

* If need multiple core and partitioning wouldn't cut it, then can offload to C++ addon (has downside of communication costs, abstractions, & few).

---

