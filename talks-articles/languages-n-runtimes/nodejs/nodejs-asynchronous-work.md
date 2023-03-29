
## Node.js Asynchronous Work

> [source](https://nodejs.dev/en/learn/)


### Asynchronous flow control

* Callbacks initiator style gets triggered passing value & callback to a middleware function processing on value & passing result to final call with callback to be made if deserved.

* Parallel sample, if `createUser` below manages it in async, if using callbacks

```
let count = 0;
let success = 0;
let delayRev = 5000;
const failed = [];
const users = ['Alice', 'Bob', 'Eve', 'Root'];
const ReservedUsers = ['Root', 'Admin'];

const createUser = async function(v, d) {
  return new Promise((resolve) => {
    const resVal = ReservedUsers.indexOf(v) >= 0 ?
      {msg: `${v} is Reserved User.`, err: true} :
      {msg: `Trying create User: ${v}`, err: false};
    setTimeout(() => resolve(resVal), d)
    console.log(`Queued: ${v} create with delay of ${d}`)
  });
};

async function dispatch(user, callback) {
  delayRev -= 1000;
  const result = await createUser(user, delayRev);
  callback(result);
}

function final(result) {
  console.log(`Result: ${result.count} attempts & ${result.success} users.`);
  if (result.failed.length)
    console.log(`Failed to send to:\n\t* ${result.failed.join('\n\t* ')}\n`);
}

users.forEach(async function (user) {
  await dispatch(user, function (response) {
    console.log(`Processing: ${user}`);
    (!response.err) ? success++ : failed.push(user);
    count++;
    (count === users.length) ? final({count, success, failed}) : null;
  });
});
```

*gives following output*

```
Queued: Alice create with delay of 4000
Queued: Bob create with delay of 3000
Queued: Eve create with delay of 2000
Queued: Root create with delay of 1000
Processing: Root
Processing: Eve
Processing: Bob
Processing: Alice
Result: 4 attempts & 3 users.
Failed to send to:
	* Root
```


### Overview of Blocking vs Non-Blocking

* `I/O` primarily refers to `libuv` supported disk & network interaction.

```
const fs = require('fs');
// BLOCKING
const data = fs.readFileSync('/README.md');
// NON-BLOCKING
fs.readFile('/README.md', (err, data) => {
  if(err) { throw err };
});
```

* Blocking & Unblocking calls on same flow shouldn't be mixed to avoid race conditions.


### JS Asynchronous Programming & Callbacks

> Callbacks have a habit of become nesting rabbit (hell) holes.

### Understanding process.nextTick()

* A Tick is one full event loop, `process.nextTick(fn)` invokes `fn` at end of current op.

*Below Code*

```
var gvar = 0;

console.log(`log#1 gives ${gvar++}`);

setImmediate(() => {
  console.log(`log#2 gives ${gvar++}`);
});

setTimeout(() => {
  console.log(`log#3 gives ${gvar++}`);
}, 0);

process.nextTick(() => {
  console.log(`log#4 gives ${gvar++}`);
});

console.log(`log#5 gives ${gvar++}`);
```

*gives*

```
log#1 gives 0
log#5 gives 1
log#4 gives 2
log#3 gives 3
log#2 gives 4
```

### Discover JS Timers

* `setTimeout(fn, delayMilliSec, param1, ..)` can be specified for a later callback to run with delay of `delayMilliSec` value provided; if not provided or given 0 it executes without delay

* `setInterval(fn, intervalMilliSec)` as a recursive timeout where `fn` is run every `intervalMilliSec`

### Understanding setImmediate()

* `setImmediate(fn)` is added to `macrotask queue` like that for `setTimeout`

* passed `fn` is executed in next interation of event loop.. where `process.nextTick queue` runs first, then `promises microtask queue` & after those the `macrotask queue`

> * some browsers (IE/Edge) provide `setImmediate()`, not others
>
> * `process.nextTick()` callback added to `process.nextTick queue`; `Promise.then()` callback added to `microtask queue`


### The Node.js Event emitter

* Node.js allows build DOM events like mechanism using Events Emitter.

* `emit` to trigger event; `on` to add callback when triggered

 * `once` to add a 1-time listener; `removeListener()` or `off()` to remove these

* `removeAllListeners` to remove all listeners for an event

*below code*

```
const EventEmitter = require('events');

const eventEmitter = new EventEmitter();

eventEmitter.on('start', (idx) => {
  console.log(`started ${idx}`);
});

console.log('before trigger')
eventEmitter.emit('start', 100);
console.log('after trigger')

eventEmitter.removeAllListeners();
eventEmitter.emit('start', 100);
```

*gives output as*

```
before trigger
started 100
after trigger
```

---

