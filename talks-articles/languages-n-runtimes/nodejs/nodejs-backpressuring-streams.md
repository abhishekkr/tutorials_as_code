
## Node.js, Backpressuring in Streams

> [source](https://nodejs.org/en/docs/guides/backpressuring-in-streams)

* `Backpressure`, data buildup behind buffer during data transfer.. `Stream` with `EventEmitters` are to help.

> Not handling backpressure could slow down all other processes, overwork GC & get memory exhaustion.

* `process.stdin` & `process.stdout` are both Stream instances; Streams are havily used

* Scenario, take a large file to compress using `zlib` module

```
const gzip = require('zlib').creaeGzip();
const fs = require('fs');

const inputStream = fs.createReadStream(FILE_PATH);
const outputStream = fs.createWriteStream(`${FILE_PATH}.gz`);

inputStream.pipe(gzip).pipe(outputStream);
```

* using `pipeline` would manage errors and destroy all if either streams cause error

```
const { pipeline } = require('stream');
const gzip = require('zlib').creaeGzip();
const fs = require('fs');

pipeline(
  fs.createReadStream(FILE_PATH),
  gzip,
  fs.createWriteStream(`${FILE_PATH}.gz`),
  (err) => {
    err ? console.error('Pipeline Failed: ', err) : console.log('Pipeline Passed.');
  }
);
```

* or do with `promisify` for async/await

```
const stream = require('stream');
const gzip = require('zlib').creaeGzip();
const fs = require('fs');
const pipeline = require('util').promisify(stream.pipeline);

async function compressIt(filepath) {
  try {
    await pipeline(
      fs.createReadStream(FILE_PATH),
      gzip,
      fs.createWriteStream(`${FILE_PATH}.gz`),
    );
    console.log('Pipeline Passed.');
  } catch(err) {
    console.error('Pipeline Failed: ', err);
  }
}
```

* The `.pipe(..)` funcion sets up required backpressure closures for event triggers. Input stream is `Readable` & output is `Writable`, either could be replaced by `Duplex` or `Transform` streams.

* The `backpressure` kicks in when data buffer exceeding `highWaterMark`, writable stream's `.write(..)` returns `false`. Pausing incoming to readable stream, on empty data buffer a `drain` event get emitted to resume data flow.

> Node.js allows to set custom `highWaterMark`, default is 16kb. Should be increased with caution & only when nothing else works.
>
> Guidelines to implement [Readable Stream](https://nodejs.org/docs/latest/api/stream.html#stream_implementing_a_readable_stream) & [Writable Stream](https://nodejs.org/docs/latest/api/stream.html#stream_implementing_a_writable_stream) shall be followed. A [blog post](https://r.va.gg/2014/06/why-i-dont-use-nodes-core-stream-module.html) on why & why not.

* For chaining of streams, intermediate would be Transform stream.

---

