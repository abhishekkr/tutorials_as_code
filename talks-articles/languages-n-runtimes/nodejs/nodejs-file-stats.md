
## Node.js Manipulating Files

> [source](https://nodejs.dev/en/learn/)

### Node.js File Stats

```
const fs = require('fs');

fs.stat('/tmp', (err, stats) => {
  if (err) {
    console.error(err);
    return null;
  }
  stats.isFile() ? console.log("is a file") : console.log("not a file");
  stats.isSymbolicLink() ? console.log("is a symlink") : console.log("not a symlink");
  stats.isDirectory() ? console.log("is a dir") : console.log("not a dir");
  console.log(stats.size);
});


const fsPromises = require('fs/promises');

async function asyncStat() {
  try {
    const stats = await fsPromises.stat('/tmp');
    stats.isDirectory() ? console.log("is a dir") : console.log("not a dir");
    console.log(stats.size);
  } catch (err) {
    console.log(err);
  }
}
asyncStat();
```

*gives output like*

```
not a file
not a symlink
is a dir
920
is a dir
920
```

* `path` module

```
const path = require('path');
const val = '/tmp/doc/of/node.js';
console.log(`Parent dir: ${path.dirname(val)}, Filename: ${path.basename(val)}, Extension: ${path.extname(val)}`);
console.log(`Just Filename: ${path.basename(val, path.extname(val))}`);
console.log(path.join('/', 'tmp', 'blah.txt'));
console.log(path.resolve('tmp', 'blah.txt'));
console.log(path.resolve('/tmp', 'blah.txt'));
console.log(path.normalize('/tmp/abc/../blah.txt'));
```

*gives output like*

```
Parent dir: /tmp/doc/of, Filename: node.js, Extension: .js
Just Filename: node
/tmp/blah.txt
/code/nodejs/tmp/blah.txt
/tmp/blah.txt
/tmp/blah.txt
```


### Working with File Descriptors, Reading/Writing Files

* following code to open file & op; `r` for reading, `r+` for read/write (existing file), `w+` for read/write wiht stream at file beginning, `a` for append, `a+` for read/write with stream at file end

```
const fs = require('fs');

fs.open('/tmp/blah.txt', 'r', (err, fd) => {
  // fd as file descriptor
});
```

* reading

```
const fs = require('fs/promises');
const fsPromises = require('fs/promises');

fs.readFile('/tmp/blah.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});

async function readblah() {
  const filehandle = await fsPromises.open('/tmp/blah.txt', 'r');
  console.log(filehandle);
  console.log(await filehandle.readFile({encoding: 'utf-8'}));
  await filehandle.close();
}
readblah();
```

* similarly there is `fs.writeFile(path, content);` available via `fs` & `fs/promises`; & `appendFile(path, content)` as well

* also `readFileSync` & `writeFileSync` for synchronous versions out of the bag


### Working with Folders

```
const fs = require('fs');
const path = require('path');

const dirPath = "/tmp";
const todayDir = "/tmp/todays";
const yesterdayDir = '/tmp/yesterday';
const fileName = path.join(dirPath, "blah.txt");

fs.mkdirSync(todayDir);

// filter dir content listing
const isFile = (val) => fs.lstatSync(val).isFile();
fs.readdirSync(dirPath)
  .map(fileName => { return path.join(dirPath, fileName); })
  .filter(isFile);

// rename dir
fs.rename(todayDir, yesterdayDir, err => {
  if (err) {
    console.error(err);
  }
});

// write file
fs.writeFileSync(fileName, 'lorem');
console.log(fs.readFileSync(fileName).toString());

// remove a file
fs.rm(fileName, err => {
  if (err) {
    console.log('faile to delete file');
  }
});

// remove a dir
fs.rmdir(yesterdayDir, err => {
  if (err) {
    console.log('faile to delete dir');
  }
});
```

---

