
## Node.js Command Line

> [source](https://nodejs.dev/en/learn/)


### Run scripts

* `node script.js` for shell to run a script with node; shebang works as well

* `node -e "console.log("at cli")"` to run without script

* `nodemon` module allows restart app automatically on change; install via `npm` & run as `npx nodemon script.js`

### Env Variables

* `VAR_A=123 node script.js` would make env available within via `process.env.VAR_A`

* `.env` file at root could be utilized by `require('dotenv').config();`

### REPL

* just running `node` triggers its REPL, `.help` shows all available; `_` gives last result

* `.editor` enables editor mode for multiline; `.break` to abort further input; `.save` to save session to a file; `.exit` to end

* `const repl = require('repl'); repl.start('$ ');` to trigger repl from a script

### Output to command line

* `console.log(..)`; would also format params as `console.log('%s %d %i %o', 'x', 1.1, 1.1, {a: 'A'});` gives `x 1 1 {a: 'A'}`

* `console.clear()` to clear; `console.count()` prints counter a string is printed next to it; `console.countReset()` reset counter to zero

* `console.trace()` prints trace to reach that position

* `console.error()` prints to stderr; `console.time('x')` start a timer & ends with `console.timeEnd('x')` to use for time taken

* can easily print colored output with escape sequence or `npm` package `chalk`; similarly `progress` package for progress bar


### Accept input from Cli

* [readline](https://nodejs.org/api/readline.html) module gets input from `process.stdin` like, can get/set prompt

```
const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout,
});

readline.question(`Enter a number: `, num => {
  const min = parseInt(num);
  const msg = isNaN(min) ?
                'You were asked to enter a proper number, dumbo.' :
                `Congrats, now you shall relax for ${min} minutes.`;
  console.log(msg);
  readline.close();
});
```

* `readline-sync` package provides simple ways to deal with things like taking hidden input

* `inquirer` package has even more features

---

