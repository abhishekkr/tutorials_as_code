## Redemption from Callback Hell (JavaScript)
##### HTML5 DevConf 2013

* No RETURN
* No THROW
> * No STACK
> > * No GUARANTEES

---

#### The Future is ES6 Generators

```
function fibonacci(){
  var i = 0, j = 1;
  while(true){
    yield i; // <- awesome
    var t = i;
    i = j;
    j += t;
  }
}

var generator() = fibonacci();
for(var i = 0; i < 12; i++) {
  print(generator.next());
}
```
* first-class co-routines
* suspended execution context
* small API

* not available yet
> * so in the meantime PROMISES/A+

---

#### PROMISES/A+
[https://promisesaplus.com/](https://promisesaplus.com/)
[https://github.com/promises-aplus](https://github.com/promises-aplus)

* some language initially implemented PROMISES in 1989; inspired by E
* python has futures; twisted defers

main resources by Domenic on Promises
* [You're missing the point of Promises](https://gist.github.com/domenic/3889970)
* [ES6 Promises Spec](https://github.com/domenic/promises-unwrapping)
* [Callback, Promises and Coroutine ~ Async Pattern in JS](http://www.slideshare.net/domenicdenicola/callbacks-promises-and-coroutines-oh-my-the-evolution-of-asynchronicity-in-javascript)

#[A Presentation](https://github.com/briancavalier/refreshpitt-promises-aplus/blob/master/slides.md)

---

A Promise is an Async Value.
There are 4 bassic sync to async transformation.

* first type
```
var user = getUser('jack');
var name = user.name;
//becomes
getUser('jack').then(function(user){
  return user.name;
});
```

* second type
```
getUser('jack', function(error, user){
  //...
});
//becomes
getUser('jack').then(function(user){
  //...
}, function(error){
  //...
});

```

* third type
```
var user = getUser('jack');
if(!user) throw new Error('no user');
var name = user.name;
//becomes
getUser('jack').then(function(user){
  if(!timeline) throw new Error('no user');
  return user.name;
});
```

* fourth type
```
try {
  doSomething(abc, 'jack');
} catch(error) {
  handleError(error);
}
//becomes
doSomething(abc, 'jack').then(undefined, handleError);

try {
  doSomething(abc, 'jack');
} catch(error) {
  handleError(error);
}
//becomes
doSomething(abc, 'jack').then(undefined, function(error){
  throw new Error('ERROR' + error.message);
});
```

---

#### Operate in Sequence

basic flow
```
var user = getUser('jack');
var tweets = getNewTweets(user);
updateTimeline(tweets);

// using callback
getUser('jack', function(user){
  getNewTweets(user, function(tweets){
    updateTimeline(tweets);
  });
});

//using promises
getUser('jack')
  .then(getNewTweets)
  .then(updateTimeline);

```

error-handled flow
```
try {
  var user = getUser('jack');
  var tweets = getNewTweets(user);
  updateTimeline(tweets);
catch(error) {
  handleError(error)
}

// using callback
getUser('jack', function(error, user){
  if(error){
    handleError(error);
  } else {
    getNewTweets(user, function(error, tweets){
      if(error){
        handleError(error);
      } else {
        updateTimeline(tweets, function(error){
          if(error){ handleError(error); }
        );
      }
    });
  }
});

//using promises
getUser('jack')
  .then(getNewTweets)
  .then(updateTimeline)
  .then(undefined, handleError);

```

---

### Dispel the FUD

* its job of a correct library to make it usable
* there are patterns for inter-operability to callback based libraries

* promises are coming to DOM as futures
* generators coming to V8
* promises are future-proof

---

[Promise Based HTTP Lib](http://github.com/machjs/mach)

---

