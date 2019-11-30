
## p5.js overview

* goal is to make coding easy for artists, using metaphor of software sketchbook

* has a full set of drawing functionality not limited to just canvas but full webpage

* also has [addon libraries](https://p5js.org/libraries/) for HTML5 objects including text, input, video, webcam and sound


### Hello World

* 2 main functions used; `setup()` block runs once typically for initialization and `draw()` block that runs repeatedly typically for animation

```hello-world.js
let x = 0;

function setup() {
  background(100);
}

function draw() {
  ellipse(x, height/2, 20, 20);
  x += 1;
}
```

* a full set of 2D drawing methods to create and style graphics/text/images

* to load p5 onto page, at least one of `setup()` or `draw()` is needed to be defined

* you may use `instance mode` to have more control over sketch


### createCanvas

* `let kanvas = createCanvas(width, height)` creates new Canvas element and append it to DOM

* can use `kanvas.parent(SomeElement)` to specify the HTML container for it

* p5.js API provides several graphics functionality, though some native HTML5 Canvas functionality isn't exposed by p5 which need to be accessed via variable `drawingContext` as below

```
function setup() {
  drawingContext.shadowOffsetX = 5;
  drawingContext.shadowOffsetY = -5;
  drawingContext.shadowBlur = 10;
  drawingContext.shadowColor = "black";
  // other features
}
```


### Mouse and Touch interaction

| mouse            | touch          |
|------------------|---------------:|
| mouseX           | touchX         |
| mouseY           | touchY         |
|                  | touches[]      |
| mouseIsPressed   |              
|                  |                |
| mousePressed()   | touchStarted() |
| mouseMoved()     |                |
| mouseDragged()   | touchMoved()   |
| mouseReleased()  | touchEnded()   |
| mouseClicked()   |                |
| mouseScrolled()  |                |

* many browsers have default behaviors for this, which can be overridden as `function touchMoved() { return false; }`


### Asynchronous calls and file loading

* Callbacks

> all of load functions accept a callback function as optional last argument, example below

```
function setup() {
  createCanvas(640, 480);
  loadImage('tree.jpg', drawImage);
}

function drawImage(img) {
  image(img, 5, 5);
}
```

* Preload

> * if `preload()` exists it runs before `setup()`
>
> * it should have only load calls whereas all initialization shall happen in `setup()`
>
> * here load calls require no callbacks

```
let img;

function preload() {
  img = loadImage('leaf.png');
}

function setup() {
  createCanvas(640, 480);
  image(img, 5, 5);
}
```

* Loading Screen

> to display while loading large files, include an element with id `p5_loading`, p5.js takes care of rest by convention

```
<div id="p5_loading" class="loadingclass"><img src="any-graphic-animation-for-loading.gif" alt="loading..."/></div>
```


### Browser functions and native JS

* few variables and functions making browser interactionin addition to [native JS functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript)

> * [windowWidth](http://p5js.org/reference/#p5/windowWidth), [windowHeight](http://p5js.org/reference/#p5/windowHeight), [displayWidth](http://p5js.org/reference/#p5/displayWidth), [displayHeight](http://p5js.org/reference/#p5/displayHeight)
>
> * [winMouseX](http://p5js.org/reference/#p5/winMouseX), [winMouseY](http://p5js.org/reference/#p5/winMouseY)
>
> * [fullscreen()](http://p5js.org/reference/#p5/fullscreen)


### Instance Mode / Namesoace

* by default all p5.js functions are in global namespace (bound to window object); but this might cause conflict when used with other JS code

* `instance mode` and `on-demand global mode` are two ways to deal with conflicts

* example of instance mode for `hello-world.js` code from above is below

```
const s = (pX) => {
  let x = 0;

  pX.setup = function() {
    pX.background(100);
  }

  pX.draw = function() {
    pX.ellipse(x, pX.height/2, 20, 20);
    x += 1;
  }
};

let myp5 = new p5(s); // also allowed 'new p5(s, "someContainer")' to append to an element
```

* for `on-demand global mode` can call `new p5()` anywhere without arguments


### Libraries

* p5.dom lib helps interact with HTML5 objects, including text, hyperlink, image, input, video, audio and webcam

* p5.sound lib gives interface to HTML5 web-audio API


### Editor

* official playground is [p5.js web editor](https://editor.p5js.org/)

---

### FAQ

* p5 is reimagining Processing's goal in native JS

---
