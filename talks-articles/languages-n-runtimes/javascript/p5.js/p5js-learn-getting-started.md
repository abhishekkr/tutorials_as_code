
## Get Started

* download [p5.js libs here](https://p5js.org/download/)

* `p5.js` provides bunch of built-in commands and some conventions to be utilized and easily create something beautiful with clear constructs

* with `p5.min.js` following sample program will perform as described in comments

```
function setup() { // runs once at start
  createCanvas(640, 480); // creates canvas with 640px wide and 480px high
}

function draw() { // runs on repeat decided by frameRate property
  if (mouseIsPressed) { // based on mouse click state, at every draw call
    fill(0);            // fills black
  } else {
    fill(255);          // fills white
  }
  ellipse(mouseX, mouseY, 75, 75); // at mouse pointer of radius 75pixels
}
```

* see varied uses in [examples](https://p5js.org/examples/)

* get details on constructs in [reference](https://p5js.org/reference)

---
