
## Hello ml5.js: Intro

> [source](https://learn.ml5js.org/docs/#/tutorials/hello-ml5)

* this example introduces classic application of ML, image classification

* can use [pre-trained model](https://youtu.be/yNkAuWz5lnY?t=33) called [MobileNet](https://github.com/tensorflow/tfjs-models/tree/master/mobilenet)


### Traditional code structure

* `<project-root-dir>/images/bird.png` taken as input to process by trained model

* `<project-root-dir>/index.html` main markup to be rendered for processing, we have this at [hello-ml5-v1.html](hello-ml5-v1.html)

* `<project-root-dir>/sketch.js` with ml5 specific JS constructs


### Some code decisions

* `preload()` in `sketch.js` loads model and data as they could be large, so initiated before running rest of code

* `setup()` is used for anything that just need to run once

* `.classify()` receives data and a callback function, in this case `gotResult()`

* `nf(..)` is a `p5.js` function formatting number to a nicer string

---
