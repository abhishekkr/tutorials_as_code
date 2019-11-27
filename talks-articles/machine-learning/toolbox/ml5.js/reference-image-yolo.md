
## Reference:Image: YOLO

> [source](https://learn.ml5js.org/docs/#/reference/yolo)
>
> [sample code](reference-image-yolo.html) with main [js here](js/yolo-v1.js)

* YOLO (You Only Look Once) is a real-time object detection system

* single NN is applied to full image, it divides image into regions and predict bounding boxes with probabilities for each region

* this is inspired from [ModelDepot/tfjs-yolo-tiny](https://github.com/ModelDepot/tfjs-yolo-tiny)

* initializing using `const yolo = ml5.YOLO();` or `ml5.YOLO(video)` or `ml5.YOLO(video, ?options, ?callback)` or `ml5.YOLO(?options, ?callback)`

> * **video**: optional, an HTML video element or p5 video element
>
> * **options**: optional, object describing modele accuracy and performance; for `MobileNet` these are `{ filterBoxesThreshold: 0.01, IOUThreshold: 0.4, classProbThreshold: 0.4 }`
>
> **callback**: optional, function to run on model load; returns promise if no callback provided which gets resolved as soon model gets loaded

* YOLO object has attributes `.isPredicting` and `.modelReady`

* Methods available are `.detect()`

---
