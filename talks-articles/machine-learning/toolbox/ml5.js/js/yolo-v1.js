/* reference/image/YOLO Object Detection v1
 *
 * */

let video;
let yolo;
let status;
let objects = [];

let canvasWidth = 640;
let canvasHeight = 480;

function setup() {
  frameRate(1); // per sec trigger of draw
  createCanvas(canvasWidth, canvasHeight);

  video = createCapture(VIDEO);
  video.size(canvasWidth, canvasHeight);

  yolo = ml5.YOLO(video, triggerDetect);

  video.hide();
  status = select("#status");
}

function triggerDetect() {
  console.log("+ detecting...")
  yolo.detect(function(err, results){
    if (err) {
      console.error(err);
      return;
    }
    objects = results;
    triggerDetect();
  });
}

function draw() {
  let width = canvasWidth - 10;
  let height = canvasHeight - 10;
  image(video, 5, 5, width, height);

  if (!yolo.modelReady) {
    console.log('model not loaded yet');
    return;
  }

  for (let i = 0; i < objects.length; i++) {
    let an_object = objects[i];
    let label = an_object.label + "(" + an_object.confidence + ")";

    status.html(label);

    noStroke();
    fill(0, 0, 255);
    text(label, an_object.x * width, an_object.y * height);
    noFill();
    strokeWeight(4);
    stroke(0, 0, 255);
    rect(an_object.x * width, an_object.y * height, an_object.w * width, an_object.h * height);
  }
}
