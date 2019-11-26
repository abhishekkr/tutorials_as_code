console.log('ml5 version:', ml5.version);

let classifier; // initializing classifier method, callback needs to be passed

let img; // var for image to classify

function preload() {
  classifier = ml5.imageClassifier('MobileNet');
  //img = loadImage('data/bassmint.png');
  img = loadImage('data/cat-01.png');
}

function setup(){
  createCanvas(640, 480);
  background(180);

  classifier.classify(img, gotResult);
  image(img, 5, 5, 630, 470); // image, x, y, pX, pY
}

// signature for function to handle errors and results from NN
function gotResult(error, results){
  console.log(".....gotResult()")
  if (error) {
    console.error(error);
    return;
  }
  console.log(results);
  createDiv('Label: ' + results[0].label);
  createDiv('Confidence: ' + nf(results[0].confidence, 0, 2));
}
