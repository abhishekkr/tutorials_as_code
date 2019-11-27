/* Sentiment Analysis
 */

let sentiment;
let status;
let submitButton;
let inputBox;
let sentimentResult;

function setup() {
  noCanvas();

  sentiment = ml5.sentiment('movieReviews', modelReady);
  console.log('loading model');

  status = select('#status');

  inputBox = createInput('it is a happy moment in life');
  inputBox.size('size', '25');
  status.child(inputBox);

  submitButton = createButton('check sentiment');
  status.child(submitButton);
  submitButton.mousePressed(checkSentiment);

  sentimentResult = createP('getting ready to use...');
  status.child(sentimentResult);
}

function checkSentiment() {
  const txt = inputBox.value();
  const prediction = sentiment.predict(txt);
  sentimentResult.html('sentiment score: ' + prediction.score);
  console.log(prediction);
}

function modelReady() {
  console.log('model loaded');
  sentimentResult.html('ready to try, click "check sentiment" button');
}
