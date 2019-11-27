
## Promises and Callback support in ml5

> error-first callbacks and promises, both are supported


### Using Callbacks

* callback function uses `foo(error, result)` signature where error object (null if no issues) is passed alongwith results and dealt with first

* example used in `imageClassifier()` method

```
// pass a callback function to converter
const classifier = ml5.imageClassifier('MobileNet', function(err, model) {
  if (error) {
    console.log(err); return;
  }
  console.log('Model loaded!');
});

// make a prediction with selected image and pass a callback fn
classifier.predict(img, function(err, results) {
  // check for errors, if no errors do main work
});
```


### Using Promises

* if no callback is provided to async function, then a promise is returned

* example of using image classification with promise as

```
ml5
  .imageClassifier("MobileNet")
  .then(classifier => classifier.predict(image))
  .then(results => {
    // utilize results
  })
```

---
