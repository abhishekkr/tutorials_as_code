
## Reference:Text: Sentiment Analysis

> [source](https://learn.ml5js.org/docs/#/reference/sentiment)
>
> [sample code](reference-text-sentiment.html) with main [js here](js/sentiment-v1.js)

* default model used is `movieReview`, trained on IMDB reviews truncated to 200 words and only 20K most common words used (not good results for negative at least for my test input)

* prediction score would range from 0 to 1, ranging from extreme hint of negative to positive

* initializing using `const sentiment = ml5.sentiment(modelName, ?callback)`

> * **modelName**: required, defaults to `movieReview`; can also use path to `manifest.json` file
>
> **callback**: optional, called when model has loaded; returns a promise resolved when model loaded

* attributes on sentiment object are `.ready` and `.model`

* methods on sentiment object is `.predict(text_to_predict)` returns score with 0 as negative and 1 as positive


---
