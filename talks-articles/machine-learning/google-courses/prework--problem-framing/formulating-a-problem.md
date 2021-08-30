
## Formulating a Problem

### Articulate Your Problem

* several subtypes of classification & regression

#### Classification Subtypes

* `binary classification` if 2 categories

* `multi-class classification` for 2plus categories

> `multi-class single-label` if need one category from a single example
>
> `multi-class multi-label` if need one-plus category from a single example

#### Regression Flow Chart

* `unidimensional regression` if need 1 output

* `multidimensional regression` if need one-plus outputs

* Other problem framed as `Clustering (unsupervised)` & `Other (translation, parsing, bounding box id, etc)`

> e.g. Problem is framing as 5 class, single-label predicting if a video will be `{upvoted, downvoted, watched-full, watched-partial, skipped}` after 30days of being available


### Start Simple

* state given problem as classification & regression; for the task use simplest possible model

> * e.g. binary classification: predict if the movie will be liked or not
>
> * e.g. regression: we'll predict a new movie's likeability in terms of it being watched/liked or skipped/partial-watch/disliked within month of being added

* first focus on leveraging data; further tuning the model will bring wins with benefit of evolutionary experience


### Identify Data Sources

* identify: how much labeled data is available; what's source of labels; are labels connected to decision required


### Design Data for Model

* each row constitutes one piece of data for one prediction

* only use info available while making prediction; each input can be scalar or 1-D int/floats/bytes(incl. strings) list

> * if input ain't scalar or 1D list, consider representation of that field
>
> * if a cell represents `>=2` semantic things in a 1D list, split them
>
> * if a cell represents nested `protobuf`, may flatten it out
>
> * exceptions for media data, cell is a blob of bytes; metadata might be used for these


### Determine where Data comes from

* assess data pipeline creation task

* latency and availability of input; make sure all input are available at prediction.. better to omit irregular inputs if can


### Determine easily obtained inputs

* pick 1-3 inputs which are easy to obtain with promise of reasonable outcome

> better if available from a single system with a simple pipeline


### Ability to Learn

* list aspects of problem that might cause difficulty to learn

> like not enough +ve labels or examples; noisy labels; difficulty generalizing new cases


### Think About Potential Bias

* many datasets are biased in a way, which may not translate across multiple contexts

---
