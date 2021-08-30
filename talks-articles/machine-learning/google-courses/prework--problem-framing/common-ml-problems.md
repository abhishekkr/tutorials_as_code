
## Common ML Problems

> tackling Supervised & Unsupervised Learning

### Supervised Learning

> for e.g. dataset below,
>
> * distance to a place & time taken to reach are `features` (named as X1, X2,.. for graphs); could be measurements or descriptions
>
> * `label` is the answer to those features (which could be, generally are, more in numbers)

```
  distance (kms)    |    time (min)     |      locality
--------------------+-------------------+---------------------
  10                |   30              |   city
  15                |   45              |   city
  14                |   60              |   village
  22                |   110             |   village
  25                |   75              |   city
```

* for supervised learning one feeds features & corresponding labels into an algorithm to train a model determining realtionship between those features

* [stanford research](https://news.stanford.edu/2017/01/25/artificial-intelligence-used-identify-skin-cancer/) used supervised learning to identify risks of skin cancer on visual diagnosis


### Unsupervised Learning

* has to identify said relationship of a model in data without labels; so need to infer it's own rules

* sometimes might even learn unintended patterns from data, as stereotypes or bias

* it tries find patterns for existing data; then for new arriving data tries to check which pattern it fits into more

> e.g. clustering is a type used where clusters based on data-set co-realtion are identified


### Reinforcement Learning

* a model (agent in RL) is set-up with a certain objective to achieve; agent receives a reward function on getting closer to objective

* lack of training data is countered here; requires amazing reward function

* less stable & predictable than Supervised approach


### Example Types of ML Problems

* `Classification` (Pick one of N labels): is it traffic light or lamppost

* `Regression` (Predict numeral values): click-through rate

* `Clustering` (Group similar examples): Most relevant documents (unsupervised)

* `Associating Rule Learning` (Infer likely associations in data): you might also like

* `Structured Output` (Create complex output): NLP trees, Image recognition, Bounding boxes

* `Ranking` (Identify position on scale/status): Search result ranking

---
