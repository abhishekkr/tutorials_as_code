
## Getting Started with ML


### The ML Mindset

* be ready for uncertainity, just rely on empirical results of its impact

#### Scientific Method

* Restrict a research goal clearly

> e.g. Predict how much time would it take to drive from Place.A to Place.B

* Make a hypothesis

> e.g. Driving time would depend on general time taken at a time and day; impacted by weather

* Collect the data

> e.g. Collect data at different time on different days

* Test hypothesis

> e.g. Train a model using collected data

* Analyze results

> e.g. Are predictions made meeting new trips made?

* Reach a conclusion

> e.g. The model should (not) be used due to ... reasons.

* Refine hypothesis; repeat

> e.g. good weather for driving to harsh weather weights might help make it finely tuned


### Identifying ML Problem Framing

* if there is no clear algorithm or flow of tasks to achieve a result; irrespective of it being a common sense like problem for human mind OR a guess-timation

* don't focus on data or pattern, before strictly defining the problem to solve

* more data typically improves model; rule of thumb is to have 1000s eg for basic linear models and 100Ks for NNs

* using every feature for model to see what's useful will make it complex, possibly less efficient; get correlation without causation


### Hard ML Problems

* `Clustering` (Unsupervise), criteria might not be intuitive for model. Can check labelling some data and see if they same labelled vectors fall in same cluster.

* `Anomaly Detection` wouldn't need ML if have a good heuristic defined. Can craft high-precision low-recall heuristic to label anomalies and use a semi-supervised approach to train a model, if heuristic is really complicated to evolve.

* `Causation` is harder to identify by ML; generally used for correlations. Research experiments are required to identify causation; even then it might be coincidentally proven due to data set on wrong vector.

* `No Existing Data`, no model training. Start with heuristic rule engine, try collect data.

---
