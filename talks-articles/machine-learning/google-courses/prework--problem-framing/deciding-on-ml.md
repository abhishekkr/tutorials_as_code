
## Deciding on ML

* start with an elevator pitch for what your ML model need to decide on

> e.g. `to predict if a movie will be upvoted by user or not`


### Ideal Outcome

* the outcome (may be different from model) is what product actually cares about

> e.g. `financial planning` by identifying getting which movies on the product app will please most consumers, so licensing rights budget gets used productively
>
> or just `user experience` by suggesting movies which user have more chance to like and haven't seen yet


### Success and Failure Metrics

* Quantify it. Success/failure is phrased independent of evaluation metrics (as precision, recall, [AUC](https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc#AUC)).

> It's to be in outcomes, so mediocre models are not launched.
>
> e.g. `financial planning` has success metric on how much viewership have bought licenses for movies amount to, if the profits are nearing at least as before
>
> or in `user experience` if user is viewing movies after less effort (scroll, hops) to pick than before or not


* Measurable metrics allow real world evaluation information. To fail fast, check hypothesis, revise and repeat.

> consider how metrics will be measured; when it will be measured; in what duration would success/failure of new ML system could be ascertained


* Other failure scenarions like some bias or divergence due to spam data might impact the outcome.


### ML Model Outputs

> * `Classification`, pick of N labels
>
> * `Regression`, predict numerical values
>
> * `Clustering`, group similar examples
>
> * `Associating Rule Learning`, infer likely assoc patterns in data
>
> * `Structured Output`, create problem domain specific complex output

* Output must be quantifiable with clear definition. If there is not a clear indicatior of result, `proxy label` will have to be used where a different outcome might hint to success/failure of desired outcome.

> e.g. say for `user experience` if users aren't upvoting the watched movie, but user retention has improved

#### Using the Output

* Model can predict either in real-time responding to inputs OR as a batch and cached decision

* Plan implementation as PseudoCode for clarity; where it will reside in Project and what latency is required for decisions.

#### Heuristics

* think of formulating a heuristic first; a good to have for comparing efficacy

* might be better to use heuristic's result if ML doesn't evolve to be better


---
