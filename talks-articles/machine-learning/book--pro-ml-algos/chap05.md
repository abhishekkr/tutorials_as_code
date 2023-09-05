
## Chapter.5 Random Forest

> Random Forest overcomes a Decision Tree's challenges, by building many. Random sampling from dataset to build multiple decision trees, so a Random Forest.

### Example Scenario

> Scenario: Whether user would Like or Dislike a Movie.
> * Prepared 20 Qs.
> * Constraint of 10 random Qs selected for a user. Based on user's response, A comes up with recommendations.
> * Similarly B recommends using 10 random Qs (which may have some overlap, since they are random).
> * The discrete class (like giving 4Stars as answer for some movie to A, and 5Stars when answering B) may have some shifts in answer levels as well.
> * Performing same process with `n recommenders` brings decision trees to Random Forest. Final recommendation being average of all `n`.

* `Bagging`, or Bootstrap Aggregating refers to random sampling subsection of dataset (bootstrap) and then taking average of predictions from all decision trees (aggregating). Less likelihood of bias to few outliers.

* Algorithm: Sample random subsets of original datasets (both rows & columns). Subset Independent variables too. Build Decision Trees, Predict on test dataset. Doing this for n times, then calculating avergae of prediction.


### Implementing

* Errors can happen due to high values in some categorical independent var (as sample size is smaller than original dataset), so convert Dependent var into categorical or factor var. Or assumption of regression instead of classification.

* Parameters to tune are very much similar to Decision Tree, alongwith `n` tree count. Tree Count to be optimized y maximum value of AUC achieved.

```
from sklearn.ensemble import RandomForestClassifier

rfc = RandomForestClassifier(n_estimators=100, max_depth=5, min_samples_leaf=100, random_state=10)
rfc.fit(X_train, y_train)

## making predictions
rfc_pred = rfc.predict_proba(X_test)

## calculating AUC
from sklearn.metrics import roc_auc_score
roc_auc_score(y_test, rfc_pred[:,1])
```

---
