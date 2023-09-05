
## Chapter.6 Gradient Boosting Machine

> GBM (unlike Random Forest's parallelization over trees) uses a Sequential approach. Each Decision Tree predicts error of Previous Decision Tree, boosting (improvng) the error (gradient).


### GBM

* Gradient: Error or Residual. Boosting: Improving. GBM reduces error.

> E.g.: Model `M` (Decision Tree) with 80% accuracy as `Y = M(x) + error`.
> * Here `error = G(x) + error2`, as it's to be predicted from previous Decision Tree's error.
> `G(x)` is another Decision Tree trying to predict error using `x` Independent var.
> * And `error2 = H(x) + error3`.
> * So, combining 3 Trees we get `Y = M(x) + G(x) + H(x) + error3` and accuracy should be higher than 80%.


### Working Details of GBM

* Algorithm

> * Init simple Decision Tree. Calculate Residual (actual-prediction diff) value.
> * Build another shallow Decision Tree to predict Residual using Independent var.
> * Update original prediction with `new prediction x learningRate`.
> Repeat from past Init for `n times`. Number of trees is `n`.

```
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
import numpy as np
from sklearn.metrics import roc_auc_score

data = pd.read_csv('gbm_dataset.csv')
## Preparation
data = data.drop(['Unnamed: 0'], axis=1)    # dropping irrelevant fields
data['Income'] = data['Income'].fillna(value=data['Income'].median())   # managing nulls
data['FamilySize'] = data['FamilySize'].fillna(value=data['FamilySize'].median())   # managing nulls
X = data.drop('TakingLoan', axis=1)     # cleaning Dependent
y = data['TakingLoan']                  # creating Dependent

## Creating Train & Test Datasets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

## Build Decision Tree
depth_tree = DecisionTreeClassifier(criterion='gini', max_depth=4, min_samples_leaf=10)
depth_tree.fit(X_train, y_train)

## Getting prediction on Train Data
dt_predict = depth_tree.predict_proba(X_train)
X_train['prediction'] = dt_predict[:,1]

## Getting prediction on Test Data
dt_test_predict = depth_tree.predict_proba(X_test)
X_test['prediction'] = dt_test_predict[:,1]

## Build 20 Trees of Residuals
depth_tree_2 = DecisionTreeRegressor(criterion='mse', max_depth=4, min_samples_leaf=10)
learning_rate = 1
for idx in range(20):
    train_error_n = y_train - X_train['prediction']     # Calculate Residual
    ## remove prediction variable appended to Independent earlier
    X_train_2 = X_train.drop(['prediction'], axis=1)
    X_test_2 = X_test.drop(['prediction'], axis=1)
    ## build tree to predict Residuals using Independent var
    depth_tree_2_fit = depth_tree_2.fit(X_train_2, train_error_n)

    dt_predict_error_2 = depth_tree_2_fit.predict(X_train_2)    # predict Residual
    ## update prediction based on predicted Residuals
    X_train['prediction'] = (X_train['prediction'] + (dt_predict_error_2 * learning_rate))
    ## Calculate AUC
    train_auc = roc_auc_score(y_train, X_train['prediction'])
    print("AUC on training dataset is: " + str(train_auc))

    dt_test_predict_error_2 = depth_tree_2_fit.predict(X_test_2)    # predict Residual
    ## update predictions based on predicted Residuals for Test Data
    X_test['prediction'] = (X_test['prediction'] + (dt_test_predict_error_2 * learning_rate))
    test_auc = roc_auc_score(y_test, X_test['prediction'])
    print("AUC on testing dataset is: " + str(test_auc))
```


### Shrinkage

* Just like Random Forest, based on accuracy of Decision Tree. To measure impact on changing LearningRate/Shrinkage.. so using code from `Working Details` section above with changing `learning_rate = 0.05`.

> For datset used in book, lowering LearningRate showed AUC increasing consistently.


### AdaBoost (Adaptive Boosting)

* For a Continuous Dependent var, Residual calculation can be Gaussian, could be different for Discrete.

* AdaBoost Algorithm..

> * Build *weak* learner (here a Decision Tree) using few Independent var.
> For 1st learner, associated weight for each observation is same.
> * Identify incorrectly classified observation based on learner.
> * Update weights such as misclassification observation get more weight than correct.
> Assign weight of each weak learner based on prediction accurace. Final prediction is weighted average predicition of multiple weak learners.

#### Working Details of AdaBoost

> Following is a gist, need better understanding.
> E.g.: Say X Continuous Independent and Y Discrete Dependent.

* Sort by X. Take an optimal splitting for X. Calculate ChangedY & Yhat (same as ChangedY for 1st).

> * ChangedY with values of 0 to -1, to give better multiplication output.
> * When ChangedY & Yhat are same, exponential of -ve would give small number. With different values, we'll start getting bigger number and updated weight would be more than original.

* Predict using ChangedY & `0.1` weights. Calculate `overall error`.

* Calculate weight to be associated with 1st Weak Learner. `0.5 * log((1 - OverallError) / OverallError)`.

* Update observation weight, increasing of misclassification. Tuning weights such in new iteration, misclassification are predicted more accurately. Formula `originalWeight * e^(-LearnerWeightage * Yhat * ChangedY)`

> Weight are updated in a way, overall sum of weights is 1. Updating weights make it a Regression. Updated derivedon an earlier formula post normalization.

* Once all predictions are made, final prediction is via summation of weight associated with each weak learner.


### Additional Functionality for GBM

* Like Random Forest, `Row Sampling` & `Column Sampling` for each tree could avoid some overfitting.


### Implementing GBM

```
from sklearn import ensemble
from sklearn.metrics import roc_auc_score

## key parameter 'loss' is whether normal Residual approach or AdaBoost
## can even inject custom loss function
gb_tree = ensemble.GradientBosstingClassifier(
    loss='deviance',
    learning_rate=0.05,
    n_estimators=100,
    min_samples_leaf=10,
    max_depth=13,
    max_features=2,
    subsample=0.7,
    random_state=10,
)

## assumes X_test, y_test have been prepared & available
gb_pred = gb_tree.predict_proba(X_test)
roc_auc_score(y_test, gb_pred[:,1])
```

---
