
## Chapter.1 Basics of ML

### Regression & Classification

> for Supervise Learning

* Regression is way of forecasting for continuous variables. Classification predicts for events that have few distinct outcomes.

* Linear Regression to forecast continuous variables, Logistic Regression to predict discrete variables. Severalother techniques usable for both.

* In Regression, need to account for generalization over overfitting. Lower the generalization, higher the error rate on unaccounted data.

* Cross validation on different datasets for training, testing & validation (60%, 20%, 20% data split) provides a check on accuracy of model when hyper-parameters are changed.

#### Measures of Accuracy

* Error is measured on testing dataset, as doing so on training dataset would be ambiguous.

> * `Absolute Error` & `RMSE` for Continuous variables. `Confusion Matrix` for Classification, Discrete event prediction.
> * Discrete events happen as probability of an event happening.

* `Absolute Error` is absolute value difference of `abs(forcast - actual)` values. Abs-value help get real overall notion, avoiding mess of +ve & -ve diff-values.

* `RMSE` (root mean square error), where getting overall squared (square making all diff values +ve) and calculating sq-root of mean for overall.

* `Confusion Matrix` counts number of instances when model predict outcome of an event & measures against actual outcome as TruePositive, TrueNegative, FalsePositive, FalseNegative (TP, TN, FP, FN).

> Performance Metrics of Confusion Matrix are:
> * Accuracy: Correct Classification Ratio; as ((TP+TN) / (TP+TN+FP+FN))
> * Precision: Correct Selection on Total Selection; as (TP / (TP+FP))
> * Recall: Correct Selection on Total Required Ratio; as (TP / (TP+FN))
> * F1: F-measure as equlibrium between Precision & Recall; as (2(TP) / (2TP+FP+FN))
> * Specificity: True Negative rate; as (TN / (FP+TN))

#### AUC Value & ROC Curve

* AUC (Area Under the Curve): Score input for probability of Positive Classification. Order these in descending.

* ROC (Receiver Operation Characterisitc) Curve is cumulative number of points considered from descending list that suit TP captured to max.


### Unsupervised Learning

* 2 major techniques: Clustering (row grouping) and PCA (Principle Components Analysis, column grouping).

> * Clustering helps segmenting, can be powerful pre-processing steps in model building.
> * PCA reduces dimensionality/variables of data, thus can speed up model building process.


### Approach to building Models

* Data Source? Filter/fetch required data.

* Data Preparation, to manage following

> * Missing values in data. Simplest is using average/median of column. Better to use K-nearest neighbors method.
> 
> * Outliers in data. Result in inefficient skews. Typical solution is capping variables at certain % (say 95%).
> 
> * Variable Transformation. Scaling variable based on gradient descent generally cause faster optimization. Log/squared transformations are used when input variable share a non-linear relation with dependent variables.

* Feature Interaction: process of creating new variables based on combination of variables; need better domain context.

* Feature Generation: process to get additional features from dataset; need better behavioral linking of data points.

* Building Models: Once the data is ready and prepared.

* Production Ready: Depends on workflow for use of model.

* Continuous Integration: with A/B testing for comparative analysis.

---
