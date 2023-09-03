
## Chapter.2 Linear Regression

### Intro

* A sequence of continuous variables with statistical relationship. LR looks at history to estimate new unknown.

* Independent variable are used to predict Dependent variable. E.g. Variables on supply might infer demand. Correlation could be +ve or -ve. Every correlation & reverse correlation isn't always Causation.


### Simple vs Multivariate

* Dependent variable can be influenced by a multitude of Independent variables. In Multivariate LR, there is a mix of +ve & -ve correlation.


### Formalizing Simple LR

* Simple LR can be represented as `Y = b + w * X`. With `X` as Independent var, `w` as slope of var (weight assigned to Independent var), `b` as bias term & `Y` as Dependent var.

* Bias Term: Value of Dependent var when all Independent var are `0` nil.

* Weight/Slope: Difference between `Y` & `X` coords at both extremes of line (when graphed `y|_x`) upon line length. As in `w = (Diff between Y coords at max & min) / (Diff between X coords at max & min)`


### Solving a Simple LR

* Assumed dataset of `Age in Months` to `Wight in Kg`, with fixed 0.75kg increment.

```
Months:    0    1     2     3     4  5     6    7     8  9
Kilogram:  3    3.75  4.5   5.25  6  6.75  7.5  8.25  9  9.75
```

> * Let's assume X-axis (Independent) as Age and Y-axis (Dependent) as Baby's Weight.

* Picking just first 2 datapoints (0, 3) & (1, 3.75)

```
3 = b + w * (0)    ==> b = 3 - 0
3.75 = b + w * (1) ==> b = 3.75 - w
~~> b = 3
~~> w = 3.75 - b = 0.75
```

> let's apply calculated bias & slope/weights to calculate Overall Squared Error

```
Months:    0    1    |     2     3     4  5     6    7     8  9
Kilogram:  3    3.75 |var: 4.5   5.25  6  6.75  7.5  8.25  9  9.75
   Squared error estimate: 0     0     0  0     0    0     0  0
   Overall Squared Error: 0
```


### More General Ways of Solving

* LR is process of solving for values of Bias & Weight so it minimizes Overall Squared Error across all data points.

> * `Overall Squared Error` is sum of squared difference between actual & prediction of all observations.
> * Minimizing it infers correct predictions. Overprediction by 5% is as bad as Underprediction by 5%.

* Process is simply iterating over multiple combinations of Bias & Weight seeking most minimization of error.

> * Final combination of optimal value is obtained by `Gradient Descent`.


### Working Details of Simple LR

* Solving for Bias & Weight is like `goal seek` problem in Excel.

* Using a more realistic data with overlaps & irregularities would bring about different X-Y graph. Solving for it would require similar flow

> * Init Bias & Weight with arbitrary values (e.g. 1 for each).
> * Make a new column for Forcast with value of `b + w * X`
> * Make a new column for Squared Error, Calculate `Overall Squared Error` from it.
> * Invoke Solver to minimize `Overall Squared Error` value by tweaking Bias & Weights.

* Simple flow of Gradient Descent for optimization of values is

> * Init value of Bias & Weight coefficients, randomly.
> * Calculate Cost func i.e. for `Overall Squared Error`.
> * Change value of coefficients slighlty; e.g. +1% of values.
> * Check if Cost increased (reduce coefficients by 1%) or decreased (again increase by 1%).
> * repeat steps 2-4 (or N) times until Cost is least.

* RMSE is calculated as `sqrt( cost / item_count )`.


### Running a Simple LR

* Residual error is diff between actual & forecast value. Residual Deviance is expected deviance from built model. Should be compared with Null Deviance.

* Null Deviance is expected deviance when no independent variables are used in building model. Best guess then is average of Dependent Variables itself.

```
import pandas as pd
import statsmodels.formula.api as smf

data = pd.read_csv('simple-dataset.csv')

estimate = smf.ols(formular='Weight~Age', data=data)
est_fit = estimate.fit()
print(est_fit.summary())
```


### Common Pitfalls of Simple LR

* When X & Y are not linearly related. E.g. Weight correlation in adults.

* Outlier among Independent variables. E.g. A highly overweight baby skews predictions for all, so skip it for Bias & Weight calculation. Can normalize outliers to 99th %ile value and add a flag that value was normalized.


### Multivariate LR

* Involves multiple Independent variables. Can be translated to math model of `Y = b + (w1 * X1) + (w2 * X2) + ..`. Weights could be negative as well.

* In Python, for sample of Icecream Sales number dependent on Weather, Non-work Days, Price.

```
import pandas as pd
import statsmodels.formula.api as smf

data = pd.read_csv('multivariate-dataset.csv')

estimate = smf.ols(formular='Sale~Temperature+OffdayFlag-Price', data=data)
est_fit = estimate.fit()
print(est_fit.summary())
```

* Issue: A `non-significant variable` has high **p-value**. High `p-value` is when standard error is high compared to coefficient value; due to high variance within multiple coefficients. Thus results in RMSE increase.

* Issue: `Multicollinearity` occur when Independent variables are related. E.g. If Weekends have price discount for Icecream. Non-work days & Price are collinear. The cumulative effect of these variables separately on Dependent may get tweaked when looked together.

> E.g. With _KidsInSchool_ as Dependent on _NetIncome_, _LiteracyRate_ & _Singles_. Here LiteracyRate increment might increase Singles, have correlation. KidsInSchool might increase X-units with more AL-unit LiteracyRate & decrease by Y-units with more AS-unit Singles. With resulting to just net effect of just (X-Y)-unit at AL-unit LiteracyRate.

* With correlated variables, creating a _Correlated_Literacy_To_Singles_ variables for use would make _LiteracyRate_ & _Singles_ as `non-significant variables` & be used instead.

* Inadvisable for a Regression to have very high coefficients, in general. If a unit change in one variables impact a vary high unit change in another; advisable to use `log(value)` or normalize values or penalize the model for having high magnitude of weights using `L1/L2 Regulraizations`. Keeping Bias & Weights small.

* Regression must be built on considerable observations. Having at least 100x data points to count of Independent variables is advisable.

> * `Adjusted R Squared` considers high count of Independent var & penalize it.
> * `R^2 = 1 - [(1-R^2)(n-1)/(n-k-1)]`. Here, n: data points count, k: independent var count. Model with least `Adjusted R Squared` are generally better.


### Assumptions of LR

* Independent var are linearly related to Dependent. If level of linearity changes, linear model is built per segment.

* No outliers in Independent var values. Otliers to be normalized & flagged.

* Error values should be independent. LR with errors all on same side or following a pattern.

* `Homoscedasticity`, errors shall not get large with value of Independent Variables. Graph distribution should cylidrical than a cone.

* Errors should be normally distributed, with only few extreme cases.

---
