
## Chapter.3 Logistic Regression

> Where LR is used it's for Linear Regression, we'll use full form for Logistic Regression.


### Discrete Outcomes not suited for Linear Regression (LR)

* Outcomes like if it'll rain or not, a match would be won or not are True/False (1/0) not continuous values. LR would result in fractional values, even negative.

> Logistic Regression helps with such limited Distinct classes.

* LR can't contain exponential relation only linear. As a person with 1year experience may finish a task in 3months, 5yr experience in 2months and 9year experience in just 1 week.

* LR ignore failure chance. E.g. a low ranking player might defeat very high ranking player in an off-chance. LR doesn't predict probability after a certain range as result is capped.

* LR assumes probability increases in proportion to Independent var.


### A more general solution: Sigmoid Curve

* Sigmoid Cruve varies between 0 to 1. And plateaus after a threshold.

* Sigmoid Activation: Sigmoid curve as formula `S(t) = 1 / (1 + e^(-t))`. Here, more value of `t`, lower the value of `e^(-t)`.. keeping `S(t)` close to 1. And lower the value of `t`, keeping `S(t)` closer to 0.

* Logistic Regression math model as `Y = 1 / (1 + e^(-L))` where `L = (b + w * X)`. Passing LR through Sigmoid Activation.

> * In LR, when X increases by 1unit, Y increases by w units.
> * In Logistic Regression (as for below calculation), Y changes more when X changes from 0 to 1 than when changes from -1 to 0. Change is curved.

```
bias = 2, weight = 3
X = 0; => Y = 1/(1+e^-(2+3*0)) = 0.88
X = 1; => Y = 1/(1+e^-(2+3*1)) = 1/(1+e^-5) = 0.99
X = -1; => Y = 1/(1+e^-(2+3*-1)) = 1/(1+e^1) = 0.27
```

* Error estimation for Logistic Regression using `Cross Entropy`, measure of diff between actual & predicted distribution. Formula `-( y.logBase2 p + (1-y).logBase2 (1-p))`, y: actual outcome, p: predicted outcome.

> E.g. PartyA won in Election; Scenario:1 gives PartyA & PartyB 50%-50% chance to win. Scenario:2 give 80%-20% chance. Should have lower Cross Entropy for Scenario:2 as shown below.
> * Scenario:1 Actual=1 & Predicted=0.5 (50%). So error as `-(1.logBase2 0.5 + (1-1).logBase2(1-0.5)) = 1` for Cross Entropy.
> * Scenario:1 Actual=1 & Predicted=0.8 (80%). So error as `-(1.logBase2 0.8 + (1-1).logBase2(1-0.8)) = 0.32` for Cross Entropy.

* Cross Entropy penalizes higher error much more than using Least Squares (sqaured diff value) error estimation. Also covers minimal error ratio more finely.


### Running a Logistic Regression

> Using `logit` method for Logistic Regression.

```
import pandas as pd
import statsmodels.formula.api as smf

data = pd.read_csv('logistic-iris-dataset.csv')

estimate = smf.logit(formular='Setosa!Slength+Swidth+Plength+Pwidth', data=data)
est_fit = estimate.fit()
print(est_fit.summary())
```


### Identifying Measure of Interest

> E.g. building a model to predict a fraud transaction. Say 1% of total transactions are fraudlent.

* Predicting always Zero, would give 99% accuracy. Real-life models would flag high-probability fraud records & send for manual review. So only 1K out of 100K records should be fraud.

> Creating a simple example to come up with error measure..

* Create a table for TransactionIds, Actual Fraud (1/0), Fraud Probability. Sort it by Probability field value in decreasing order. Calculate *Cumulative number of Transactions Reviewed* & *Cumulative Frauds Captured* on sorted table.

```
Id   Actual  Fraud        Cumulative     Cumulative        Cumulative by Random
     Fraud   Probability  Transactions   Frauds Captured   Fraud Capt.   Guess
-------------------------------------------------------------------------------
 5   0       0.84         1              0                 0.5
 2   0       0.7          2              0                 1
 1   1       0.56         3              1                 1.5
 4   1       0.55         4              2                 2
 3   1       0.39         5              3                 2.5
```

> In this case Random Guess might match Model prediction, or go better sometimes. But, with large subset Model performs with more consistency.

* AUC (Area Under the Curve) metric is a better metric to evaluate performance of Logistic Regression model

* In practice, output of rare event modeling has 5 column table scoring dataset into Ten Buckets based pn probability. After each transaction is rank ordered by probability, grouped into bucket based on its decile.

> * `prediction_rank` representing decile of probability
> * `prediction avg_default` average probability of default obtained by model
> * `total_observations` to be equal count as `overall count of records / 10`
> * `Actual avg_default` to be average actual of default
> * `sum` representing actual default captured in each bucket; this should increase as decile so with `prediction_rank`


### Common Pitfalls

* Model should be able to predict with decent time gap for actual event; sutiable to any real-life action that need to be performed.

* Better to cap outliers.

---
