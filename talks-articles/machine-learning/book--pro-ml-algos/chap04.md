
## Chapter.4 Decision Tree

> Decision Tree forms basis of tree-based algorithms to classify/predict. Are Business User friendly, as can be visualized based on rules.

> E.g. dataset of Continuous var (TaxableIncome) & Categorical var (Refund, Unmarried) to predict if someone is Cheating on their taxes.. can be visualized as following Tree with root node, decision nodes & leaf nodes

```
 [Unmarried]-->[Refund]-->[TaxableInc]--(>80K)--->[Yes]
    |,N           |,Y         '>--------(<=80K)-->[No]
   [No]          [No]
```

### Components

* Root Node: represents entire sample, Splitting into 2/more homogeneous sets.

* Decision Node: A sub-node Splitting further.

* Leaf/Terminal Node: Final nodes.

* Pruning: Removing sub-nodes from Decision Node.

* Branch/Sub-tree: Subsection of an entire tree.


### Classification with Multiple Discrete Independent Variables

* `Information Gain` is used to shortlist Independent var to select first Splitting at Root. It is a measure of Uncertainity after splitting a node. Least uncertainity relates to most Info Gain, and suitable for prior Split.

* Calculating uncertainity as entropy using `-(p.logBase2_p + q.logBase2_q)` with p (probability of event 1) & q (probability of event 2).

* Calculate uncertainity of Dependent var as for Root Node. Check reduction in uncertainity for each Independent variable's Overall Uncertainity. Ordering in most reduction ratio, there is order for Splitting.

* Calculating Left-Right Node: `Gini Impurity`. `Gini Impurity` refers to extent of inequality within a node. If a node has 50%-50% (equal segments) observations, it's most impure. Node with one segment having all observations is purest. Defined as `1 - (p^2 - q^2)` with p & q as probability of each class.

> Steps:
> * Rank order distinct values by % of their observations belonging to certain class of Dependent var.
> * Pick top value as Left Node & rest Distinct as Right node. Left node has 0 impurity.
> * Calculate *Overall Impurity* = `((Left Node Impurity * Observation Count in Left Node) + (~same for right)) / Total Observation Count`.
> * Now slowly move distinct values to Left Node until Right node is purest.
> * Combination with Least Overall Impurity is to be selected as for Left & Right nodes.

* Now, if Left or Right Decision Node has multiple distinct values. They would be considered as well while checking suitable Splitting Var for Info Gain & then followed with Gini Impurity to decide on Left & Right nodes.

* Splitting until all leaf nodes are pure, might Overfit the data & hence not generalizable. As accuracy is high on training data, might not be on validation data. Thus Splitting shall stop at point where validation dataset accuracy stops improving.


### Classification for Continuous Independent Variables

* Assume a dataset where `Survived` is Dependent var on `Age` Independent var which is Continuous.

> * Sort it in increasing order on Independent var. Test multiple rules like testing impurity for when Age is less than 10 or less than 21 upto max age data available. Calculating Gini Impurity for these would give a datapoint with least impurity to be used as rule for Split.


### Classification when Multiple Independent Variables

* First find Splitting Rule for Left/Right Nodes. Then calculate Info Gain by Split, thus shortening var to slit overall dataset.


### Classification when Independent Variables are Continuous & Discrete

* For Continuous variables, calculate optimal Split point. Then calculate Info Gain associated.

* For Discrete variables, calculate Gini Impurity to get grouping.

* By the max Info Gain related Var, Decision Tree splits first. Repeat steps to further build.


### If Response Variable is Continuous?

* If Response Variable are continuous, Squared Error be calculated instead of InfoGain/GiniImpurity. Var giving least RMSE is to Split first.

* Multivariate Continuous Variables: Keep adding suitable variables, sort'em; Identify Split Rule for minimal RMSE.

* Continuous Dependent & Discrete Independent: Sort dataset by increasing avg Response Value, for cumulative represented Discrete set of VarA. Calculate RMSE for different groupings to pick ideal setting with least.

* Continuous Dependent & Discrete,Continuous Independent: Identify cut-off points for each var separately. Get var that reduces most uncertainity. To be branched out as others.


### Implementing Decision Tree

```
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor

depth_tree = DecisionTreeClassifier()     # DecisionTreeRegressor()
depth_tree.fit(X, y)
```

> `DecisionTreeClassifier` for Classification; `DecisionTreeRegressor` for Regression.


### Common Techniques in Tree Building

* To Avoid Overfitting: Optimize for Leaf Nodes. Restrict number of observations in each leaf node to a minimum count. Specify max depth of tree manually.


### Visualizing Tree Build

```
from IPython.display import Image  
from sklearn.externals.six import StringIO  
from sklearn.tree import export_graphviz
import pydot


## Assuming certain vars are init
## * GRAPHVIZ2_BIN_PATH as filesystem path with GraphViz2 binary
## * data.columns[1:] as providing Independent variable names as Features

import os
os.environ["PATH"] += os.pathsep + GRAPHVIZ2_BIN_PATH

features = list(data.columns[1:])

dot_data = StringIO()  
export_graphviz(depth_tree, out_file=dot_data, feature_names=features, filled=True, rounded=True)
graph = pydot.graph_from_dot_data(dot_data.getvalue())  
Image(graph[0].create_png())
```


### Impact of Outliers on Decision Tree

* Outliers have little impact in Decision Tree for Classification, unlike previous apporaches.

* For Continuous Variable prediction, it would be chsllenging as RMSE is used.

---
