
## Chapter.11 Clustering

> Unsupervised learning technique to group data points.

### Intuition of Clustering

> E.g. Fastfood Chain with 4K outlets. Major metric for eval of Outlet Managers is total sales.
> Scenario.1: Eval only on sales made. Drawback being no account of outlet locations.
> Scenario.2: Cluster outlets by purchasing power demographic; say city & rural areas. Drawback being even certain city outlets may be in college areas and other in low econominc zone.

* Probable reasons for difference could be due to
> * Difference in products sold in different outlets.
> * Age group of regular customers
> * Lifestyle of regular customers

* For simplicity, let's define as
> * City vs Rural
> * High-Value Products vs Low-Value Products
> * High vs Low Age Group
> * Permium vs Budget Shoppers

* Above 4 categorizations bring about 16 different clusters. Impact of some factors might be high; minimal of others.

* Granular segmentation makes clustering useless. Only main impacts should be considered.

* K-means clustering helps identify main few factors to get better clusters.


### The Process of Clustering

> Plot Outlets on varying population of allocated region & sales made.

* Select K, number of clusters. Select random K points (outlets in example).

* Measure (euclidean) distance between each datapoint and K points. Assigning each datapoint to nearest K-point.

* Calculate mean of each Cluster into a new K-point.

> Repeast previous 2 steps until convergence is achieved or maximum iterations.

* Calculate variance of each cluster. Keep track of K-points & variance.

> Repeat all steps above from selecting random K-points onwards, to get a lowest variance and pick those K-points.


### Working Details of K-means Clustering

* Applying K-means on a sample dataset. Say A & B as Independent var to be clustered into 2.

> * First Random init for Cluster

```
        A: 5 5 3 0 2 4 2 2 1 5
        B: 0 2 1 4 1 2 2 3 3 4
  Cluster: 1 2 1 2 1 2 1 2 1 2
```

> * Calculate Centroid of each cluster

```
      K-1    K-2
  A:  2.6    3.2  | Values being avergae of A & B's values
  B:  1.4    3    | belonging to K-1 & K-2
```

> * Calculate distance between each Cluster center & data-points to check belonging

```
 Distance between AB(5,0) and C(2.6,1.4) can be given by s = (5 – 2.6)^2 + (0 – 1.4)^2.

       AB:  5,0   ..
      K-1:  7.72  ...
      K-2: 12.24  ...
  Cluster:  1     ...
```

> * Now use this updated Cluster mapping following lower distance and run the Centroid calculation & K-point allocation update again.. unless Convergence or Max Iteration.

* All points in same close cluster are as close as possible. Each group's center is farthest possible from other group's center.

> * `Totss` (Total Sum of Squares) sum of squared distance of all points from whole dataset center.
> * `Cluster Centers` of each cluster are midpoint (mean) of all points.
> * `Tot.withinss` as sum of squared distance from cluster's center.
> * `Betweenss` as diff of Totss & To.withinss.

### Implementing K-means Clustering

```
from sklearn.cluster import KMeans
import numpy as np
import pandas as pd
from urllib.request import urlretrieve
import seaborn as sns


# fetch data
url = ('https://..../driver-data.csv')
filename = "/tmp/dataset-kmeans.csv"
urlretrieve(url, filename)

xdata = pd.read_csv(filename, index_col="id")
print(xdata.head())

kmeans = KMeans(n_clusters=4)
kmeans.fit(xdata)
print(kmeans.cluster_centers_)
print(kmeans.labels_)
unique, counts = np.unique(kmeans.labels_, return_counts=True)
dict_data = dict(zip(unique, counts))
print(dict_data)
```


### Significance of Major Metrics

> Clustering Objective being `Minimize Intra-Cluster Distance` & `Maximize Inter-Cluster Distance`.

* With increase in K, `Tot.withinss` decreases being measure of intracluster distance.

* Lower the Ratio `Tot.withinss / Totss`, higher the quality of Cluster itself.


### Identifying Optimal K

* At a value where further increment in K doesn't correlate to previous rate of reduction in ratio of `Tot.withinss / Totss`.

> It's called Elbow Method, as when graphing (X:K, Y:Ratio) values the general K chosen is at the Elbow data point where decrease in Ratio is low enough.


### Top-Down vs Bottom-Up Clustering

* In **Bottom-Up** approach, start with assumption of no clusters & build multiple clusters until find K below Elbow Curve.

* **Top-Down** alternatively assumes each point is a Cluster & tries combine points based on their distance from other.

* `Heirarchical Clustering` is Classic Top-Down. Distance among all points are calculated; closest points are combined with a new K-point. Process gets repeated to form a final cluster. **Heirarchical** being first 2 points combining, then bringing in 3rd, then 4th and so on.

```
 Distance| A    | B    | C    | D   | E    | F
        A| 0    | 0.71 | 5.66 | 3.61| 4.24 | 3.2     Here minimal distance is
        B| 0.71 | 0    | 4.95 |  it's a diagonal     among D,F => 0.5
        C| 5.66 | 4.95 | 0    |         mirror
        D| 3.61 | 2.92 | 2.24 | 0   |
        E| 4.24 | 3.54 | 1.41 | 1   | 0
        F| 3.2  | 2.5  | 2.5  | 0.5 | 1.12 | 0

 Distance| A    | B    | C    | D,F | E
        A| 0    | 
        B| 0.71 | 0                            Here minimal is A->B : 0.71
        C| 5.66 | 4.95 | 0
      D,F| 3.2  | 2.5  | 2.24 | 0              d(DF->A) = min(d(DF), d(FA))
        E| 4.24 | 3.54 | 1.41 | 1   | 0

 Distance| A,B  | C    | D,F | E
      A,B| 0    |
        C| 4.95 | 0    |
      D,F| 2.5  | 2.24 | 0   |
        E| 3.54 | 1.41 | 1   | 0              Minimal is D,F -> E : 1

 Distance| A,B  | C    | ((D,F),E)
      A,B| 0    |
        C| 4.95 | 0    |
  (D,F),E| 2.5  | 1.41 | 0                    Minimal is (((D,F),E),C) : 1.41

 Distance| A,B  | C    | ((D,F),E)
      A,B| 0    |
        C| 4.95 | 0    |
  (D,F),E| 2.5  | 1.41 | 0                    Minimal is (((D,F),E),C) : 1.41

 Distance  | A,B  | (((D,F),E),C)
        A,B| 0    | 2.5
((D,F),E),C| 2.5  | 0                         Minimal is (((D,F),E),C) : 1.41
```

> Drawback of `Heirarchical Clustering` is large calculation with increasing data points. Suited only for small datasets.

---
