
## Chapter.12 Principal Component Analysis

> Certain scenarios have limited data points with high Independent variables collected (like medical research).
> In such cases majority of var could be correlated and regression could be extensive due to high weight count to be predicted.
>
> PCA helps in such cases.

### Intuition of PCA

* PCA helps reconstruct dataset with fewer variables.

> * If given 2 Independent Vars (say A & B), where B is mapped like some mathematical function of A. Just A could be used, as translating all values of B via that formulat would get identical columns.
>
> * If A & B are highly correlated but not completely. First principle component is line explaining maximum variance and is linear correlation of multiple Independent variables. Second principle component is line perpendicular to first PC.


### Working Details of PCA

* Given dataset with highly correlated variables x1 & x2 as below

```
 x1 | 1    | 2    | 3    | 4    | 5    |
 x2 | 10.6 | 20.2 | 30.7 | 40.2 | 50.3 |
```

> First PC, `PC1 = w1*x1 + w2*x2`. Second PC, `PC2 = -w2*x1 + w1*x2`. Weights w1 & w2 are random & iterated to optimize; with objective to **maximize PC1 variance** and **overall variance in PC shall be same as in original dataset**.
>
> * PC Variance is `PC1 variance + PC2 variance`; Original Variance is `x1 variance + x2 variance`
>
> * Proceed with identifying optimal values of w1 & w2 to have maximize PC1 variance while keeping difference Original & PC variane negligible.


### Scaling Data in PCA

* Data scaling is a pre-processing step for PCA, so variable have similar ranges.

> This allows to keep w1 near to Zero & w2 near to One.


### Extending PCA to Multiple Variables

* Unlike 2-var, here weights are randomly init in a Matrix form for `[x, x2, x3, ..] * [PC1, PC2, PC3, ..]` dim.

* Two Objectives from before stay; alongwith keeping PC1 variance higher than PC2, PC2 variance higher than PC3 and so on.


### Implementing PCA

* `from sklearn.decomposition import PCA` to import & init via `pca = PCA(n_components=2)` as 2 being Independent var count to fit PCA on top of data `pca.fit(data)`.

* Once fit, transform original data into transformed `x_pca = pca.transform(data)`

> `pca.components_` same as weights associated to PC; `x_pca` as transformed dataset; `pca.explained_variance_ratio_` for amount of variance by each PC


### Applying PCA to MNIST

* MNIST having 28x28 image unrolled with each pixel value as columns. Given `28*28 = 784` columns, intuitively observe for Columns with Zero variance; Columns with minimal variance; and Columns with high variance.

> PCA helps avoiding Zero & Minimal variance columns while achieving decent accuracy with limited column count.
> Experiments have shown it to reduce columns by 95% while using PC.

---
