
## SVD (Singular Value Decomposition)

> for more: [wikipedia](https://en.wikipedia.org/wiki/Singular_value_decomposition), [machinelearningmastery.com](https://machinelearningmastery.com/singular-value-decomposition-for-machine-learning/), [statbot.co](https://blog.statsbot.co/singular-value-decomposition-tutorial-52c695315254)

* Allows to achieve variable reduction for ML computation.

> Can also be used for least sqaure linear regression, image compression and denoising data.

* SVD is a matrix decomposition method. It's stable as all matrices have an SVD.

* It helps reduce a big matrix to it's constituent parts to make calculations simpler.

* SVD has been known under names like `factor analysis`, `principal component decomposition` and `empirical orthogonal function`; mathematically equivalent and need grip over matrix algebra and vector calculus.

* SVD decomposes a larger matrix into 3 matrices as `A = USVᵀ`

> * A is main `m x n` matrix
>
> * U is `m x n` orthogonal matrix; is left Singular Vector
>
> * S `Sigma` is an `n x n` diagonal matrix; Si are called Singular Values
>
> * V is `n x n` orthogonal matrix, transposed; is right Singular Vector

* Sum of squares of `Singular Values` from diagonal matrix should be equal to toal variance in `A`. Truncated SVD can contain major portion of Variance.

* `scipy` provides with `svd()` and `TruncatedSVD()` methods to aid

* [sample code](../toolbox/numpy/singular-value-decomposition.py)

---
