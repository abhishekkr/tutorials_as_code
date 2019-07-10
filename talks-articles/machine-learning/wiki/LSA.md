
## LSA (Latent Semantic Analysis)

> [wikipedia](https://en.wikipedia.org/wiki/Latent_semantic_analysis)

* LSA assumes words close in meaning will occur in similar peices of text with distributional hypothesis.

* LSA tries finding `low-rank approximtion` for `term-document matrix` by `dot product` between all term-vectors (giving correlation between terms) and document-vectors (giving document correlation over terms).

* From theory of Linear Algebra, there exists a decomposition of `term-document matrix` giving a `diagonal matrix` alongwith a tall and a wide matrix. This is [SVD](./SVD.md).

---
