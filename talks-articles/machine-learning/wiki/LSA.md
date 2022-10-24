
## LSA (Latent Semantic Analysis)

> [wikipedia](https://en.wikipedia.org/wiki/Latent_semantic_analysis), [datacamp.com](https://www.datacamp.com/community/tutorials/discovering-hidden-topics-python)

* LSA assumes words close in meaning will occur in similar peices of text with distributional hypothesis.

* LSA tries finding `low-rank approximtion` for `term-document matrix` by `dot product` between all term-vectors (giving correlation between terms) and document-vectors (giving document correlation over terms).

* From theory of Linear Algebra, there exists a decomposition of `term-document matrix` giving a `diagonal matrix` alongwith a tall and a wide matrix. This is [SVD](./SVD.md).

* Uses a BoW (Bag of Word) model resulting in term-document matrix (rows for words, columns for documents).

* LSA learns latent topics by performing matrix decomposition on document-term matrix using [SVD](./SVD.md).

* LSA typically used as dimesnion reduction or noise reduction technique.

---
