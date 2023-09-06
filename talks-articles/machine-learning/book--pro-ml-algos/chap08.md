
## Chapter.8 Word2Vec

> Word2Vec is a NN-based text-mining analysis. Similar words to have similar vectors.

### Building a Word Vector from Scratch

> Hypothesis: Related words would have similar words around in a sentence, more often.
> * Say input words `{enjoy, reading, books}` with vector form `{0,1,1,1,0}` for 5-dim input vector into a 3-dim vector (Hidden Layer has 3 units). Output Layer with Softmax Classifier to have 5 units.

```
 8 Vectorized Inputs   Hidden Layer   HL:Output      HL:OL Weights   OL
 (5 words, size: 8x5)  (size: 5x3)    (size: 8x3)    (size: 3x5)     (size: 8x5)
 [[0,1,1,1,0],         [[3.38, ..],   [[-0.75,..],   [[-1.95, ....]  [[.....],
  [1,0,1,1,0],          [1.78, ..],   [...],         [.....],        [.....],
  [1,1,0,1,0],          [-5.65,..],   [...],         [.....]]        [.....],
  [1,1,1,0,0],          [3.11, ..],   [...],                         [.....],
  [0,0,1,1,1],          [1.66, ..]]   [...],                         [.....],
  [1,0,1,1,0],                        [...],                         [.....],
  [1,0,0,1,1],                        [...],                         [.....],
  [1,0,1,0,1]]                        [...]]                         [.....]]

HL (5x3) => as 5 inputs to each of 3 neurons
HL:Out (8x3) => MatMul of IL & HL
HL:OL Weights (3x5) => 3 HL columns mapped with 5 OL columns
OL (8x5) => MatMul of HL:Output & HL:OL Weights.
```

> * Randomly init weight is used, no Activation Fn is used in HL unlike NN.
> * OL has +ve & -ve numbers so they are passed via Softmax to have between 0 to 1.
> * Then Cross Entropy Error across whole batch is calculated. `Cross Entropy Loss = sumOf( ACTUALi * LogBase2(PROBABILITYi) )`.
> Now aim is to vary initial weights using Optmizer to reduce Overall Cross Entropy Error.


### Methods

* Above used method is `CBOW` (Continuous Bag Of Words) model.

> For a sentence "Let us build a new model for this problem." & fixing window size 2-words on left & right.
> Input & Output Vectors would look like

```
{Let,us,a,new}       Input Words ==> Output Word  {build}
{build,a,model,for}                               {new}
{a,new,for,this}                                  {model}
{new,model,this,problem}                          {for}
```

* Another approach `skip-gram` model, above is reverse generating

```
{build}      Input Word ==> Output Words    {Let,us,a,new}
{new}                                       {build,a,model,for}
{model}                                     {a,new,for,this}
{for}                                       {new,model,this,problem}
```


### Pitfalls for Word2Vec Model

* Frequent Words: Words like `the, a, of, is` appear often, need to be penalized.

> * Using Probability of word selection with `P(wi) = ( Sqrt( z(wi) / 0.001 ) + 1  ) * (0.001 / z(wi))`
> here `z(wi)` is count of `wi` occurence over total occurence of any word.

* Negative Sampling: With N unique words (each vector of N-dim) & M-dim vectors in HL means NxM weights from HL to OL in a single HL model. If NxM is very high, means longer training time & also might result into Overfitting of data. Negative Sampling helps with it.

> * Picking index where Output is 1 (correct label) & C (very low count) random Negative indices with label 0. Reduces single iteration weights to `CxM`.
> * In Word2Vec, Negative index selection bases on words with higher frequency of selection. Probability `P(wi) = FWi / sumOf( FWi )`, here `FWi = f(wi)^(3/4)` & `f(wi)` is frequency of `wi` word.
> Repeating higher frequency words more often in selection.


### Implementing

> gist, not complete

```
import nltk
import gensim
from gensim.models import word2vec
import pandas as pd

import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

features_count = 100    # word vector dim, HL neurons
min_word_count = 50     # frequency cut-off of words accepted for calculation
workers_count = 4
context = 9             # context window size
downsampling = 1e-4     # for frequent words, lower probability

## input vocab is tokenized sentence as ["input", "vocab", "is", "tokenized"]
## Training Model
w2v_model = word2vec.Word2Vec(t2, workers=workers_count, size=features_count,
                min_count=min_word_count, window=context, sample=downsampling)

## with trained model, vector of weights for any word are available as
## model['word']

## most similar word to given word as
## model.most_similar('word')
```

---
