
## Notes from "Attention Is All You Need"

> [Paper on Transformers](https://arxiv.org/pdf/1706.03762.pdf) by Google Brain team & others

> Proposal of a new simple network architecture, Transformer, solely on Attention mechanisms.
> Models are parallelizable and faster to train.

### Intro

* RNN, LSTM & Gated RNNs are established AI practices for sequence modeling & transduction problems as language modeling and machine translations.

* Attention mechanisms allow modeling of dependencies without regard to their distance in input/output sequences.

* Reducing sequential computation.. here, Transformer are proposed as a model architecture relying entirely on attention mechanism avoiding recurrence.


### Background

* Extended Neural GPU, ByteNet & ConvS2S (all using CNNs) also compute hidden representations in parallel for all input/output positions.

* In Transformers the number of operations required to relate signals from 2 arbitrary input or output signals is of constant number. ByteNet has logarithmic & ConvS2S has linear ops.

* Constant ops does reduce effective resolution due to averaging attention-weighted positions, effect of Multi-Head Attention.

* Self-Attention (intra-attention) relates different positions of single sequence to compute sequence representation. Used in tasks like reading comprehension, abstractive summarization, textual entailment and learning task-independent sentence representations.

* Transformer is a transduction model that relies entirely on self-attention.


### Model Architecture

* Most competitive neural sequence models have encoder-decoder structure. Consuming previously generated symbols as additional input.

* Transformer Model Architecture

```
                           ,---------------,    ,----------,                      --,
                       PE  |               |,   |          |,                       |
 Inputs-->[Input    ]-(+)--=-=->[ MHA ]--[A&N]--=>[ FF ]--[A&N]--.                  ~Nx
          [Embedding]         \__/'                              |                  |
                                                   ._____________|                --'
                           ,--------------,        |  |
                           |              |        |  |         ,-----------,
                       PE  |    [Masked]  |,       |, |,        |           |,    --,
 Outputs->[Output   ]-(+)--'-=->[ MHA  ]-[A&N]-=->[ MHA ]-[A&N]-=>[ FF ]-->[A&N]    ~Nx
(shifted) [Embedding]         \__/'             \_________/'                 |    --'
( right )                                                                    |,
                                                                        [Linear]
                                                                             |
                                                                             |,
                                                                       [Softmax]
                                                                             |
                                                                             |,
                                                            Output Probabilities

* MHA  => Multi-Head Attention
* A&N  => Add & Norm
* FF   => Feed Forward
* PE   => Positional Encoding

```

* Encoder Stack is composed of N=6 identical layers with 2 sub-layers (MHA & FF Network) each. 

* Decoder Stack is composed of N=6 identical layers with 3 sublayers (Masked MHA, MHA & FF Network) each. Self-Attention sub-layer is modified to prevent positions from attending to subsequent positions. Masking combined with output embeddings offset-by-one ensures predictions for position `i` can depend on known output positions `< i`.

* Attention function is like mapping a query and a set of key-value pairs to an output. Output is computed as weighted sum (each weight is compatibility function of query with corresponding key) of values.

* Multi-Head Attention (MHA) consists of several attention layers running in parallel.

> * Linearly project keys, values & queries more times with different, learned linear projections; on each then Attention function is performed. These get concatenated and projected again for final value.

```
       ........       ,=============,
 V--->[Linear]'---->,-------------,||
       ........     |   Scaled    |||
 K--->[Linear]'---->| Dot-Product |---->[Concat]---->[Linear]---> MHA
       ........     |  Attention  |_|
 Q--->[Linear]'---->|_____________|


* MHA  => Multi-Head Attention
```

* Scaled Dot-Product Attention, is Attention used.

> * The input consists of queries and keys of dimension `dk`, and values of dimension `dv`. Take dot products of the query with all keys, divide each by √dk, and apply a softmax function to obtain weights on the values.
> * Dot Attention function is faster & space-efficient than Additive Attention.

```
     ,--------,
 Q-->|        |                                         ,--------,
     | MatMul |-->[Scale]-->[Mask (opt.)]-->[SoftMax]-->|        |    Scaled
 K-->|________|                                         | MatMul |--> Dot-
                                                        |        |    Product
 V----------------------------------------------------->|________|    Attention

```

* Transformer uses MHA in 3 different ways

> * In `Encoder-Decoder Attention Layers`, previous decoder layer provide queries and the memory key-vals from output of encoder.
> * Encoder contains `Self-Attention Layers` where all key-vals and queries come from same place, here output of previous Encoder Layer.
> * Decoder has `Self-Attention Layers` which allow each position in decoder to attend to all positions in decoder upto incl. that position.

* Position-wise Feed-Forward Network

> * Contains 2 linear transformations with a ReLU activation in between.
> * Linear transformations are same across positions; use different parameters layer to layer.

* Embeddings and Softmax

> * use learned tokens to convert input & output tokens to vector of a dimension
> * use learned linear transformation & softmax function to convert decoder output to predicted next-token probabilities
> * same weight matrix gets shared between 2 embedding layers & pre-softmax linear transformation; in embedding layer weights multiplied by √d_model_

* Positional Encoding (PE)

> * Transformer being devoid of convolution/recurrence, need to inject info of token's relative/absolute position. PE helps with it.
> * There are many PE choices; used are `PE(pos, 2i) = sin(pos / (10000 ^ (2i/dmodel)))` and `PE(pos, 2i+1) = cos(pos / (10000 ^ (2i/dmodel)))`.
> * Above `pos` is position, `i` is dimension. Sinusoidal version gets used as it might allow model to extrapolate sequence lengths higher than once used during training.

* With `n` sequence length, `d` dimension, `k` kernel size of convolutions & `r` neighborhood size in restricted self-attention

```
 |  Layer-Type                   | Complexity per Layer | Sequential Ops | Max Path Length |
 |-------------------------------+----------------------+----------------+-----------------|
 | * Self-Attention              |  O(n^2 . d)          |  O(1)          |  O(1)           |
 | * Recurrent                   |  O(n . d^2)          |  O(n)          |  O(n)           |
 | * Convolutional               |  O(k . n . d^2)      |  O(1)          |  O(logk(n))     |
 | * Self-Attention (restricted) |  O(r . n . d)        |  O(1)          |  O(n/r)         |
```


### Why Self-Attention?

* Has a simpler total complexity per layer than recurrent as `n` is smaller than `d`.

* Minimum sequential ops.

* Shorter the path between combination of input & output; easier to learn long-range dependencies.

* Self-Attention could yield more interpretable models. Individual Attention heads clearly learn to perform different tasks.


### Training

* Training Data & Batching

> * Trained on standard WMT 2014 English-German dataset of about 4.5 milion sentence pairs. Encoded using byte-pair encoding with shared source-target vocab of 37K tokens.
> * English-French with 36M sentences with tokens split into 32K word-piece vocab.
> * Per training batch, 25K source tokens & 25K target tokens.

* Hardware and Schedule: One machine with nVIDIA P100 GPUs. For base model, each training step took 0.4sec; total 100K steps or 12hrs. For big models, 1s per step for 300K steps or 3days.

* Optimizer: Adam Optimizer with `β1 = 0.9, β2 = 0.98 and E = 10^(−9)`. Increasing learning rate `lrate = d^(-0.5) . min( step_num^(-0.5), step_num . warmup_steps^(-1.5)  )` over warmup-steps of 4K.

* Regularization: three kinds used

> * Residual Dropout applied to output of sub-layer before added to sub-layer input & normalized.
> * Also to sums of embeddings & PE in both encoder & decoder stacks. `P_drop_ = 0.1`
> * Label Smoothing `E_ls_ = 0.1`. Makes model more unsure but improves accurace & BLEU score.


### Results

* Machine Translation: Eng-to-German translation model outperforms previous best by more than 2.0 BLEUs. Eng-to-French model outperforms all previous model at 1/4th training cost.

* Model Variations: To evaluate importance of different components of Transformer, varied base model in different ways measuring change in perf.

> * For Eng-to-German model, used Beam Search but no checkpoint averaging.
> * Varied attention head count & attention key-val dimension while keeping amount of computation constant. Single head attention is 0.9 BLEU worse than best setting. Too many heads also deterirate the quality.
> * Reducing attention key size reduces model quality, so a more sophisticated compatibility function than dot-product might be better.

* English Constituency Parsing: Transformer outperforms Berkley Parser even when trained on a low training set.


### Conclusion

Transformer, the first sequence transduction model based entirely on attention can be trained significantly faster and outperforms previous models.

Code used to train is available at [https://github.com/tensorflow/tensor2tensor](https://github.com/tensorflow/tensor2tensor).

---
