
## Chapter.10 Recurrent Neural Network

> CNNs perform well for Image Classifications with translated & rotated (static) patterns. Not well for temporal patterns. RNNs help with that.

### Architecture

* RNNs can be architected in multiple cardinalities: `1 to 1` (like Word2Vec NNs), `1 to many` (like captioning an input image), `many to one` (e.g. sentiment analysis on move reviews), `many to many` (e.g. language translation).

* E.g. "Given string of words, predict following word." Sample: 'Give me new sample'

```
 Encode each word, leaving space for an extra.
    Give: {1,0,0,0}; me: {0,1,0,0}; new: {0,0,1,0}

 Encoding phrase: 'Give me new' => {1,1,1,0}

 Prep Training Dataset: Input => {1,1,1,0}; Output => {0,0,0,1}

 Build model with Input & Output.
```

> Above input doesn't change even for phrase 'Give new me'. Thus requires a different many-to-1 representations as

```
 Give->[]-->[]-,
 me  ->[]----->[]-,
 new ->[]-------->[]-->[]

 represents different than

 Give->[]-->[]-,
 new ->[]----->[]-,
 me  ->[]-------->[]-->[]
```


### Interpreting

* RNN is like mechanism to hold memory within HiddenLayers. Connecting a previous version of HiddenLayer with itself alongwith Input, embeds memory.


### Working Details

* `WxH` is randomly init 4x3-dim. HL i.e. MatMul of IL & WxH, is 1x3-dim for each Input row (i.e. 1x4-dim),

```
Sample

Input: 'This is an exmaple'  | Expected Output
  This  ->[[1,0,0,0],          is    ->[[0,1,0,0],
  is    -> [0,1,0,0],          an    -> [0,0,1,0],
  an    -> [0,0,1,0],          example> [0,0,0,1],
  example> [0,0,0,1]]          blank -> [0,0,0,0]]

WxH (size: 4x3)              | Hidden Layer (size: 4x3)
   [[0.033, 0.021, 0.065],      [[.,.,.],
    [0.052, -0.08, -0.04],       [.,.,.],
    [0.048,  0.04, -0.08],       [.,.,.],
    [0.056, -0.03, -0.06]]       [.,.,.]]
```

* Expected output is one-hot encoded version of word in sequence. In RNN a HL is connected to another HL. So, Weights (WyH, random init) associated with previous HL & current HL would be 3x3-dim matrix as prev HL was 4x3 i.e. 1x3-dim for each input.

* Calc of HL at time steps is via `h(t) = @h(Zh(t))`, here `@h` is activation fn (e.g. Tanh) and `Zh(t) = WxH*X(t) + WyH*h(t-1) i.e. (MatMul of WxH & IL + MatMul of WyH & HL)`. 

* Time Step 1: HL value would be MatMul of IL & WxH (since no previous HL value).

* Time Step 2: HL contains current step value and value from previous, both to be incorporated.

> Time Step 2 onwards, each Step contains an influence of all previous values.

* Calculation of Softmax to normalize output; calculation of Cross Entropy Error; minimizing loss though Forward & Back Propagation epochs is similar to that of simple NNs.


### Implementing: SimpleRNN

> [code](../implementations/rnn-simple.py)

* Initialize documents & encode words corresponding to those; pad doc to a max length to mantain input size consistency.

* Input shape should be of form (step-count, feature-count-per-step), so if each input is based on 2 time steps & each representing only 1 column then shape would be (2,1).

```
model.add(
    SimpleRNN(1,
              activation='tanh',
              return_sequences=False,
              recurrent_initializer='Zeros',
              input_shape=(2, 1),
              unroll=True)
)
```

> Here: `1` is for a neuron in HL; `return_sequences=false` as not returning output sequences; `unroll=True` is to consider previous time-steps.

* Fit the compiled model as below; reshaping `padded_docs` so to convert training dataset while fitting `(data_size, time_step_count, feature_count_per_step)` and label to be in array format as final dense layer expects an array.

```
model.fit(padded_docs.reshape(2, 2, 1),
          np.array(labels).reshape(max_length, 1),
          epochs=500)
```


### Implementing: Text Generation

> Using [alice dataset](http://www.gutenberg.org/ebooks/11). [Code](buggy-rnn-textgen.py) fails for shape in current state, but shows logic flow.

* Read the dataset. Normalize text to have only  lowercase, remove punctuations. Ont-hot-encode the words.

* Create Input & Target datasets. Encode Input & Output datasets. Build the model.

> * Indication of Overfitting: Reproducing almost exact text; Low ratio of rows to columns.
> * High column count can be dealt by using Embedding (similar to calculation of Word Vectors).


### Embedding Layer in RNN

> Using [dataset](./sentiment-review.csv) to predict sentiment (discrete output +ve or -ve) of a service based on user tweets. [Code](./rnn-embed-layer.py) showing the logic flow.

> The code has 32 as Embedding vector length i.e. 32 embedded dim inputs. Each connected to 1 HL unit, so 32 weights. A Bias and a final weight connecting prev HL to current HL's unit value; so total 34 weights.


### Issues with Traditional RNN

> In calculation of Gradient with BPTT (BackPropagation Through Time) Algo; consider value U as weights associated with Inputs

* Traditional RNN takes multiple steps into account for a prediction. With every new time step; earlier layer's output have diminishing impact.

* Problem of Vanishing Gradient: (U<1, gradients are small) Some outputs have dependency on a unit output of much earlier. E.g. "I like Music and Poetry. I can listen to symphonies all day while I'm reading ---.", here new word would be 'Poetry' based on early unit.

* Problem of Exploding Gradients: (U>1, gradients increase heavily giving high weightage to earlier inputs) low weights to near inputs for the word cause issue.


### LSTM

> Long Short-Term Memory helps avoid Vanishing & Exploding Gradient Problems.

* Compared to Traditional RNNs: Input X & HL Output h are same, the Activation within HL is different.

```
         C_of(t-1)                h_of(t-1)
           |,                        |,
        ,--!-------------------------!----,
        |  |                         |----|--X_of(t)
        | (X)<-- f_of(t) [Sigmoid]---|    |
        |  |                         |    |
        | (+)<-(X)-i_of(t) [Sigmoid]-|    |
        |  |    '<-C_of(t) [tanh]----|    |
        |  |                         |    |
        |  |   ,----(x)<-O(t) [Sigmoid]   |
        |  |   |     |                    |
        |  |-[tanh]  |--------------------|--->h_of(t)
        '--!---------!--------------------'
           |,        |,
          C_of(t)  h_of(t)
        ,--!---------!--------------------,
```

> * 'C' as Cell State, a way to capture long-term dependencies.
> * 'f' as Forget Gate `f_of(t) = Sigmoid( (Wxf * X^(t)) + (Whf * h^(t-1)) + b_of(f) )` mechanism to specify what's to be forgotten (historical words captured in `h^(t-1)` to be selectively forgotten).
> * Next step, input to update cell state achieved through Sigmoid on input `i_of(t)` and magnitude of update (+/-) via tanh `g_of(t)`.
> * Thus, cell state finally be `C_of(t) = (C_of(t-1) * f_of(t)) + (i_of(t) * g_of(t))`.
> * Final Gate, combination of Input & Cell State output to next HL as `O_of(t)`.
> * Finall HL represented as `h_of(t) = O_of(t) * tanh(C_of(t))`

* Cell State can memorize values needed at a later time providing better results than Traditional RNN.


### Implementing Basic LSTM

> [basic keras code](./rnn-lstm.py) for logic flow

* Order of weights & bias in LSTM layer is: Input Gate, Forget Gate, Modulation Gate (Cell Gate), Output Gate.

* In practice, inputs would be encoded into vectors to obtain predictions.


### Implementing LSTM for Sentiment Analysis

> Using `model.add(LSTM(10))` instead of `model.add(SimpleRNN(..))` as in embed model would provide better accuracy.

---
