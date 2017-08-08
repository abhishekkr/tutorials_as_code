
## Tensorflow Machine Learning Cookbook

> [sample chapters](https://www.packtpub.com/mapt/book/big-data-and-business-intelligence/9781786462169/1/ch01lvl1sec10/How+TensorFlow+Works)

> [code for book](https://github.com/nfmcclure/tensorflow_cookbook)

> [TensorFlow](https://www.tensorflow.org)

### 01. Getting Started with TensorFlow


#### How TensorFlow Works

Simple flow might seem easy, but developing complicated algorithms comes relatively easy.

* Getting Ready

> Here we'll only concern with Python library wrapper of TensorFlow, although most of original code is in C++.
> Use Python 3.4+ and TensorFlow 0.12. Scipy, Numpy and Scikit-Learn (also included in [Anaconda package](https://www.continuum.io/downloads)).
> Runs on CPU, most algorithms run faster on GPU supporting Nvidia Compute Capability v4.0+ (v5.1 recommended).
> Will need [Nvidia Cuda Toolkit and v5.x+](https://developer.nvidia.com/cuda-downloads)


* How to do it

most recipe follow this outline

> * Import or Generate Datasets
>
> * Transform and normalize data, tensorflow has built-in functions to do so
>

```
data = tf.nn.batch_norm_with_global_normalization(...)
```

>
> * Partition datasets into train, test and validation sets
> many algo require hyperparameters tuning, so we set aside a validation set for determining the best of hyperparameters
>
> * Set algorithm parameters (hyperparameters)
> set of parametes held constant throughout procedure can be initialized together for ease of user
> for e.g. number of iterations, learning rate, or other fixed parameters
>

```
learning_rate = 0.01
batch_size = 100
iterations = 1000
```

>
> * Initialize variables and placeholders
> TF depends on what it can and can't modify. TF will adjust variable and wight/bias during optimization to minimize `loss` function.
> Feed in data via placeholders.Initialize variables and placeholders with size and type, so TF knows what to expect. TF needs to know type of data to expect.
> More bytes used for precision results in slower algorithms.
>

```
a_var = tf.constant(42)
x_input = tf.placeholder(tf.float32, [None, input_size])
y_input = tf.placeholder(tf.float32, [None, num_classes])
```

>
> * Define the model structure
> done by building a computational graph, TF chooses what operations and values must be variable and placeholders to arrive at our model outcomes
>

```
y_pred = tf.add(tf.mul(x_input, weight_matrix), b_matrix)
```

>
> * Declare the loss functions
> After defining model, we must be able to evaluate the output.
> Here we declare `loss` function, it tells us how far our predictions are.

```
loss = tf.reduce_mean(tf.square(y_actual - y_pred))
```

>
> * Initialize and train the model
> We have everything in place, need to create an instance of our graph.
>

```
## one way to initialize computational graph
with tf.Session(graph=graph) as session:
  ...
  session.run(...)
  ...


## we can also initiate our graph with
session = tf.Session(graph=graph)
session.run(...)
```

>
> * Evaluate the model
> By looking at how well it does with new data through some specified criteria.
>
> * Tune hyperparameters
> We'll want to go back and change some hyperparameters, based on model performance. Change and evaluate on validation set.
>
> * Deploy/predict new outcomes
>

[tutorials](https://www.tensorflow.org/tutorials/)

---

####



---
---
