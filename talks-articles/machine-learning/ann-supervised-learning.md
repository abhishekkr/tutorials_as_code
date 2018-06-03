

## Supervised Learning

There are many Supervised Learning algorithms.

Backpropagation is most popular.

### The Perceptron Learning Rule

It works by finding what went wrong and making tweaks to try prevent those errors.

First need to claculate Perceptron's output for each output node.

```
like,
output = f(input1 * weight1  +  input2 * weight2  + ...)
```

Now, compare actual output to target output to find the error

```
like,
error = targetOutput - actualOutput
```

Now, tweak weigths using Perceptron's error

```
like,
weightChange = learningRate * error * input
```

To ensure small gradual changes made to weight, keep a small learning-rate.
But not too low, else it can take too much time to train.

---

### Single Layer Perceptron

Sample with AND function...

```
          0               LearningRate = 0.1
        _|^|__            ExpectedOutput = 1
       (_>= 1_)           ActualOutput = 0
w1=0.3  /    \  w2=0.3    Error = 1
       |      |                       Weight Update
    1->[]    []<-1                    wi = learningRate * error * input
                                      w1 = 0.1 * 1 * 1 + w1  ~> new w1 is 0.4
                                      w2 = 0.1 * 1 * 1 + w2  ~> new w2 is 0.4

          0               LearningRate = 0.1
        _|^|__            ExpectedOutput = 1
       (_>= 1_)           ActualOutput = 0
w1=0.4  /    \  w2=0.4    Error = 1
       |      |                       Weight Update
    1->[]    []<-1                    wi = learningRate * error * input
                                      w1 = 0.1 * 1 * 1 + w1  ~> new w1 is 0.5
                                      w2 = 0.1 * 1 * 1 + w2  ~> new w2 is 0.5

          0               LearningRate = 0.1
        _|^|__            ExpectedOutput = 1
       (_>= 1_)           ActualOutput = 0
w1=0.5  /    \  w2=0.5    Error = 0
       |      |                       No error
    1->[]    []<-1                    training complete

```

---


### Multi Layer Perceptron

MLP learns a function `f(-): Rm -> Ro` with m dimension input and o dimension output.

Given a set of features and a target, it can learn a non-linear function approximator for either classification or regression.

Different from `logistic regression` as have 1 or more non-linear hidden layers between input and output.

Each neuron in hidden layer transforms the values from previous layer into output values.
There are weights between layers' neurons, as list of lists.
There is list of bias vectors, where vector at index `i` represents bias values added to layer `i+1`.

Disadvantages include:
* have a non-convex loss function with more than one local minimum; so random weight initializations can lead to different validation accuracy
* requires guess tuning of hyperparameters like count of hidden neurons, layers, iterations, etc.
* sensitive to feature scaling

#### Classification

MLP can train classification using Backpropagation. After fitting/training model can predict labels for new samples.

```
from sklearn.neural_network import MLPClassifier

x = [[0., 0.], [1.,1.]]
y = [0, 1]

clf = MLPClassifier(solver='lbfgs',
                    alpha=1e-5,
                    hidden_layer_size=(5,2),
                    random_state=1)

clf.fit(x,y)

clf.predict([[2.,2.], [-1.,-2.]])
```


#### Regression

Trains using Backpropagation with no `activation function` in output layer.

It uses `square error` as loss function. Output is a set of continuous values.


#### Regularization

Helps is avoiding over-fitting by penalizing weights with large magnitudes.


#### Algorithms

MLP trains using SGD(Stochastic Gradient Descent), Adam or L-BFGS(Limited-memory Broyden-Fletcher-Goldfarb-Shanno).

---

References:

* ANN
> * [The Project Spot](http://www.theprojectspot.com/tutorial-post/introduction-to-artificial-neural-networks-part-1/7)

* Supervised NN Models
> * [Scikit](http://scikit-learn.org/stable/modules/neural_networks_supervised.html)

---
