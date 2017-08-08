

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

References:

* ANN
> * [The Project Spot](http://www.theprojectspot.com/tutorial-post/introduction-to-artificial-neural-networks-part-1/7)

---
