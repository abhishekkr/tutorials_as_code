
## Concepts

### Algebra

* `variable`, notation for any non-constant value part of an equation/expression

* `coefficients`, are non-variable components of a multiplication expression/term (if none it would be 1)

* `functions`, `f(x) = {y}` is a logic-set that takes input & processes them deterministically for an output (same input shall always give same output)

* `linear equations`, any equation that can be represented as `a1.x1 + a2.x2 + ... + aN.xN + b = 0` form with `x1, x2, ..xN` being variables and `a1, a2, ..aN` coefficients

> linear equations could have one or more variables; are meaningful with all coefficients being non-zero

* `logarithms`, inverse function to exponentiation so `log(x) base b = N` means `b to the power N = x`

* `sigmoid function`, a `function` having a S shaped (sigmoid) curve; common e.g. is logistic function `f(x) = 1 / (1 + e_to_power_negative_x)`

> sigmoid function is a bounded, differentiable, real function
>
> that is defined for real input values with non-negative derivative at each point
>
> & exactly one inflection point


### Linear Algebra

* `tensor`, type of data-structure which are generalization of vectors(1st order tensor being 1Dimensional) or matrices (2nd order tensor being 2Dim)

* `tensor rank`, is dimension of vector space by columns i.e. maximum linearly independent columns

* `matrix multiplication`, binarr operation producing a new matrix from multiplication of two


### Trigonometry

> `activation function` is added into a neural network to help network learn patterns in data
>
> used to compute weighted sums of input & biases to decide whether a neuron is to be activated or not

* `tanh` (Hyperbolic Tangent) is another possible function be used as non-linear activation function between layersof a neural network; maps value between -1 and 1 (larger the input closer it is to 1.0)

```
## calculated as below
### (e^x - e^-x) / (e^x + e^-x)

from math import exp
from matplotlib import pyplot
 
# tanh activation function
def tanh(x):
	return (exp(x) - exp(-x)) / (exp(x) + exp(-x))
 
inputs = [x for x in range(-10, 10)]
outputs = [tanh(x) for x in inputs]
pyplot.plot(inputs, outputs)
pyplot.show()
```


### Statistics

* `derivative` of function of a real variable denotes change in output w.r.t. change in input

> derivative of a constant function is zero, derivative of `y = x^2` is a parabola

* `gradient` captures all `partial derivative` info of scalar-valued multivariable function

* `slope`, derivative of a linear function is slope (line)

* `partial derivative` is derivative of a multi-variable function in respect of one of the variables with others held as constants

```
f(x,y) = x^2 - x.y

df/dx  = 2x - y
df/dy  = -x
df = (fx, fy) = ((2x - y), (-x))

applied at (1, -1); df = (3, -1)
```

* `chain rule` is formular expressing derivative of composition of 2 diffferentiable functions `say f & g` in terms of their derivatives

```
h(x) = f(g(x)) for all x
then
h'(x) = f'(g(x)).g'(x)
```

> for `backpropagation algorithm` decides how much to update each weight comparing predicted & desired output of an example by using error derivative with help of Chain Rule
>
> * [diagrammatic ref](https://developers-dot-devsite-v2-prod.appspot.com/machine-learning/crash-course/backprop-scroll/)


### Programming/Console

* [Python](https://docs.python.org/3/tutorial/)

* [Bash Ref Manual](https://tiswww.case.edu/php/chet/bash/bashref.html), [Cheatsheet](https://github.com/LeCoupa/awesome-cheatsheets/blob/master/languages/bash.sh)

* [Learn Shell](http://www.learnshell.org/)

---
