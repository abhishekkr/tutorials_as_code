
## Chapter.7 Artifical Neural Network

> * NN is a supervised learning algo, mix of multiple hyper-parameters.
> * Some hyper-parameters: hidden layer count, hidden unit count, activation func, learning rate.


### NN Structure

* Input Layer of Independent var. In a Regression, typically one node in Output Layer for Dependent var. In Classification, nodes to match number of classes.

* If a is one of units/neurons in hidden layer.. `a = f(sum of wi*xi)`, with f as activation func.

* Multiple layers aid to non-linearity, so model can learn complex patterns.


### Working of NN Training

> Training entails calibrating all weights while iterating forward & back propagation.

#### Forward Propagation

* For first run assign random weights between 0 & 1 (common way is using Gaussian Distribution). Final weights don't need to be in limit of 0 to 1.

```
 Assuming XOR dataset.
 (Independent, Independent) -> (Dependent)
 (0,0) -> 0; (0,1) -> 1; (1,0) -> 1; (1,1) -> 0

 One Input Layer with 2 inputs. One Hidden Layer 'A' with 3 neurons. One Output Layer with 1 unit.
 * Weights IL to HL
 w11(IL:1 -> HL.A:1) = 0.8; w12(IL:1 -> HL.A:2) = 0.4; w13(IL:1 -> HL.A:3) = 0.3
 w21(IL:2 -> HL.A:1) = 0.2; w22(IL:2 -> HL.A:2) = 0.9; w23(IL:2 -> HL.A:3) = 0.5
 * Weights HL to OL
 wa1(HL.A:1 -> OL) = 0.3;  wa2(HL.A:2 -> OL) = 0.5;  wa3(HL.A:3 -> OL) = 0.9

 So, for Inputs (1,1) 
 IL   =>  (1);  (1)
 HL.A =>  (1x0.8 + 1x0.2)=1.0;  (1x0.4 + 1x0.9)=1.3;  (1x0.3 + 1x0.5)=0.8
```

#### Applying Activation Function

* Activation fn are applied at hidden layer, necessary to model complex non-linear patterns.

* Some major activation fn are

```
Sigmoid = 1 / (1 + e^(-x))

Tanh = (e^x - e^(-x)) / (e^x + e^(-x))

Rectified Linear Unit = x if x>0, else 0
```

* Applying Sigmoid to hidden layer values.

```
 HL.A =>  (1x0.8 + 1x0.2)=1.0;  (1x0.4 + 1x0.9)=1.3;  (1x0.3 + 1x0.5)=0.8
         Sigmoid(1.0) = 0.731; Sigmoid(1.3) = 0.785; Sigmoid(0.8) = 0.689

 OL => (0.73 * 0.3) + (0.79 * 0.5) + (0.69 * 0.9) = 1.235
```

* Using Squared Error loss function (instead being Classification) would make Back Propagation calculations easier to understand.

```
(PredictedValue - ActualValue)^2  =  (1.235 - 0)^2  =  1.525
```

> Reached FORWARD from Input Layer to Output Layer; now reverse.

#### Back Propagation

* Change each weight starting from last layer by a small amount, until minimum error is reached.

* If a change in weight increases error, direction of weight change is reversed.

* Learning Rate helps build trust in weight update decision.

#### Working of Back Propagation

* Initial Overall Error is `1.525`. Changing weight `wa1(HL.A:1 -> OL)` from `0.3 to 0.29`; reduces error to `1.507`. So, we know reducing it further is right direction.

```
 initial, OL => (0.73 * 0.3) + (0.79 * 0.5) + (0.69 * 0.9) = 1.235
 now, OL => (0.73 * 0.29) + (0.79 * 0.5) + (0.69 * 0.9) = 1.228

 Overall Error  (1.228 - 0)^2  =  1.507
```

* If `0.05` is Learning Rate. Weight of value `0.3` can be updated as `0.3 - 0.05 (Error Reduction due to Weight Change)`.

```
 initial reduction in weight = 0.01
 wa1 = (0.3 - 0.05((1.525 - 1.505) / 0.01)) = 0.2
 wa2 = (0.5 - 0.05((1.525 - 1.505) / 0.01)) = 0.4
 wa3 = (0.9 - 0.05((1.525 - 1.505) / 0.01)) = 0.8
 new, OL => (0.73 * 0.2) + (0.79 * 0.4) + (0.69 * 0.8) = 1.014

 Overall Error  (1.014 - 0)^2  =  1.028
```

* Now, updating weights in the layer before (here among Input Layer to Hidden Layer)

```
 * Weights IL to HL, let's reduce by 0.01
 w11 = 0.79; w12 = 0.39; w13 = 0.29; w21 = 0.19; w22 = 0.89; w23 = 0.49

 HL.A =>  (1*0.79 + 1*0.19)=0.98;  (1*0.39 + 1*0.89)=1.28;  (1x0.29 + 1x0.49)=0.78
          Sigmoid(0.98) = 0.727; Sigmoid(1.28) = 0.782;     Sigmoid(0.78) = 0.686
 OL => (0.727 * 0.2) + (0.782 * 0.4) + (0.686 * 0.8) = 1.007

 Overall Error  (1.007 - 0)^2  =  1.014
```

* Reducing weights decreased error by small value, so we'll reduce further. Keep repeating Forward & Backward propagation until Overall Error is most minimized.

> `from scipy.special import expit ; expit(x)` would give `Sigmoid(x)` value

#### Stochastic Gradient Descent

* Gradient Descent is way of minimizing error in above scenario. Stochastic stands for Training Data to calculate error & thereby weight update.

* Other optimizers with similar functionality are: RMSprop, Adagrad, Adadelta, Adam, Adamax, Nadam.

#### What use Learning Rate?

* A lower Learning Rate is preferred for opitmal results. (Only a very slight nudge to not leave a mark)


### Batch Training

* Error calculation to be done over a batch of data, and using Overall Errors in those different batches.

> Typical batch size is generally high like 32 or something.

* Softmax Activation suits where output is beyond expected value between 0 & 1

```
So if, predicted binary output are 1.233 & 0.594
Step:1 Raising output to its exponential value, giving 3.43 & 1.81
Step:2 Getting new probabilities as (3.43/(3.43+1.81)) & (1.81/(3.43+1.81)) i.e. 0.65 & 0.35
Now, calculate cross entropy error..
Say if actual output was 1 & 0.. -((1 * logBase2(0.65)) + (0 * logBase2(0.35)) = 0.62

Now deploy Gradient Descent again to minimize overall cross entropy error
```


### Different Loss Optimization Functions

* Mean Squared Error
* Mean Absolute % Error
* Mean Squared Log Error
* Squared Hinge
* Hinge
* Categorical Hinge
* Logcosh
* Categorical Cross Entropy
* Sparse Categorical Cross Entropy
* Binary Cross Entropy
* Kullback Leibler Divergence
* Poisson
* Cosine Proximity

#### Scaling Dataset

> Generally NN perform well with scaled input dataset.

* With high magnitude Independent var, weights slowly arrive at optimal weight value. After scaling dataset first, inputs as small number help arrive optimal weight value sooner.


### Implementing

```
from keras.datasets import mnist
import matplotlib.pyplot as plt

# %matplotlib inline
# download & load MNIST dataset
(X_train, y_train), (X_test, y_test) = mnist.load_data()

# plot images as grayscale
for idx in range(4):
    plt.subplot(221+idx)
    plt.imshow(X_train[idx], cmap=plt.get_cmap('gray'))
plt.show()


import numpy as np
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout
from keras import utils

## Preprocess Dataset
num_pixels = X_train.shape[1] * X_train.shape[2]
# reshape the inputs
X_train = X_train.reshape(X_train.shape[0], num_pixels).astype('float32')
X_test = X_test.reshape(X_test.shape[0], num_pixels).astype('float32')
# scale inputs
X_train = X_train / 255
X_test = X_test / 255
# one hot encode the output
y_train = utils.to_categorical(y_train)
y_test = utils.to_categorical(y_test)
num_classes = y_test.shape[1]

## Build Model
model = Sequential()
model.add(Dense(1000, input_dim=num_pixels, activation='relu'))  # add 1K units in HL, apply relu activation
model.add(Dense(num_classes, activation='softmax'))  # init output layer
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.summary()
model.fit(
    X_train,
    y_train,
    validation_data=(X_test, y_test),
    epochs=5,
    batch_size=1024,
    verbose=1,
)
```

> as number of epochs increase; accuracy of test dataset increases


### Avoid Overfitting: Regularization

* Other than Scaling, the loss function minimizes loss over increasing epochs. Too many epochs can overfit.

> * It's bad overfitting, if accuracy on Test Dataset slows/stops out, while keeps increasing on Train Dataset.
> * When plotting weight range on histogram for all epochs. Higher epoch counts have more weight ranges.

* Having heavy weight aren't desirable for Generalization purposes. Regularization penalizes for high weights.

* Major types L1 & L2 Regularization. These add additional cost term to error based on weights `L1: sumOf(|wi|)` and `L2: sumOf(w,^2)`.


### Assign Weight to Regularization term

* Modified loss function with L2 Regularization: `Overall Loss = sumOf(x-x')^2 + ,\ sumOf(wi^2)`

* Modified loss function with L1 Regularization: `Overall Loss = sumOf(x-x')^2 + ,\ sumOf(|wi|)`

* In python, implemented as

```
from keras import regularizers

model3 = Sequential()
model3.add(
    Dense(
        1000,
        input_dim=784,
        activation='relu',
        kernel_regularizer=regularizers.l2(0.001)
    )
)
model3.add(
    Dense(
        10,
        activation='softmax',
        kernel_regularizer=regularizers.l2(0.001)
    )
)
model3.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model3.fit(
    X_train,
    y_train,
    validation_data=(X_test, y_test),
    epochs=100,
    batch_size=1024,
    verbose=2,
)
```

> Now majority of weights are closer to Zero, avoding overfitting.

---
