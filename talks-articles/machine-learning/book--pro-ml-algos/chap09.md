
## Chapter.9 Convolutional Neural Network

### Traditional NN's Problem

* NNs are bad with inputs that are translation invariant (similar but shifted values). Example to detect a person on a chair & standing up in all variation of postures. CNNs are good with such classifications.


### CNN?

* Convolution is multiplication between two matrices while sliding smaller over larger, not MatMul.

```
 MatrixA: [[1,2,3,4],         MatrixB: [[1,2],    Result: [[44,54,64],
           [5,6,7,8],                   [3,4]]             [84,94,104],
           [9,10,11,12],                                   [124,134,144]]
           [13,14,15,16]]
 MatrixB: [[1,2], [3,4]]

 1) {1,2,5,6} subsection of Larger mutliplied with {1,2,3,4} smaller
    1*1 + 2*2 + 5*3 + 6*4 = 44

 2) {2,3,6,7} subsection of Larger mutliplied with {1,2,3,4} smaller
    2*1 + 3*2 + 6*3 + 7*4 = 54

 3) {3,4,7,8} subsection of Larger mutliplied with {1,2,3,4} smaller
    3*1 + 4*2 + 7*3 + 8*4 = 64

 4) {5,6,9,10} X {1,2,3,4} = 84
 5) {6,7,10,11} X {1,2,3,4} = 94
 6) {7,8,11,12} X {1,2,3,4} = 104

 7) {9,10,13,14} X {1,2,3,4} = 124
 8) {10,11,14,15} X {1,2,3,4} = 134
 9) {11,15,12,16} X {1,2,3,4} = 144
```

> Smaller Matrix is called `filter` or `kernel`, with values arrived at statistically via Gradient Descent.
> Values are like Constituent Weights.

* Convolution to Activation: Similar to NNs, values pass through an Activation Fn (traditional fn are supported like Sigmoid, Tanh, ReLU).

> ReLU fn on above output, keep it same as all numbers are positive.

* Convolution Activation to Pooling: If Max Pooling considers a 2x2 convolution output, gives max value as output. If output was bigger matrix as 4x4, it divides into non-overlapping 2x2 to choose highest value for a new 2x2 block.

> Sum & Average Pooling are also available.

* Say for Image as input data; Traditional NNs have each pixel associated with a weight. Thus a slight pixel shift is not accomodated well. With Convolution & Pooling, pixel share weights constituted within each filter. The issue remains if highlighted pixel shifts far away.


### Creating CNNs

* N pooling steps results in last N units of translation invariance.

```
import matplotlib.pyplot as plt
import numpy as np
from keras.datasets import mnist
from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense

## import & reshape data to fit CNN
(X_train, y_train), (X_test, y_test) = mnist.load_data()
X_train = X_train.reshape(X_train.shape[0], X_train.shape[1], X_train.shape[1], 1).astype('float32')
X_test = X_test.reshape(X_test.shape[0], X_test.shape[1], X_test.shape[1], 1).astype('float32')
X_train = X_train / 255
X_test = X_test / 255
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)
num_classes = y_test.shape[1]

## build a model
model = Sequential()
model.add(Conv2D(10, (3,3), input_shape=(28, 28,1), activation='relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Flatten())
model.add(Dense(1000, activation='relu'))
model.add(Dense(num_classes, activation='softmax'))
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.summary()

model.fit(
    X_train,
    y_train,
    validation_data=(X_test, y_test),
    epochs=5,
    batch_size=1024,
    verbose=1
)

## for above convolution 1unit shift works alright, but not more
def shift_n_predict(offset):
    X_train1 = X_train[0]
    pic = np.zeros((28,28))
    pic2 = np.copy(pic)
    for i in range(X_train1.shape[0]):
        pic2 = X_train1[i,:,:]
        pic = pic+pic2
    pic = (pic/X_train1.shape[0])
    for i in range(pic.shape[0]):
      if i<20:
        pic[:,i] = pic[:,i+offset]
    plt.imshow(pic)
    ## predict the label
    pred = model.predict(pic.reshape(1,28,28,1))
    print(pred)

shift_n_predict(1)  ## shifting by 1units
shift_n_predict(2)  ## shifting by 2units
```


### Working Details

```
# import relevant packages
from keras.datasets import mnist
import matplotlib.pyplot as plt
import numpy as np
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten, Conv2D, MaxPooling2D
from keras.utils import to_categorical
from keras import backend as K
from keras import regularizers

# with sample dataset
X_train = np.array(
    [[[1,2,3,4], [2,3,4,5], [5,6,7,8], [1,3,4,5]],
     [[-1,2,3,-4], [2,-3,4,5], [-5,6,-7,8], [-1,-3,-4,-5]]]
)

y_train = np.array([0,1])

# normalize
X_train = X_train / 8
# one-hot-encode the outputs
y_train = to_categorical(y_train)

# reshape input into required format (num-samples, height-image, width-image, num-channels)
X_train = X_train.reshape(X_train.shape[0], X_train.shape[1], X_train.shape[1], 1).astype('float32')

model = Sequential()
model.add(Conv2D(1, (3,3), input_shape=(4, 4, 1), activation='relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Flatten())
model.add(Dense(10, activation='relu'))
model.add(Dense(2, activation='softmax'))
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
model.summary()

model.fit(X_train, y_train, epochs=100, batch_size=2, verbose=1)
print(model.layers)

names = [weight.name for layer in model.layers for weight in layer.weights]
weights = model.get_weights()
for name, weight in zip(names, weights):
    print(name, weight.shape)

pred = model.predict(X_train[0].reshape(1,4,4,1))
print(pred)
```

* Each pixel of image as Independent variable, via Flattening.


### Connecting Dots: Feed Forward Network

* Steps performed so far: `Convolution -> Pooling -> Flattening -> HL -> Calc Output Probability`.


### Other Details of CNN

> LeNet

* (1.) A grayscale 28x28 dim image. Six filters 3x3 in size generate six 26x26 size images.

* (2.) Color image have 3channels (R G B); output of Step(1) has 6 channels. Now performing MaxPooling2D on each separately results in six 13x13-dim images.

* (3.) Step(2)'s 6 channels multiploed with weights of 3x3x6 dim, convolving over a 3-dim image (of dimensions 13x13x6). Results into 11x11 image for each filter. E.g. If Considering 10 different weight matrices.. result would be 11x11x10 dim image.

* (4.) Max Pooling on each (of 10) 11x11 image result in 5x5 image (as floor(11/2)).

> `Stride` is amount of filter convolution over original image from a step to next. With **stride** 2, there would be 2pixel convolution between consecutive convolutions. The output would be `2x2 matrix`.

> Convolution reduces image size; `Padding` helps avoid it. E.g. A 28x28 image translated to 30x30 (with Zero Padding) when convolved with 3x3 filter, result into 28x28 image.


### Backward Propagation in CNN

* Similar to NN; but updating filter/matrices of weights instead of weights.

* Regularization helps given it can have millions of parameters in a CNN. `Dropout` method randomly choose % of weight to not update (generally 20%) & training entire network over whole number of epochs.


### Data Augmentation

> When image is translated by more pixels than convolutional pooling layers, prediction worsens. Resolved by Data Augmentation.

* A translated image is same as new image generated from original. Can be generated using `keras`'s `ImageDataGenerator`.

```
from keras.preprocessing.image import ImageDataGenerator

shift=0.2
datagen = ImageDataGenerator(width_shift_range=shift)
datagen.fit(X_train)

i=0
for X_batch,y_batch in datagen.flow(X_train,y_train,batch_size=100):
  i=i+1
  print(i)
  if(i>500):
    break
  X_train=np.append(X_train,X_batch,axis=0)
  y_train=np.append(y_train,y_batch,axis=0)
print(X_train.shape)
```

> above 100 batchs of 500 random shuffling, generate 50K 20% shift-ed (`0.2`) image from original.

* Such Data Augmentation help with further generalization of variations, even with few pooling layers.

---
