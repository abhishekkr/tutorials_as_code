"""
## NumPy UltraQuick Tutorial

[source](https://colab.research.google.com/github/google/eng-edu/blob/main/ml/cc/exercises/numpy_ultraquick_tutorial.ipynb?utm_source=mlcc)

> create/manipulate vectors and matrices

"""

## import module as
import numpy as np


## populate array with specific numbers
### 'np.array' to create NumPy matrix with hand-picked values
one_dim_array = np.array([1.3, 3.7, 4.3, 5.6, 7.9])
print(one_dim_array)

two_dim_array = np.array([[1.3, 3.7], [4.3, 5.6], [6.4, 7.9]])
print(two_dim_array)

### can populate matrix with all zeros or one using 'np.zeros' or 'np.ones'

## populate arrays with number sequences using 'np.arange'
seq_int = np.arange(3, 9)
print(seq_int)

## populate arrays with random numbers
### 'randint' for integers
rand_ints_between_10_and_50 = np.random.randint(low=10, high=51, size=(5))
print(rand_ints_between_10_and_50)
### 'random' for floats between 0.0 & 1.0
rand_floats_between_0_and_1 = np.random.random([5])
print(rand_floats_between_0_and_1)

## math operations on NumPy operands
### 'broadcasting' is expanding shape of an operand in matrix math operation
### to dimensions compatible for that operation
rand_floats_between_1_and_2 = rand_floats_between_0_and_1 + 1.0
rand_floats_between_100_and_200 = rand_floats_between_1_and_2 * 100.0


"""
Task.1 Create a Linear Dataset
to create a simple dataset consisting single feature and label

* assign int sequence from 6 to 20 to a NumPy array name 'feature'
* assign 15 values to NumPy array named 'label' as: 'label = (3) (feature) + 4'; as first value be '(3) (6) + 4 = 22'
"""

feature = np.arange(6, 21)
print(feature)
label = (feature * 3) + 4.0
print(label)


"""
Task.2 Add some noise to the dataset
to mae dataset realistic; insert random noise to each element of 'label' array

* modify each value assigned to 'label' by adding different random float between -2/+2 without 'broadcasting'
  instead create noise array having same dimension
"""
noise = (np.random.random([15]) * 4)
print(noise)
label = label + noise
print(label)
