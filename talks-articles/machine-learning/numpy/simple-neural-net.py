#!/usr/bin/env python3

"""

Perceptron: with no inner layers
        synapse(with weight)
(Input) -----------> (Neuron) ---> (output)
  x               {x1w1 + ... + xNwN}

### Training Process
* take inputs from training example and put through formula to get neuron's output
* calculate error which is difference between output we got and actual output
* depending on severeness of error, adjust weights accordingly
* repeat process N00000 times

### Error Weighted Derivative
* adjust weights by = error.input.0'(output)
`Error = output - actual_output`
* Input either 1 or 0

Backpropagation


numpy uses here
exp—for generating the natural exponential
array—for generating a matrix
dot—for multiplying matrices
random—for generating random numbers

"""

import numpy as np


training_inputs = np.array([
    [1, 0, 0],
    [1, 1, 0],
    [1, 0, 1],
    [0, 0, 0],
    [1, 0, 0],
    [1, 1, 0],
    [1, 0, 1],
    [0, 1, 0],
    [0, 1, 1],
    [0, 0, 1],
    [1, 0, 0],
    [1, 1, 0],
    [1, 0, 1],
    [0, 0, 0],
    [1, 0, 0],
    [1, 1, 0],
    [1, 0, 1],
    [0, 1, 0],
    [0, 1, 1],
    [0, 0, 1]
])

training_outputs = np.array([ [1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,0,0] ]).T

np.random.seed(10)

synaptic_weights = 2 * np.random.random((3, 1)) - 1

print("Random starting synaptic weight", synaptic_weights)


def sigmoid(x):
    return 1 / ( 1 + np.exp(-x) )

def sigmoid_derivative(x):
    return x * ( 1 - x)

"""
For sigmoid:
https://stackoverflow.com/questions/10626134/derivative-of-sigmoid
https://gist.github.com/jovianlin/805189d4b19332f8b8a79bbd07e3f598
"""

def think(iteration_input):
    required_input = iteration_input.astype(float)
    return sigmoid(
        np.dot(required_input, synaptic_weights)
    )

def train():
    global training_inputs
    global training_outputs
    global synaptic_weights
    iterations = 500 # 100000
    for iteration in range(iterations):
        input_layer = training_inputs
        outputs = think(input_layer)
        error = training_outputs - outputs
        adjustments = error * sigmoid_derivative(outputs)
        synaptic_weights += np.dot(input_layer.T, adjustments)

    print('Synaptic weights after training: ',synaptic_weights)
    print('Outputs after training: ',outputs)

def ask_now():
    item1 = str(input('enter 0 or 1: '))
    item2 = str(input('enter 0 or 1: '))
    item3 = str(input('enter 0 or 1: '))
    neural_input = np.array([item1, item2, item3])
    print("Your result is: ", think(neural_input))


if __name__ == "__main__":
    train()
    ask_now()
