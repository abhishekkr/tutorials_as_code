# -*- coding: utf-8 -*-
"""
numpy provides n-dimensional array object; many functions to manipulate these
it doesn't know of computational graphs, deep learning or gradients

we can use it to fit 2-layer network to random dat,
a by manually implementing forward and backward passes
"""


import numpy


## initializing dimensions
BatchSize, InDimension, HiddenDimension, OutDimension = 64, 1000, 100, 10


## randomly initialize input and output data
in_data = numpy.random.randn(BatchSize, InDimension)
out_data = numpy.random.randn(BatchSize, OutDimension)


## randomly initialize weights
weight01 = numpy.random.randn(InDimension, HiddenDimension)
weight02 = numpy.random.randn(HiddenDimension, OutDimension)


learning_rate = 1e-6


def check_loss(prediction, value):
    ## compute and print loss
    loss = numpy.square(prediction - value).sum()
    print(tensor, loss)


def calculate_gradients(hid, hidden_relu, out_data_prediction):
    ## backdrop to compute gradients of weight01,02 wrt loss
    gradient_out_data_prediction = 2.0 * (out_data_prediction - out_data)
    gradient_weight02 = hidden_relu.T.dot(gradient_out_data_prediction)
    gradient_hid_relu = gradient_out_data_prediction.dot(weight02.T)
    gradient_hid = gradient_hid_relu.copy()
    gradient_hid[hid < 0] = 0
    gradient_weight01 = in_data.T.dot(gradient_hid)
    return gradient_weight01, gradient_weight02


def recalculate_weight(weight, gradient_weight):
    weight -= learning_rate * gradient_weight
    return weight


for tensor in range(500):
    ## forward pass: compute predicted out_data
    hid = in_data.dot(weight01)
    hidden_relu = numpy.maximum(hid, 0)
    out_data_prediction = hidden_relu.dot(weight02)

    check_loss(out_data_prediction, out_data)

    gradient_weight01, gradient_weight02 = calculate_gradients(hid, hidden_relu, out_data_prediction)

    ## update weights
    weight01 = recalculate_weight(weight01, gradient_weight01)
    weight02 = recalculate_weight(weight02, gradient_weight02)
