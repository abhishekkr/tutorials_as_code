# -*- coding: utf-8 -*-
"""
a pytorch tensor is conceptually similar to numpy array
tensor is an n-dimensional array
to run pytorch tensor on GPU, just need to cast it to a new datatype
"""


import torch


dtype = torch.FloatTensor
## for GPU ## dtype = torch.cuda.FloatTensor


## initializing dimensions
BatchSize, InDimension, HiddenDimension, OutDimension = 64, 1000, 100, 10


## randomly initialize input and output data
in_data = torch.randn(BatchSize, InDimension).type(dtype)
out_data = torch.randn(BatchSize, OutDimension).type(dtype)


## randomly initialize weights
weight01 = torch.randn(InDimension, HiddenDimension).type(dtype)
weight02 = torch.randn(HiddenDimension, OutDimension).type(dtype)


learning_rate = 1e-6


def check_loss(prediction, value):
    ## compute and print loss
    loss = (prediction - value).pow(2).sum()
    print(tensor, loss)


def calculate_gradients(hid, hidden_relu, out_data_prediction):
    ## backdrop to compute gradients of weight01,02 wrt loss
    gradient_out_data_prediction = 2.0 * (out_data_prediction - out_data)
    gradient_weight02 = hidden_relu.t().mm(gradient_out_data_prediction)
    gradient_hid_relu = gradient_out_data_prediction.mm(weight02.t())
    gradient_hid = gradient_hid_relu.clone()
    gradient_hid[hid < 0] = 0
    gradient_weight01 = in_data.t().mm(gradient_hid)
    return gradient_weight01, gradient_weight02


def recalculate_weight(weight, gradient_weight):
    weight -= learning_rate * gradient_weight
    return weight


for tensor in range(500):
    ## forward pass: compute predicted out_data
    hid = in_data.mm(weight01)
    hidden_relu = hid.clamp(min=0)
    out_data_prediction = hidden_relu.mm(weight02)

    check_loss(out_data_prediction, out_data)

    gradient_weight01, gradient_weight02 = calculate_gradients(hid, hidden_relu, out_data_prediction)

    ## update weights
    weight01 = recalculate_weight(weight01, gradient_weight01)
    weight02 = recalculate_weight(weight02, gradient_weight02)
