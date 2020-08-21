
## Caffe2

**===========================DEPRECATED========================**

[source](https://caffe2.ai/docs/intro-tutorial)

* Caffe2 is a Machine Learning framework enabling simple and flexible deep learning; allows utilizing CPU and GPU.

* There are 2 stages for working with Deep Learning app with Caffe2
> * create your model, which will learn from inputs and info (classifiers) about inputs and expected outputs
>
> * run finished model elsewhere, relatively lightweight

* To use locally without hassle, pull following docker providing cpu edition

```
docker pull caffe2ai/caffe2:c2v0.8.1.cpu.min.ubuntu16.04
docker run -it caffe2ai/caffe2:c2v0.8.1.cpu.min.ubuntu16.04 ipython
```

### Blobs and Workspace, Tensors

* data is organized as `blbs`, most blobs contain a `tensor` (like a multidimensional array, data point with multiple info vectors)
> * `numpy` a prereq to Caffe2 represents tensor as numpy arrays

* [workspace](https://caffe2.ai/docs/workspace.html) stores all blobs
> workspace initialize themselves when used

```
from caffe2.python import workspace, model_helper
import numpy as np

x = np.random.rand(4,3,2) ## create random tensor
print(x, x.shape)

workspace.FeedBlob("my_blob", x) ## feeding blob to workspace
x2 = workspace.FetchBlob("my_blob") ## fetching blob from workspace
print(x2)
```

### Nets and Operators

* fundamental model abstraction is a `net`, a graph of `operators`
> * net definition is stored in ProtoBufs, can be inspected by `net.Proto()`

* each `operator` takes a set of input `blobs` and produce one/more output `blob`

* example shows creations of simple model with one [fully connected](https://caffe2.ai/docs/operators-catalogue.html#fc) (FC) layer, a [sigmoid](https://caffe2.ai/docs/operators-catalogue.html#sigmoidgradient) activation with [Softmax and a CrossEntropy loss](https://caffe2.ai/docs/operators-catalogue.html#softmaxwithloss)
> other documentation for better context
>
> * [fully connected layer - FC](https://en.wikipedia.org/wiki/Convolutional_neural_network#Fully_connected)
>
> * [Sigmoid Function](https://en.wikipedia.org/wiki/Sigmoid_function)
>
> * [activation functions](https://medium.com/the-theory-of-everything/understanding-activation-functions-in-neural-networks-9491262884e0)
>
> * [Softmax Function](https://en.wikipedia.org/wiki/Softmax_function)
>
> * [CrossEntropy](https://en.wikipedia.org/wiki/Cross_entropy) loss

```
### First, prepare data
## input to model would be a mini batch of 16 samples

# Create the input data, dimension 16
data = np.random.rand(16, 100).astype(np.float32)

# Create labels for the data as integers [0, 9]; dimension 16
label = (np.random.rand(16) * 10).astype(np.int32)

## feeding data and labels blobs to workspace
## in-practice data,labels will be loaded from some datasource not prepared directly in memory as it will be really huge
workspace.FeedBlob("data", data)
workspace.FeedBlob("label", label)

## composing nets directly is tedious,
## so using model helper class, it will create 2 inter-related nets
## one that initializes parameters `init_net`, `m.param_init_net` which is a net only run once
## other that runs actual training `exec_net`, actual training was done by `m.net`
m = model_helper.ModelHelper(name="my first net")

### Second, define model; operators are not yet executed
## creating a model by defining bunch of operators FC, Sigmoid, SoftmaxWithLoss
## FC op takes input data blob, weights
## weights and bias using either XavierFill or ConstantFill will take empty array
## shape = [output, input]
fc_1 = m.net.FC(["data", "fc_w", "fc_b"], "fc1")
pred = m.net.Sigmoid(fc_1, "pred")
softmax, loss = m.net.SoftmaxWithLoss([pred, "label"], ["softmax", "loss"])

## to view created net definition
# print(m.net.Proto())
## initialization net view
# print(m.param_init_net.Proto())
```

* Primary idea of Caffe2 API is to use Python to compose a model, pass those nets to C++ code as serialized ProtoBufs to be trained.


### Executing

* to execute training of previous section composed net, do as following

```
workspace.RunNetOnce(m.param_init_net) ## this will pass ProtoBuf to C++

workspace.CreateNet(m.net) ## create actual training Net

## Run 10 * 100
for _ in range(100):
  data = np.random.rand(16, 100).astype(np.float32)
  label = (np.random.rand(16) * 10).astype(np.int32)
  workspace.FeedBlob("data", data)
  workspace.FeedBlob("label", label)
  workspace.RunNet(m.name, 10)   # run for 10 times
```

* above we don't need to pass net definition to `RunNet()` as it's already in workspace

* can inspect results stored in output blobs (contains tensors)

```
print(workspace.FetchBlob("softmax"))
print(workspace.FetchBlob("loss"))
```


### Backward pass

* this net only has forward pass, so it is not learning anything

* backward pass is created by adding gradient operators for each operator in forward pass

```
## insert before `RunNetOnce()`
m.AddGradientOperators([loss])
```

---
---
