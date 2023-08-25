#!python3

from tvm.driver import tvmc

model = tvmc.load('my_model.onnx')  #step.1: load
# model = tvmc.load('my_model.onnx', shape_dict={'input1' : [1, 2, 3, 4], 'input2' : [1, 2, 3, 4]})

## to see Relay, can
# model.summary()

package = tvmc.compile(model, target='llvm')  #step.2: compile

result = tvmc.run(package, device='cpu')    #step.3: run
