
## Artificial Neural Network

```
[Biological Neurons]

 D,/'\/'\             B    D~ Dendrites; used to collect input
   \   N '\______/-/--'    N~ Nucleus; sums all inputs from 'D' & fires if aggregate is higher than threshold
   / """"   ,--A--\--/-    A~ Axon; N fires electrical impulse to B using A
   \ /^^^--/       \       B~ Boutons; are networked to X-count other Neurons via Synapse connection
    '
```

### Modelling Artificial Neurons

Artificial Neuron model are simplified over biological neurons. Artificial Neurons mostly referred to as Perceptron.

```
[Perceptron]   

         w1
      ,-(x1)--, ,_________
      |-(x2)--,\| ,- |  ,-|
inputs|-(x3)----| \  | -+-|----->
      |  :  w3 /| /_ |_/  |   output
      |  :    / '----'----'
      '-(xN)-'   sum  activation
            wN        function
```

* Perceptron weights can (de)amplify (+/-) original input signal.

* Weighted Signals are added and passed to Activation Function.
> ActivationFunc is used to convert input into usable output.
> ActivationFunc are of varied forms, simplest is `step function`. StepFunction typically outputs `1` if input is higher than a threshold, otherwise `0`.

```
[example]

 Input 1 (x1) = 0.6  || Weight 1 (w1) = 0.5
 Input 2 (x2) = 1.0  || Weight 2 (w2) = 0.8

 Threshold = 1.0

 (x1 * w1) + (x2 * w2) = (0.6 * 0.5) + (1 * 0.8) = 1.1

 here total input to Perceptron is 1.1, higher than activation threshold 1.0
 so neuron would fire
```

### Implementing ANN

Here we'll look at Feed-Forward Networks

```
connections... input-layer (i) to hidden-layer (h) to output-layer (o)

i1 to hA{1,N}; i2 to hA{1,N}; .... iM to hA{1,N}
hA1 to o{1,O}; hA2 to o{1,O}; .... hAN to o{1,O}
```

* all feeding forward to all Perceptron in next layer
* input-layer gaining input from external factors and output-layer reflecting fires
* hidden-layer could have multiple layers with similar feeding forward Perceptron in each earlier layer


#### Linear Separability

It's an approach to check if points can be separated in a n-dimensional space sing n-1 dimension

Consider 2-input patterns (x1,x2) being classified into 2 classes.
Classes separable with a single line L (decision surface) on graph are Linearly Separable Pattern.

Linear Separability refers to fact that classes of patterns with n-dimensional vector x=(x1,x2,x3...xN) can be separated with a single decision layer.

Single Layer Perceptron can categorize set of patterns into 2 classes, if two classes are lineraly separable. Like logical-OR function.

```
x1 =  0  0  1  1                        '|
x2 =  0  1  0  1                        \|
y  =  0  1  1  1      { y = x1 || x2 }   |,
                                         '-\---->
```

Classic example of Linearly In-separable pattern is logical-XOR. Logically classified with 2 decision planes.

```
x1 =  0  0  1  1                         '|  .'
x2 =  0  1  0  1                         ,|.'   /
y  =  0  1  1  0      { y = x1 (X) x2 }   |    /
                                          '---,-->
```

Thus mostly multi-layer Perceptrons are required for reliable networks.


#### Learning Types

During learning process brain's neural structure alters, increasing/decreasing strength of it's neural synapses.
More relevant and recent information will have stronger synapse, older and irrelevant synapses will start fading.

Multi-layer Perceptron can emulate it by varying weights among them.
Learning algorithm are useful for process conceptually hard for humans to turn into code.

Learning process within ANN is result of altering network weights. Objective is to find a set of weight matrices which map input to correct output.

Major learning paradigms are:

* Supervised Learning
> If desired output for network is also provided with the input while training the network.
> This enables calculating error based on it's target output and actual output.
> Then the error rate could be used to tweak weights in network.

* Unsupervised Learning
> Only provided with set of inputs. Neural Network is responsible to find pattern within inputs without external aid.
> Popular in data-mining, finding preferences mapped with similar user behavior.

* Reinforcement Learning
> Similar to supervised learning, as some feedback is given. But instead of target output a reward is given on level of correctness.
> System aims to maximize the reward through trail and error.
> Mapped to animal learning tricks to survive.


---

References:

* ANN
> * [The Project Spot](http://www.theprojectspot.com/tutorial-post/introduction-to-artificial-neural-networks-part-1/7)

* Linear Separability
> * [ECE UTEP EDU](http://www.ece.utep.edu/research/webfuzzy/docs/kk-thesis/kk-thesis-html/node19.html)
> * [AI Shack](http://aishack.in/tutorials/linear-separability/)

---
