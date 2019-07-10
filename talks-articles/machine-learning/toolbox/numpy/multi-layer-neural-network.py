from numpy import exp, array, random, dot


class NeuronLayer():
    def __init__(self, number_of_neurons, number_of_inputs_per_neuron):
        self.synaptic_weights = 2 * random.random((number_of_inputs_per_neuron, number_of_neurons)) - 1


class NeuralNetwork():
    def __init__(self, neural_layers):
        self.neural_layers = neural_layers
        self.neural_layers_output = []

    # The Sigmoid function, which describes an S shaped curve.
    # We pass the weighted sum of the inputs through this function to
    # normalise them between 0 and 1.
    def __sigmoid(self, x):
        return 1 / (1 + exp(-x))

    # The derivative of the Sigmoid function.
    # This is the gradient of the Sigmoid curve.
    # It indicates how confident we are about the existing weight.
    def __sigmoid_derivative(self, x):
        return x * (1 - x)

    # We train the neural network through a process of trial and error.
    # Adjusting the synaptic weights each time.
    def train(self, training_set_inputs, training_set_outputs, number_of_training_iterations):
        for iteration in range(number_of_training_iterations):
            self.think(training_set_inputs, True)
            neural_layer_length = len(self.neural_layers)
            layer_error = layer_delta = layer_adjustment = None
            index = neural_layer_length - 1
            while index > -1:
                layer = self.neural_layers[index]
                if index == 0:
                    layer_error = layer_delta.dot(self.neural_layers[index+1].synaptic_weights.T)
                else:
                    layer_error = training_set_outputs - self.neural_layers_output[index]
                layer_delta = layer_error * self.__sigmoid_derivative(self.neural_layers_output[index])
                if index == 0:
                    layer_adjustment = training_set_inputs.T.dot(layer_delta)
                else:
                    layer_adjustment = self.neural_layers_output[index-1].T.dot(layer_delta)
                self.neural_layers[index].synaptic_weights += layer_adjustment
                index -= 1

    # The neural network thinks.
    def think(self, training_set_inputs, training=False):
        outputs = []
        for i, layer in enumerate(self.neural_layers):
            output_from_layer = self.__sigmoid(dot(training_set_inputs, self.neural_layers[i].synaptic_weights))
            outputs.append(output_from_layer)
            training_set_inputs = output_from_layer
        if training:
            self.neural_layers_output = outputs
        else:
            return outputs


    # The neural network prints its weights
    def print_weights(self):
        for i, layer in enumerate(self.neural_layers):
            print("    Layer %s: " % (i))
            print(layer.synaptic_weights)
        print("=====================================")


if __name__ == "__main__":
    #Seed the random number generator
    random.seed(1)

    neural_layers = [
        NeuronLayer(4, 3), # Create layer Layer=1 (4 neurons, each with 3 inputs)
        #NeuronLayer(9, 4), # Create layer Output-4 (9 neurons, each with 3 inputs)
        #NeuronLayer(7, 9), # Create layer Output-3 (7 neurons, each with 9 inputs)
        #NeuronLayer(4, 7), # Create layer Output-2 (4 neurons, each with 7 inputs)
        NeuronLayer(3, 4), # Create layer Output-1 (3 neurons, each with 4 inputs)
        #NeuronLayer(1, 3)  # Create layer Output   (a single neuron with 3 inputs)
    ]

    # Combine the layers to create a neural network
    neural_network = NeuralNetwork(neural_layers)

    print("Stage 1) Random starting synaptic weights: ")
    neural_network.print_weights()

    # The training set. We have 7 examples, each consisting of 3 input values
    # and 1 output value.
    training_set_inputs = array([
        [0, 0, 1], [0, 1, 1], [1, 0, 1], [0, 1, 0], [1, 0, 0],
        [1, 1, 1], [0, 0, 1], [0, 1, 1], [1, 0, 1], [0, 1, 0],
        [1, 1, 1], [0, 0, 1], [0, 1, 1], [1, 0, 1], [0, 1, 0],
        [1, 0, 0], [1, 1, 1], [0, 0, 0]
    ])
    training_set_outputs = array([
        [0, 1, 1, 1, 1,
         0, 0, 1, 1, 1,
         0, 0, 1, 1, 1,
         1, 0, 0]
    ]).T

    # Train the neural network using the training set.
    # Do it 60,000 times and make small adjustments each time.
    neural_network.train(training_set_inputs, training_set_outputs, 60000)

    print("Stage 2) New synaptic weights after training: ")
    neural_network.print_weights()

    # Test the neural network with a new situation.
    print("Stage 3) Considering a new situation [1, 1, 0] -> ?: ")
    outputs = neural_network.think(array([1, 1, 0]))
    print(outputs[-1])
    # Test the neural network with a new situation.
    print("Stage 4) Considering a new situation [1, 0, 0] -> ?: ")
    outputs = neural_network.think(array([1, 0, 0]))
    print(outputs[-1])
