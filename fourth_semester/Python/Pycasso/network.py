import tensorflow as tf
from functools import partial
from abc import ABC, abstractmethod


def weight_variable(shape):
    return tf.Variable(tf.truncated_normal(shape, stddev=0.1), name="weight_var")


def bias_variable(shape):
    return tf.Variable(tf.constant(0.1, shape=shape), name="bias_var")


def conv2d(x, w):
    return tf.nn.conv2d(x, w, strides=[1, 1, 1, 1], padding='SAME')


def max_pool_2x2(x):
    return tf.nn.max_pool(x, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='SAME')


class Infix(object):
    def __init__(self, func):
        self.func = func

    def __or__(self, other):
        return self.func(other)

    def __ror__(self, other):
        return Infix(partial(self.func, other))

    def __call__(self, v1, v2):
        return self.func(v1, v2)


class Layer(ABC):
    input_tensor = None
    output = None
    out_fun = None

    @abstractmethod
    def add_input(self, input_tensor):
        pass


class BiasedLayer(Layer):
    weights = []
    biases = []

    def __init__(self, weight_shape, bias_shape, out_fun):
        self.weights = weight_variable(weight_shape)
        self.biases = bias_variable(bias_shape)
        self.out_fun = out_fun


class ConvLayer(BiasedLayer):
    def add_input(self, input_tensor):
        self.input_tensor = input_tensor
        self.output = self.out_fun(conv2d(self.input_tensor, self.weights) + self.biases)


class DropoutLayer(Layer):
    def __init__(self, keep_prob):
        self.keep_prob = keep_prob

    def add_input(self, input_tensor):
        self.input_tensor = input_tensor
        self.output = tf.nn.dropout(self.input_tensor, self.keep_prob)


class FullyConnectedLayer(BiasedLayer):
    def add_input(self, input_tensor):
        self.input_tensor = input_tensor
        self.output = self.out_fun(tf.matmul(self.input_tensor, self.weights) + self.biases)


class PoolLayer(Layer):
    def add_input(self, input_tensor):
        self.input_tensor = input_tensor
        self.output = max_pool_2x2(self.input_tensor)


class Network:
    def __init__(self, input_tensor):
        self.output = self.input_tensor = input_tensor
        self.layers = []
        self.keep_prob = None

    def add_layer(self, layer):
        if isinstance(layer, DropoutLayer):
            self.keep_prob = layer.keep_prob

        self.layers.append(layer)
        layer.add_input(self.output)
        self.output = layer.output
        return self

    def reshape_output(self, new_shape):
        self.output = tf.reshape(self.output, new_shape)
        return self
