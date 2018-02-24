import time
from PIL import Image
import numpy as np
import os
from keras import backend
from keras.applications.vgg16 import VGG16

from scipy.optimize import fmin_l_bfgs_b

MEAN_PIXEL_VALUE = [103.939, 116.779, 123.68]  # based on ImageNet
MAX_SIZE = 512

class Net:
    class Evaluator(object):
        """
        Cache result of function as loss and grads need to be separate functions
        """

        def __init__(self, objective, size):
            self.loss_value = None
            self.grad_values = None
            self.objective = objective
            self.size = size

        def eval_loss_and_grads(self, x):
            outs = self.objective([x.reshape((1, *self.size, 3))])
            loss_value = outs[0]
            grad_values = outs[1].flatten().astype('float64')
            return loss_value, grad_values

        def loss(self, x):
            assert self.loss_value is None  # previous result was deleted
            self.loss_value, self.grad_values = self.eval_loss_and_grads(x)
            return self.loss_value

        def grads(self, _):
            assert self.loss_value is not None  # use cached result
            grad_values = np.copy(self.grad_values)
            self.loss_value = None
            self.grad_values = None  # clear cache
            return grad_values

    def __init__(self, ratio, iterations, content_img, style_img):
        self.iterations = iterations
        content_img = crop_to_square(content_img)
        style_img = crop_to_square(style_img)
        self.size = content_img.size
        content_arr = image_to_array(content_img, self.size)
        style_arr = image_to_array(style_img, self.size)

        content_image = backend.variable(content_arr)
        style_image = backend.variable(style_arr)
        self.combination_image = backend.placeholder((1, *self.size, 3))
        input_tensor = backend.concatenate([content_image,
                                            style_image,
                                            self.combination_image], axis=0)
        self.model = VGG16(weights='imagenet', include_top=False, input_tensor=input_tensor)
        self.feature_layers = ['block1_conv2', 'block2_conv2',
                               'block3_conv3', 'block4_conv3',
                               'block5_conv3']  # described in paper
        self.content_weight = 0.025
        self.style_weight = self.content_weight * ratio / len(self.feature_layers)

    def mix_images(self):
        layers = dict([(layer.name, layer.output) for layer in self.model.layers])
        loss = backend.variable(0.)

        layer_features = layers['block2_conv2']
        content_image_features = layer_features[0]
        combination_features = layer_features[2]

        loss += self.content_weight * content_loss(content_image_features,
                                                   combination_features)

        for layer_name in self.feature_layers:
            layer_features = layers[layer_name]
            style_features = layer_features[1]
            combination_features = layer_features[2]
            loss += self.style_weight * style_loss(style_features, combination_features, *self.size)

        loss += total_variation_loss(self.combination_image, *self.size)

        grads = backend.gradients(loss, self.combination_image)
        loss_and_grads = [loss]
        loss_and_grads += grads
        objective_function = backend.function([self.combination_image], loss_and_grads)
        evaluator = Net.Evaluator(objective_function, self.size)
        x = np.random.uniform(-128, 127, (1, *self.size, 3))

        for i in range(self.iterations):
            print('Start of iteration', i)
            start_time = time.time()
            x, min_val, info = fmin_l_bfgs_b(evaluator.loss, x.flatten(),
                                             fprime=evaluator.grads, maxfun=20)
            print('Current loss value:', min_val)
            end_time = time.time()
            print('Iteration %d completed in %ds' % (i, end_time - start_time))

        x = x.reshape((*self.size, 3))
        x = np.flip(x, 2)  # back to rgb
        x += MEAN_PIXEL_VALUE
        x = np.clip(x, 0, 255).astype('uint8')  # remove distortion
        im = Image.fromarray(x)
        return im


def open_image_as_array(path, size):
    im = Image.open(os.path.join(*path)).resize(size)
    imarray = np.asarray(im, dtype='float32')
    imarray = np.expand_dims(imarray, 0)
    imarray -= MEAN_PIXEL_VALUE
    # in paper there is BGR pixel order for some reason
    return np.flip(imarray, 3)


def image_to_array(image, size):
    im = image.resize(size)
    imarray = np.asarray(im, dtype='float32')
    imarray = np.expand_dims(imarray, 0)
    imarray -= MEAN_PIXEL_VALUE
    # in paper there is BGR pixel order for some reason
    return np.flip(imarray, 3)


def content_loss(content, combination):
    return backend.sum(backend.square(combination - content))


def gram_matrix(x):
    # G(A) = A*A
    # where A* is conjugate transpose which is equal to transpose if matrix values are real
    features = backend.batch_flatten(backend.permute_dimensions(x, (2, 0, 1)))
    return backend.dot(features, backend.transpose(features))


def style_loss(style, combination, width, height):
    # Frobenius norm of difference of gram matrices, squared
    s = gram_matrix(style)
    c = gram_matrix(combination)
    channels = 3
    imsize = width * height
    return backend.sum(backend.square(s - c)) / (4. * (channels ** 2) * (imsize ** 2))


def total_variation_loss(x, width, height):
    a = backend.square(x[:, :height - 1, :width - 1, :] - x[:, 1:, :width - 1, :])
    b = backend.square(x[:, :height - 1, :width - 1, :] - x[:, :height - 1, 1:, :])
    return backend.sum(backend.pow(a + b, 1.25))


def crop_to_square(image):
    size = min(*image.size)
    if size > MAX_SIZE:
        size = MAX_SIZE
        if image.size[1] < image.size[0]:
            w = int(image.size[0]/image.size[1] * MAX_SIZE)
            image = image.resize([w, MAX_SIZE])
        else:
            h = int(image.size[1] / image.size[0] * MAX_SIZE)
            image = image.resize([MAX_SIZE, h])

    w, h = image.size
    return image.crop(((w - size) // 2, (h - size) // 2, (w + size) // 2, (h + size) // 2))
