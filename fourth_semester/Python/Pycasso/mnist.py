from tensorflow.examples.tutorials.mnist import input_data
import os
from network import *


def betternn(x, keep_prob):
    """
    Builds a network that learns to recognize digits
    :param x: input tensor of shape (N_examples, 784) as standard MNIST image is 28x28=7845
    :param keep_prob: probability for dropout layer
    :return: y - a tensor of shape (N_examples, 10) with
    values equal to probabilities of example being given digit
    """
    # input image is stored as 784 pixels, reshape it to (28,28,1) as it's greyscale
    # -1 is special value that indicates that this dimension should be inferred to keep
    # constant size
    net = Network(tf.reshape(x, [-1, 28, 28, 1]))
    net.add_layer(
        # take 5x5 features and create 32 feature maps
        ConvLayer([5, 5, 1, 32], [32], tf.nn.relu)
    ).add_layer(
        # reduce size by factor of 2
        PoolLayer()
    ).add_layer(
        # this time create 64 feature maps
        ConvLayer([5, 5, 32, 64], [64], tf.nn.relu)
    ).add_layer(
        # reduce size again
        PoolLayer()
    ).reshape_output(
        # reduced size twice (so image is [28,28] -> [7,7]) and created 64 feature maps
        # so flatten previous output
        [-1, 7 * 7 * 64]
    ).add_layer(
        # create 1024 features
        FullyConnectedLayer([7 * 7 * 64, 1024], [1024], tf.nn.relu)
    ).add_layer(
        # reduce complexity
        DropoutLayer(keep_prob)
    ).add_layer(
        # Map 1024 features to 10 classes representing digits
        FullyConnectedLayer([1024, 10], [10], tf.nn.softmax)
    )
    return net.output


def main(_):
    # Import data
    mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)

    # Create the model
    x = tf.placeholder(tf.float32, [None, 784], name="x")

    # Define loss and optimizer
    y_ = tf.placeholder(tf.float32, [None, 10], name="y_")

    keep_prob = tf.placeholder(tf.float32, name="keep_prob")

    # Build the graph for the deep net
    y_conv = betternn(x, keep_prob)

    cross_entropy = tf.reduce_mean(
        tf.nn.softmax_cross_entropy_with_logits(labels=y_, logits=y_conv))
    train_step = tf.train.AdamOptimizer(1e-4).minimize(cross_entropy)
    correct_prediction = tf.equal(tf.argmax(y_conv, 1), tf.argmax(y_, 1))
    accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32), name="accuracy")

    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())
        for i in range(20000):
            batch = mnist.train.next_batch(50)
            if i % 100 == 0:
                train_accuracy = accuracy.eval(feed_dict={
                    x: batch[0], y_: batch[1], keep_prob: 1.0})
                print('step %d, training accuracy %g' % (i, train_accuracy))
            train_step.run(feed_dict={x: batch[0], y_: batch[1], keep_prob: 0.5})

        print('Done training. Test accuracy: %g' % accuracy.eval(feed_dict={
            x: mnist.test.images, y_: mnist.test.labels, keep_prob: 1.0}))

        tf.add_to_collection('acc', accuracy)
        tf.add_to_collection('conv', y_conv)
        export_path = os.path.join(os.getcwd(), "saved", "model")
        print("Saving to ", export_path)
        saver = tf.train.Saver()
        saver.save(sess, export_path)


if __name__ == '__main__':
    tf.app.run(main=main)
