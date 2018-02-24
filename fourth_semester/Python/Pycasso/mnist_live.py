from paint import Paint
import threading
from PIL import Image
import numpy as np
import time
import os
import tensorflow as tf


def th1(img_ref, data_ref, done_ref):
    with tf.Session() as sess:
        new_saver = tf.train.import_meta_graph(os.path.join("saved", "model.meta"))
        graph = tf.get_default_graph()
        new_saver.restore(sess, os.path.join("saved", "model"))

        x = graph.get_operation_by_name("x").outputs[0]
        y_ = graph.get_operation_by_name("y_").outputs[0]
        keep_prob = graph.get_operation_by_name("keep_prob").outputs[0]
        conv = tf.get_collection("conv")[0]

        while not done_ref[0]:
            mnist_like_image = [np.array(list(img_ref.resize([28, 28]).tobytes()), dtype='float32')]
            res = conv.eval(feed_dict={
                x: mnist_like_image, y_: [[0 for x in range(10)]], keep_prob: 1.0})
            data_ref[:] = []
            data_ref.extend(res[0])
            time.sleep(0.1)


if __name__ == '__main__':
    img = Image.new("P", [600, 600])
    data = [0.1 for x in range(0, 10)]
    done = [False]
    t = threading.Thread(target=th1, args=(img, data, done))
    t.start()
    ge = Paint(img, data)
    done[0] = True
