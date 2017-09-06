#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Ivan
# Date: 2017-08-27
import numpy as np
import tensorflow as tf
import time
import matplotlib.pyplot as plt
from tensorflow.examples.tutorials.mnist import input_data


# Using Tensorflow's default tools to fetch data, this is the same as what we did in the first homework assignment.
mnist = input_data.read_data_sets('./mnist', one_hot=True) 

# Random seed.
rseed = 42
batch_size = 200
lr = 1e-1
num_epochs = 20
num_hiddens = 500
num_train, num_feats = mnist.train.images.shape
num_test = mnist.test.images.shape[0]
num_classes = mnist.train.labels.shape[1]

# Placeholders that should be filled with training pairs (x, y). Use None to unspecify the first dimension 
# for flexibility.
x = tf.placeholder(tf.float32, [None, num_feats], name="x")
y = tf.placeholder(tf.int32, [None, num_classes], name="y")

# Model weights initialization.
# Your code here.

# logits is the log-probablity of each classes, forward computation.
# Your code here.

# Use TensorFlow's default implementation to compute the cross-entropy loss of classification.
# Your code here.

# Build prediction function.
# Your code here.

# Use TensorFlow's default implementation for optimziation algorithm. 
# Your code here.

# Start training!
num_batches = num_train / batch_size
losses = []
train_accs, valid_accs = [], []
time_start = time.time()
with tf.Session() as sess:
    # Before evaluating the graph, we should initialize all the variables.
    sess.run(tf.global_variables_initializer())
    for i in xrange(num_epochs):
        # Each training epoch contains num_batches of parameter updates.
        total_loss = 0.0
        for _ in xrange(num_batches):
            # Fetch next mini-batch of data using TensorFlow's default method.
            x_batch, y_batch = mnist.train.next_batch(batch_size)
            # Note that we also need to include optimizer into the list in order to update parameters, but we 
            # don't need the return value of optimizer.
            _, loss_batch = sess.run(# your code here.)
            total_loss += loss_batch
        # Compute training set and validation set accuracy after each epoch.
        train_acc = # your code here.
        valid_acc = # your code here.
        losses.append(total_loss)
        train_accs.append(train_acc)
        valid_accs.append(valid_acc)
        print "Number of iteration: {}, total_loss = {}, train accuracy = {}, validation accuracy = {}".format(i, total_loss, train_acc, valid_acc)
    # Evaluate the test set accuracy at the end.
    test_acc = # your code here.
time_end = time.time()
print "Time used for training = {} seconds.".format(time_end - time_start)
print "MNIST image classification accuracy on test set = {}".format(test_acc)

# Plot the losses during training.
plt.figure()
plt.title("MLP-784-500-10 with TensorFlow")
plt.plot(losses, "b-o", linewidth=2)
plt.grid(True)
plt.xlabel("Iteration")
plt.ylabel("Cross-entropy")
plt.show()
