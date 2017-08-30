#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Ivan
# Date: 2017-08-26
import argparse
import time
import numpy as np
import matplotlib.pyplot as plt
from tensorflow.examples.tutorials.mnist import input_data


# Load the MNIST dataset from the official website.
mnist = input_data.read_data_sets("mnist/", one_hot=True)

num_train, num_feats = mnist.train.images.shape
num_test = mnist.test.images.shape[0]
num_classes = mnist.train.labels.shape[1]

# Set hyperparameters of MLP.
rseed = 42
batch_size = 200
lr = 1e-1
num_hiddens = 500
num_epochs = 20

# Initialize model parameters, sample W ~ [-U, U], where U = sqrt(6.0 / (fan_in + fan_out)).
np.random.seed(rseed)
# Your code here to create model parameters globally.
#input to hidden
u1=np.sqrt(6.0/(num_feats+num_hiddens))
w1=np.random.uniform(-u1,u1,(num_feats,num_hiddens))
b1=np.zeros(num_hiddens)
# hidden to output
u2=np.sqrt(6.0/(num_hiddens+num_classes))
w2=np.random.uniform(-u2,u2,(num_hiddens,num_classes))
b2=np.zeros(num_classes)


# Used to store the gradients of model parameters.
dw1 = np.zeros((num_feats, num_hiddens))
db1 = np.zeros(num_hiddens)
dw2 = np.zeros((num_hiddens, num_classes))
db2 = np.zeros(num_classes)

# Helper functions.
def ReLU(inputs):
    """
    Compute the ReLU: max(x, 0) nonlinearity.
    """
    # Your code here.
    inputs[inputs<0]=0
    return inputs

def softmax(inputs):
    """
    Compute the softmax nonlinear activation function.
    """
    # Your code pass
    return np.exp(inputs)/(1+np.exp(-inputs))


def forward(inputs):
    """
    Forward evaluation of the model.
    """
    # Your code here.
    h1=ReLU(inputs.dot(w1)+b1)
    h2=h1.dot(w2)+b2
    return (h1,h2),softmax(h2)


def backward(probs, labels,x, h1,h2):
    """
    Backward propagation of errors.
    """
    # Your code here.
    n=x.shape[0]
    da2=-(labels-probs)
    da1=da2.dot(w2.T)
    db2[:]=np.mean(da2,axis=0)
    dw2[:]=h1.T.dot(da2)/n
    da1[h1<0]=0.0
    db1[:]=np.mean(da1,axis=0)
    dw1[:]=x.T.dot(da1)/n


def predict(probs):
    """
    Make predictions based on the model probability.
    """
    # Your code here.
    return np.argmax(probs,axis=1)


def evaluate(inputs, labels):
    """
    Evaluate the accuracy of current model on (inputs, labels).
    """
    # Your code here.
    _,probs=forward(inputs)
    prediction=predict(probs)
    n=inputs.shape[0]
    #print(prediction)
    #print(prediction==predict(labels))
    return sum(prediction==predict(labels))/n

#(H1,H2),P=forward(mnist.train.images)
#backward(P,mnist.train.labels,mnist.train.images,H1,H2)
#print(evaluate(mnist.train.images,mnist.train.labels))

# Training using stochastic gradient descent.
time_start = time.time()
num_batches = int(num_train / batch_size)
train_accs, valid_accs = [], []
for i in range(num_epochs):
    for j in range(num_batches):
        # Fetch the j-th mini-batch of the data.
        insts = mnist.train.images[batch_size * j: batch_size * (j+1), :]
        labels = mnist.train.labels[batch_size * j: batch_size * (j+1), :]
        # Forward propagation.
        # Your code here.
        (H1,H2),P=forward(insts)

        # Backward propagation.
        backward(P,labels,insts,H1,H2)
        # Your code here.
        
        # Gradient update.
        w1 -= lr * dw1
        w2 -= lr * dw2
        b1 -= lr * db1
        b2 -= lr * db2
    # Evaluate on both training and validation set.
    train_acc = evaluate(mnist.train.images, mnist.train.labels)
    valid_acc = evaluate(mnist.validation.images, mnist.validation.labels)
    train_accs.append(train_acc)
    valid_accs.append(valid_acc)
    print ("Number of iteration: {}, classification accuracy on training set = {}, classification accuracy on validation set: {}".format(i, train_acc, valid_acc))
time_end = time.time()
# Compute test set accuracy.
acc = evaluate(mnist.test.images, mnist.test.labels)
print ("Final classification accuracy on test set = {}".format(acc))
print ("Time used to train the model: {} seconds.".format(time_end - time_start))

# Plot classification accuracy on both training and validation set for better visualization.
plt.figure()
plt.plot(train_accs, "bo-", linewidth=2)
plt.plot(valid_accs, "go-", linewidth=2)
plt.legend(["training accuracy", "validation accuracy"], loc=4)
plt.grid(True)
plt.show()
