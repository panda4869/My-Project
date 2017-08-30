import argparse
import time
import numpy as np
import matplotlib.pyplot as plt
from tensorflow.examples.tutorials.mnist import input_data

mnist=input_data.read_data_sets('mnist/',one_hot=True)
num_train,num_feat=mnist.train.images.shape
num_class=mnist.train.labels.shape[1]



# Set hyperparameters of MLP.
rseed = 42
batch_size = 200
lr = 1e-1
num_hiddens = 500
num_epochs = 20

#init global parameter
u1=np.sqrt(6.0)/np.sqrt(num_feat+num_hiddens)
w1=np.random.uniform(-u1,u1,(num_feat,num_hiddens))
b1=np.zeros(num_hiddens)
u2=np.sqrt(6.0)/np.sqrt(num_hiddens+num_class)
w2=np.random.uniform(-u2,u2,(num_hiddens,num_class))
b2=np.zeros(num_class)

#init derivative
dw1=np.zeros((num_feat,num_hiddens))
db1=np.zeros(num_hiddens)
dw2=np.zeros((num_hiddens,num_class))
db2=np.zeros(num_class)

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
#print(P)

time_start=time.time()
train_accs,valid_accs=[],[]
num_batches=int(num_train/batch_size)
for i in range(num_epochs):

    for j in range(num_batches):
        inits=mnist.train.images[j*batch_size:(j+1)*batch_size,:]
        labels=mnist.train.labels[j*batch_size:(j+1)*batch_size,:]
        #forward
        (H1,H2),P=forward(inits)
        #backward
        backward(P,labels,inits,H1,H2)

        w1-=lr*dw1
        b1-=lr*db1
        w2-=lr*dw2
        b2-=lr*db2
    train_acc=evaluate(mnist.train.images,mnist.train.labels)
    valid_acc=evaluate(mnist.validation.images,mnist.validation.labels)
    print ("Number of iteration: {}, classification accuracy on training set = {}, classification accuracy on validation set: {}".format(i, train_acc, valid_acc))
    train_accs.append(train_acc);valid_accs.append(valid_acc)

time_end=time.time()
test_acc=evaluate(mnist.test.images,mnist.test.labels)
print('Final classification accuracy on test set: %.4f'%test_acc)
print('Time used to train the model: %.4f'%(time_end-time_start))
#print(train_accs)
plt.figure()
plt.plot(train_accs,'bo-',label='Training Accuracy',linewidth=2)
plt.plot(valid_accs,'go-',label='Validation Accuracy',linewidth=2)
plt.legend(loc='best')
plt.grid(True)
plt.show()

