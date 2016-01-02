'''Trains a simple convnet on the MNIST dataset.
If implemented correctly, it is easy to get over 99.2% for testing accuracy
after a few epochs. And thre is still a lot of margin for parameter tuning,
e.g. different initializations, decreasing learning rates when the accuracy
stops increasing, or using model ensembling techniques, etc.
'''

from __future__ import print_function
import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras import backend as K
import keras.utils as np_utils
from keras.optimizers import SGD

batch_size = 128
num_classes = 10
epochs = 20

# input image dimensions
img_rows, img_cols = 28, 28

# the data, shuffled and split between train and test sets
(x_train, y_train), (x_test, y_test) = mnist.load_data()

if K.image_data_format() == 'channels_first':
    x_train = x_train.reshape(x_train.shape[0], 1, img_rows, img_cols)
    x_test = x_test.reshape(x_test.shape[0], 1, img_rows, img_cols)
    input_shape = (1, img_rows, img_cols)
else:
    x_train = x_train.reshape(x_train.shape[0], img_rows, img_cols, 1)
    x_test = x_test.reshape(x_test.shape[0], img_rows, img_cols, 1)
    input_shape = (img_rows, img_cols, 1)

# In order to speed up the convergence, we may normalize the input values
# so that they are in the range of (0, 1) for (-1, 1)
# Your code here.
x_train =x_train /255
x_test =x_test/255
#print(x_train[0])
print('x_train shape:', x_train.shape)
print(x_train.shape[0], 'train samples')
print(x_test.shape[0], 'test samples')



# Convert class vectors to binary class matrices, e.g. "1" ==> [0,1,0,0,0,0,0,0,0,0]
y_train = np_utils.to_categorical(y_train,num_classes)
y_test = np_utils.to_categorical(y_test,num_classes)

model = Sequential()

# Please build up a simple ConvNets by stacking a few conovlutioanl layers (kenel size with 3x3
# is a good choice, don't foget using non-linear activations for convolutional layers),
# max-pooling layers, dropout layers and dense/fully-connected layers.
# Your code here.
# model.add(...)
# model.add(...)
model.add(Conv2D(32,(3,3),input_shape=(28,28,1),activation='relu'))
model.add(Conv2D(32,(3,3),activation='relu'))
model.add(MaxPooling2D(pool_size=(2,2)))
model.add(Dropout(0.25))
model.add(Conv2D(64,(3,3),activation='relu'))
model.add(Conv2D(64,(3,3),activation='relu'))
model.add(MaxPooling2D(pool_size=(2,2)))
model.add(Dropout(0.25))
model.add(Flatten())
model.add(Dense(256,activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(num_classes, activation='softmax'))

# complete the loss and optimizer
# Hints: use the cross-entropy loss, optimizer could be SGD or Adam, RMSProp, etc.
# Feel free to try different hyper-parameters.
sgd=SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
model.compile(loss='categorical_crossentropy',
              optimizer=sgd,
              metrics=['accuracy'])

# Extra Points 1: use data augmentation in the progress of model training.
# Note that data augmentation is a practical technique for reducing overfitting.
# Hints: you may refer to https://keras.io/preprocessing/image/
# Your code here
# x_train = data_augment(...)



# Extra Points 2: use K-Fold cross-validation for ensembling k models,
# i.e. (1) split the whole training data into K folds;
#      (2) train K models based on different training data;
#      (3) when evaludating the testing data, averaging over K model predictions as final output.
# The codes may look like:
#   for i in range(K):
#       x_train, y_train = ...
#       model_i = train(x_train , y_train)


model.fit(x_train, y_train,
          batch_size=batch_size,
          epochs=epochs,
          verbose=1,
          validation_data=(x_test, y_test))
score = model.evaluate(x_test, y_test, batch_size=batch_size,verbose=0)
print('Test loss:', score[0])
print('Test accuracy:', score[1])
