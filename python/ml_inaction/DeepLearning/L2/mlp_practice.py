import numpy as np 
import matplotlib.pyplot as plt 
import tensorflow as tf 
from tensorflow.examples.tutorials.mnist import input_data
import time

mnist=input_data.read_data_sets('./mnist',one_hot=True)

rseed=42
lr=1e-1
num_hidden=500
num_epoch=20
batch_size=200
num_train,num_feat=mnist.train.images.shape
num_test=mnist.test.images.shape[0]
num_class=mnist.train.labels.shape[1]

## initialize parameter
x=tf.placeholder(tf.float32,[None,num_feat],name='x')
y=tf.placeholder(tf.int32,[None,num_class],name='y')


u1=np.sqrt(6.0)/np.sqrt(num_feat+num_hidden)
u2=np.sqrt(6.0)/np.sqrt(num_hidden+num_class)
weights={'h':tf.Variable(tf.random_uniform(shape=[num_feat,num_hidden],minval=-u1,maxval=u1),name='h1'),'o':tf.Variable(tf.random_uniform(shape=[num_hidden,num_class],minval=-u2,maxval=u2),name='h2')}
bias={'h':tf.Variable(tf.zeros([num_hidden],name='b1')),'o': tf.Variable(tf.zeros([num_class],name='b2'))}


def forward_nn(x,weights,bias):
    hidden_layer=tf.add(tf.matmul(x,weights['h']),bias['h'])
    hidden_layer=tf.nn.relu(hidden_layer)
    output_layer=tf.add(tf.matmul(hidden_layer,weights['o']),bias['o'])

    return output_layer

def nn_prection(logits):
    preds=tf.nn.softmax(logits)
    return preds


def nn_loss(logits,y):
    cross_entropy=tf.nn.softmax_cross_entropy_with_logits(logits=logits,labels=y,name='cross_entropy')
    loss=tf.reduce_mean(cross_entropy,name='loss')
    return loss
def nn_accuracy(logits,y):
    preds=nn_prection(logits)
    correct_predict=tf.equal(tf.argmax(preds,1),tf.argmax(y,1))
    accuracy=tf.reduce_mean(tf.cast(correct_predict,tf.float32))
    return accuracy

logits=forward_nn(x,weights,bias)
loss=nn_loss(logits,y)
accuracy=nn_accuracy(logits,y)
optimizer=tf.train.GradientDescentOptimizer(lr).minimize(loss)

num_batch=int(num_train/batch_size)
#test_acc,train_acc,val_acc=[],[],[]
loses=[]
time_start=time.time()
with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    for i in range(num_epoch):
        total_loss=0.0
        for _ in range(num_batch):
            x_batch,y_batch=mnist.train.next_batch(batch_size)
            _,loss_batch=sess.run([optimizer,loss],feed_dict={x:x_batch,y:y_batch})
            total_loss+=loss_batch
        train_acc=sess.run([accuracy],feed_dict={x:mnist.train.images,y:mnist.train.labels})
        val_acc=sess.run([accuracy],feed_dict={x:mnist.validation.images,y:mnist.validation.labels})
        loses.append(loss)
        print('iteration: [{}], training accuracy: {}, validation accuracy: {}'.format(i,train_acc,val_acc))
    test_acc=sess.run([accuracy],feed_dict={x:mnist.test.images,y:mnist.test.labels})
time_end=time.time()
print('Time used for training: {} Seconds'.format(time_end-time_start))
print('test_acc: {}'.format(test_acc))

plt.figure()
plt.title('Tensorflow Loss Graph')
plt.plot(loses,'b-o',linewidth=2)
plt.grid(True)
plt.xlabel('Epoches')
plt.ylabel('Loss')
plt.show()





