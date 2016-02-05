#############################
# < Your Name Here：Minxiang Pan >
# STAT W4240 
# Homework 05
# < Homework Due Date >
#
#############################
#################
# Question 2 (a)
#################
#make a data frame
d=data.frame(x1=c(3,2,4,1,2,4,4),x2=c(4,2,4,4,1,3,1),Color=c("red", "red", "red", "red", "blue", "blue", "blue"))
#plot the observation
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5))
#################
# Question 2 (b)
#################
#draw the hyperplane
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
legend("topleft",c("hyperplane"),lty=c(1),bty="n",cex=0.9)
#################
# Question 2 (d)
#################
#plot the margin
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
k=1
b1=4-4
b2=3-4
#use the solid line to indicate the hyperplane and use dash lines to indicate the margin
abline(-0.5, k)
abline(b1, k, lty = 2)
abline(b2, k, lty = 2)
legend("topleft",c("hyperplane","margin"),lty=c(1,2),bty="n",cex=0.9)
#################
# Question 2 (e)
#################
#use the solid line to indicate the hyperplane and use dash lines to indicate the margin
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1)
abline(-0.5, k)
abline(b1, k, lty = 2)
abline(b2, k, lty = 2)
# use arrows to indicate support vectors
arrows(4,4,4.25,3.75);arrows(2,2,2.25,1.75);arrows(2,1,1.75,1.25);arrows(4,3,3.75,3.25)
legend("topleft",c("hyperplane","margin"),lty=c(1,2),bty="n",cex=0.9)
#################
# Question 2 (g)
#################
# we change the slope
plot(d$x1, d$x2, col=as.vector(d$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5));abline(-0.5,1.1)
legend("topleft",c("hyperplane"),lty=c(1),bty="n",cex=0.9)
#################
# Question 2 (h)
#################
#add an observation
d1=data.frame(x1=c(3,2,4,1,2,4,4,3),x2=c(4,2,4,4,1,3,1,1),Color=c("red", "red", "red", "red", "blue", "blue", "blue","red"))
#plot the observations
plot(d1$x1, d1$x2, col=as.vector(d1$Color),xlab="x1",ylab="x2",xlim=c(0,5),ylim=c(0,5))