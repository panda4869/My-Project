#1#
#EM algorithm#
#import the data#
H<-matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
#create the EM function#
MultinomialEM<-function(H,k,tau){
 n<-dim(H)[1] 
 d<-dim(H)[2]
 c<-rep(1/k,k)
 #randomly choose the centroid#
 set.seed(1)
 ind<-sample(1:n,k)
 centro<-H[ind,]
 det=1
 #initial the matrix A#
 A.old<-matrix(rep(1/k,k),n,k)
 centro<-centro/apply(centro,1,sum)+0.01
  while(det>tau){
  #E-step#
  phi<-exp(H %*% t(log(centro)))
  A<-matrix(rep(c*phi/apply(c*phi,1,sum),k),n,k)
  #M-step#
  c.updata<-rep(apply(A,2,sum)/n,k)
  b<-t(A) %*% H
  centro<-matrix(rep(b/apply(b,1,sum),d),k,d)
  #calculate the change#
  det<-norm(A-A.old,"O")
  A.old<-A
  }
 m<-vector()
 #find the most possible class#
  m<-apply(A,1,which.max)
  return(m)
  
}
#to avoid log(0)#
H1<-H+0.01
#k=3#
cl1<-vector()
cl1<-MultinomialEM(H1,3,0.1)
cl.mat1<-matrix(cl1,nrow=200,byrow=T)
# we have to rotate the image 90 degree clockwise#
im1<-NULL
for(i in 0:( dim(cl.mat1 )[1] -1)){
  im1 <-cbind (im1,cl.mat1[dim (cl.mat1)[1]-i ,])
}


#k=4#
cl2<-vector()
cl2<-MultinomialEM(H1,4,0.1)
cl.mat2<-matrix(cl2,nrow=200,byrow=T)
# we have to rotate the image 90 degree clockwise#
im2<-NULL
for(i in 0:(dim(cl.mat2 )[1] -1)){
  im2 <-cbind (im2,cl.mat2[dim (cl.mat2)[1]-i,])
}


#k=5#
cl3<-vector()
cl3<-MultinomialEM(H1,5,0.1)
cl.mat3<-matrix(cl3,nrow=200,byrow=T)
# we have to rotate the image 90 degree clockwise#
im3<-NULL
for(i in 0:(dim(cl.mat3 )[1] -1)){
  im3<-cbind (im3,cl.mat3[dim (cl.mat3)[1]-i,])
}
#plot the image#
par(mfrow=c(1,3))
image (x=1:200 ,y=1:200 , im1, axes =FALSE,xlab="",ylab="",main="k=3")
image (x=1:200 ,y=1:200 , im2, axes =FALSE,xlab="",ylab="",main="k=4")
image (x=1:200,y=1:200,im3, axes =FALSE,xlab="",ylab="",main="k=5")