#code for q1(a)
p1<-read.csv("hw02_q1_p1_fall14.csv")
apply(p1,1,mean)
apply(p1,2,mean)
#code for q1(b)
a=scale(p1,center=T,scale=F)
cov(a)
#code for q1(c)
eigen(cov(a))
#code for q1(d)
pr.out=prcomp(p1,center=T,scale=T)
names(pr.out)
pr.out$rotation
pr.out$x
dim(pr.out$x)
#code for q1(e)
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
#code for q1(f)
pr.a=princomp(p1,center=T,scale=T,cor=T)
p2<-read.csv("hw02_q1_p2_fall14.csv")
pp=data.matrix(p2)
pp1=scale(pp,center=T,scale=T)
s=pp1%*%pr.a$loadings
s
#code for q1(g)
M=pr.a$loadings[,1:2]
pp2=(pp1%*%M)%*%t(M)
c=rbind(pp1,pp2)
dist1=dist(c,method="euclidean")
dist1=data.matrix(dist1)
a=dist1[6:10, 1:5]
diag(a)
#code for q1(h)
d=pp2-pp1
d

#code for q2(a)
# Refer to the method in the HW01
library(pixmap)
pic_list = 1:38
view_list = c('P00A+000E+00','P00A+005E+10', 'P00A+005E-10','P00A+010E+00')
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
face_vector=matrix(nrow=152,ncol=32256)
for (i in 1:length(pic_list)){ 
  for(j in 1:length(view_list)){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]] , view_list[j])
    face=read.pnm(file=filename)
    face_matrix=getChannels(face)
    face_vector[4*(i-1)+j,]=as.vector(face_matrix)
  }
  
}
#code for q2(b)
meanface_matrix=apply(face_vector,2,mean)
dim(meanface_matrix)=c(192,168)
fix(meanface_matrix)
meanface= pixmapGrey(meanface_matrix)
plot(meanface)
title('Meanface')
filename = 'Meanface.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#code for q2(c)

image=prcomp(face_vector,center=T)
image.var=image$sdev^2
pve=image.var/sum(image.var)
plot(pve,xlab="Principal Components",ylab="Cumulative Proportion of Variance explained")

#code for q2(d)
pic_data = vector("list",9)
eigenfaces_matrix = vector()
for(i in 1:3){
  row_matrix=vector()
  for(j in 1:3){
    eigenfaces=t(image$rotation[,3*(i-1)+j])
    dim(eigenfaces)=c(192,168)
    row_matrix=cbind(row_matrix,eigenfaces)
  }
  eigenfaces_matrix=rbind(eigenfaces_matrix,row_matrix)
}
eigenfaces1=pixmapGrey(eigenfaces_matrix)
title("Eigenfaces")

#code for q2(e)
#add additional pictures 1 at a time
#initialize the matrix
yaleB01=read.pnm(file="CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm")
yaleB01_vector=as.vector(getChannels(yaleB01))
vector_matrix=yaleB01_vector-apply(face_vector,2,mean)
M=matrix(vector_matrix,nrow=1)
pic_data = vector("list",25)
adjust_faces_matrix=vector()
adjust_faces=apply(face_vector,2,mean)
#create loops to combind pictures
for(i in 1:5){
  row_matrix=vector()
  for(j in 1:5){
    #add one victor at a time
      m=(M%*%image$rotation[,5*(i-1)+j])%*%t(image$rotation[,5*(i-1)+j])
      m=as.vector(m)
      adjust_faces=adjust_faces+m
   #transfer the vector back to the matrix
     dim(adjust_faces)=c(192,168)
    row_matrix=cbind(row_matrix,adjust_faces)
    adjust_faces=as.vector(adjust_faces)
  }
  adjust_faces_matrix=rbind(adjust_faces_matrix,row_matrix)
}
adjust_faces1=pixmapGrey(adjust_faces_matrix)
plot(adjust_faces1)
title("Using 24 Eigenfaces")


vector_matrix=yaleB01_vector-apply(face_vector,2,mean)
M=matrix(vector_matrix,nrow=1)
pic_data = vector("list",25)
adjust_faces_matrix=vector()
#start with the mean face
adjust_faces=apply(face_vector,2,mean)
for(i in 1:5){
  row_matrix=vector()
  for(j in 1:5){
# use three loops to add 5 eigenfaces at a time
    for(k in 1:5){
      m=(M%*%image$rotation[,25*(i-1)+5*(j-1)+k])%*%t(image$rotation[,25*(i-1)+5*(j-1)+k])
      m=as.vector(m)
      adjust_faces=adjust_faces+m}
    dim(adjust_faces)=c(192,168)
    row_matrix=cbind(row_matrix,adjust_faces)
    adjust_faces=as.vector(adjust_faces)
  }
  adjust_faces_matrix=rbind(adjust_faces_matrix,row_matrix)
}
adjust_faces2=pixmapGrey(adjust_faces_matrix)
plot(adjust_faces2)
title("Using 24 Eigenfaces")
#code for q2 (f)
#face to be subtracted
face05=read.pnm(file="CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")
plot(face05)
face_vector1=face_vector[-c(17,18,19,20),]
dim(face_vector1)
image1=prcomp(face_vector1,center=T)
face05_vector=as.vector(getChannels(face05))
vector_matrix=face05_vector-apply(face_vector,2,mean)
pic_data = vector("list",25)
adjust_faces_matrix=vector()
face05_adjust=(face05_vector%*%image1$rotation)%*%t(image1$rotation)+apply(face_vector,2,mean)
dim(face05_adjust)=c(192,168)
adjust_faces3=pixmapGrey(face05_adjust)
plot(adjust_faces3)
title("Reconstructed Face")