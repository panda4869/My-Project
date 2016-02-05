# code for q4(a)
#use the same method in hw2
library(pixmap)
pic_list = 1:38
views_4a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
for (i in 1:length(pic_list)){ 
  for(j in 1:length(views_4a)){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]] , views_4a[j])
    face=read.pnm(file=filename)
    face_matrix=getChannels(face)
    if((i==1)&&(j==1)){ # Preallocate matrix to store picture vectors, store sizes for computations
      vector_length= prod(dim(face_matrix))
      face_matrix_4a = mat.or.vec(length(pic_list)*length(views_4a),vector_length)
      subject_number=rep(0,length(pic_list)*length(views_4a))
    }
    face_matrix_4a[length(views_4a)*(i-1)+j,]=as.vector(face_matrix)
    subject_number[length(views_4a)*(i-1)+j]=i
  }
  
}
subject_number=as.factor(subject_number)
face_frame_4a=data.frame(face_matrix_4a,subject_number)
dim(face_matrix_4a)
# Get the size of the matrix for use later
fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5) # Number of training obs
ntest_4a = fm_4a_size[1]-ntrain_4a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a) # Training indices
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a] # Testing indices

# to print the first 5 train_faces and test_faces
ind_train_matrix=vector()
ind_test_matrix=vector()
for(i in 1:5){
  #to show the first five training files
  train_faces=face_matrix_4a[ind_train_4a[i],]
  dim(train_faces)=c(192,168)
  ind_train_matrix=cbind(ind_train_matrix,train_faces)
  #to show the first five testing files
  test_faces=face_matrix_4a[ind_test_4a[i],]
  dim(test_faces)=c(192,168)
  ind_test_matrix=cbind(ind_test_matrix,test_faces)
}
pic_train_faces=pixmapGrey(ind_train_matrix)
pic_test_faces=pixmapGrey(ind_test_matrix)
par(mfrow=c(2,1))
plot(pic_train_faces)
title("The first five training faces")
plot(pic_test_faces)
title("The first five testing faces")

# code for q4(b)
train_matrix=mat.or.vec(length(ind_train_4a),vector_length)
for(i in 1:length(ind_train_4a)){
  #to show the first five training files
  train_matrix[i,]=face_matrix_4a[ind_train_4a[i],]
}
train_matrix_centered=train_matrix-colMeans(train_matrix)
image_train=prcomp(train_matrix_centered)
image_train_scores=train_matrix_centered%*%image_train$rotation[,1:25]

test_matrix=mat.or.vec(length(ind_test_4a),vector_length)
for(i in 1:length(ind_test_4a)){
  #to show the first five training files
  test_matrix[i,]=face_matrix_4a[ind_test_4a[i],]
}

test_matrix_centered=test_matrix-colMeans(train_matrix)
#image_test=prcomp(test_matrix_centered)
image_test_scores=test_matrix_centered%*%image_train$rotation[,1:25]
#to predict the pictures in the test_set
knn.pred=knn(image_train_scores,image_test_scores,subject_number[ind_train_4a],k=1)
#compare the predictions with the test_set
#table(knn.pred,subject_number[ind_test_4a])
#count the correct prediction
sum(diag(table(knn.pred,subject_number[ind_test_4a])))
which(subject_number[ind_test_4a]!=knn.pred)
#plot the faces
face_mis_pred=face_matrix_4a[116,]
dim(face_mis_pred)=c(192,168)
face_mis_pred_image=pixmapGrey(face_mis_pred)
KNN_photo=face_matrix_4a[ind_test_4a[27],]
dim(KNN_photo)=c(192,168)
KNN_photo_image=pixmapGrey(KNN_photo)
plot(face_mis_pred_image);title("1NN_photo_image")
plot(KNN_photo_image);title("face_mis_pred_image")

# code for q4(c)
pic_list = 1:38
views_4c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
for (i in 1:length(pic_list)){ 
  for(j in 1:length(views_4c)){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]] , views_4c[j])
    face=read.pnm(file=filename)
    face_matrix=getChannels(face)
    if((i==1)&&(j==1)){ # Preallocate matrix to store picture vectors, store sizes for computations
      vector_length= prod(dim(face_matrix))
      face_matrix_4c = mat.or.vec(length(pic_list)*length(views_4c),vector_length)
      subject_number1=rep(0,length(pic_list)*length(views_4c))
    }
    face_matrix_4c[length(views_4c)*(i-1)+j,]=as.vector(face_matrix)
    subject_number1[length(views_4c)*(i-1)+j]=i
  }
  
}
subject_number1=as.factor(subject_number1)
face_frame_4c=data.frame(face_matrix_4c,subject_number1)

fm_4c_size = dim(face_matrix_4c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4c = floor(fm_4c_size[1]*4/5)
ntest_4c = fm_4c_size[1]-ntrain_4c
set.seed(2)
ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]
train_matrixc=mat.or.vec(length(ind_train_4c),vector_length)
for(i in 1:length(ind_train_4c)){
  #to show the first five training files
  train_matrixc[i,]=face_matrix_4c[ind_train_4c[i],]
}
train_matrix_centeredc=train_matrixc-colMeans(train_matrixc)
image_trainc=prcomp(train_matrix_centeredc)
image_train_scoresc=train_matrix_centeredc%*%image_trainc$rotation[,1:25]

test_matrixc=mat.or.vec(length(ind_test_4c),vector_length)
for(i in 1:length(ind_test_4c)){
  #to show the first five training files
  test_matrixc[i,]=face_matrix_4c[ind_test_4c[i],]
}

test_matrix_centeredc=test_matrixc-colMeans(train_matrix)
image_test_scoresc=test_matrix_centeredc%*%image_trainc$rotation[,1:25]
#to predict the pictures in the test_set
knn.predc=knn(image_train_scoresc,image_test_scoresc,subject_number1[ind_train_4c],k=1)
#compare the predictions with the test_set
table(knn.predc,subject_number1[ind_test_4c])
#count the correct prediction
sum(diag(table(knn.predc,subject_number1[ind_test_4c])))
#count the incorrect prediction
length(ind_test_4c)-sum(diag(table(knn.predc,subject_number1[ind_test_4c])))
which(subject_number1[ind_test_4c]!=knn.predc)


# print wrong prediction faces
wrongface_matrix=vector()
originalface_matrix=vector()
knn.predc1=as.numeric(as.character(knn.predc))
for(i in 1:length(which(subject_number1[ind_test_4c]!=knn.predc))){
  
    originalface_vector=face_matrix_4c[ind_test_4c[which(subject_number1[ind_test_4c]!=knn.predc)[i]],]
    dim(originalface_vector)=c(192,168)
    originalface_matrix=cbind(originalface_matrix,originalface_vector)
}

originalface=pixmapGrey(originalface_matrix)
par(mfrow=c(1,2),mar=c(2,1,1,2))
plot(originalface)
title("Original faces")
wrongface_matrix=vector()
for(i in 1:length(which(subject_number1[ind_test_4c]!=knn.predc))){
  #add one eigenface at a time
  wrongface_vector=face_matrix_4c[4*(knn.predc1[i]-1)+1,]
  dim(wrongface_vector)=c(192,168)
  wrongface_matrix=cbind(wrongface_matrix, wrongface_vector)
}
wrongface=pixmapGrey(wrongface_matrix)
plot(wrongface)
title("1NN faces")

# code for q4(d)
# run the partial code in the part c for 10 times and print the results
for(i in 1:10){
  set.seed(i+2)
ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]
train_matrixc=mat.or.vec(length(ind_train_4c),vector_length)
for(i in 1:length(ind_train_4c)){
  #to show the first five training files
  train_matrixc[i,]=face_matrix_4c[ind_train_4c[i],]
}
train_matrix_centeredc=train_matrixc-colMeans(train_matrixc)
image_trainc=prcomp(train_matrix_centeredc)
image_train_scoresc=train_matrix_centeredc%*%image_trainc$rotation[,1:25]

test_matrixc=mat.or.vec(length(ind_test_4c),vector_length)
for(i in 1:length(ind_test_4c)){
  #to show the first five training files
  test_matrixc[i,]=face_matrix_4c[ind_test_4c[i],]
}

test_matrix_centeredc=test_matrixc-colMeans(train_matrix)
image_test_scoresc=test_matrix_centeredc%*%image_trainc$rotation[,1:25]
#to predict the pictures in the test_set
knn.predc=knn(image_train_scoresc,image_test_scoresc,subject_number1[ind_train_4c],k=1)
#compare the predictions with the test_set
#count the correct prediction
print(sum(diag(table(knn.predc,subject_number1[ind_test_4c]))))
#count the incorrect prediction
print(length(ind_test_4c)-sum(diag(table(knn.predc,subject_number1[ind_test_4c]))))
}


