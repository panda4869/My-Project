# Code for Q4 (a)
install.packages("pixmap")
library(pixmap)
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")
class(face_01)
# now plot the data
plot(face_01)
# give it a nice title
title('hw01_01a: the first face')
# save the result
filename = 'hw01_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# Code for Q4 (b)
# make face_01 into a matrix with the given command
face_01_matrix = getChannels(face_01)

# load a second face
face_02 = read.pnm(file = "CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix = getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix = cbind( face_01_matrix , face_02_matrix )
faces = pixmapGrey( faces_matrix )

# plot to verify
plot(faces)
#Find the max and min of the pixel value
max(faces_matrix)
min(faces_matrix)

# Code for Q4 (c)
# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
#Find the lenghth of each list
summary(dir_list_1)
summary(dir_list_2)
# Code for Q4 (d)
pic_list = c( 05 , 11 , 31 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')
pic_data = vector("list",length(pic_list)*length(view_list))
faces_matrix = vector()
# Create a loop to copy the picture into a matrix
for(i in 1:3){
  for(j in 1:3){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]] , view_list[j])
    face=read.pnm(file=filename)
    if(j==1){face_matrix1=getChannels(face)}
    if(j==2){face_matrix2=getChannels(face)}
    if(j==3){face_matrix3=getChannels(face)
             if(i==1){a=cbind(face_matrix1,face_matrix2,face_matrix3)}
             if(i==2){b=cbind(face_matrix1,face_matrix2,face_matrix3)}
             if(i==3){c=cbind(face_matrix1,face_matrix2,face_matrix3)}}
    if(i==3 & j==3){faces_matrix=rbind(a,b,c)}
    
  }}

faces = pixmapGrey( faces_matrix )
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename = 'hw01_01d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()


