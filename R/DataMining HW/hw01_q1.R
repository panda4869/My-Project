# Code for 2.8(a)
college=read.csv("College.csv",header=T,na.strings="?")
fix(college)

# Code for 2.8(b)
rownames(college)=college[,1]
fix(college)
college=college[,-1]# Remove the first column
fix(college)

# Code for 2.8(c)
summary(college)# summary 
pairs(college[,1:10])# Generate scatterplot
attach(college)
plot(Outstate~Private,xlab="Private",ylab="Outstate")# Generate boxplot
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)#Create a new qualitative variable
summary(college$Elite)
plot(Outstate~Elite,xlab="Elite",ylab="Outstate")
par(mfrow=c(2,2))#Divide the graph into 4 sections to output histogram simultaneouslyi
hist(Enroll)
hist(Accept)
hist(F.Undergrad)
hist(P.Undergrad)
lm.sol<-lm(Accept~1+Enroll)# Create a linear regression model
plot(Enroll,Accept);abline(lm.sol)# To draw the regression line
summary(lm.sol)# To show the fitness of the model
par(mfrow=c(2,2))
plot(lm.sol)
detach(college)
