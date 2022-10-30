library(readxl)
library(dplyr)
library(ggplot2)

data<-read.csv("C:/Users/Vaishu/Downloads/CS1B_MotorClaims.csv",header=T)
data
##
mean(data$Claims)
sd(data$Claims)
lamda<-mean(data$Claims)/(sd(data$Claims)^2)
alpha<-mean(data$Claims)*lamda
set.seed(100)
k<-rgamma(1000,alpha,lamda)
head(k)
mean(k)
vari<-sd(k)^2
qqnorm(k)
qqline(k)
#####################
data("mtcars")
?mtcars
dim(mtcars)
str(mtcars)
table(mtcars)
summary(mtcars)
View(mtcars)
mtcars<-mtcars[,-c(8,9)]
cor(mtcars)
pca<-prcomp(mtcars,center = T,scale = T)
summary(pca)#check cumm prop
############################
data2<-read.csv("C:/Users/Vaishu/Downloads/CS1B_BMIClaims.csv",header=T)
###########################

###############################
data3<-read.csv("C:/Users/Vaishu/Downloads/FluTrain.csv",header=T)
View(data3)
hist(data3$ILI)
plot(data3$Queries,log(data3$ILI))
mymod<-lm(ILI~Queries,data = data3)
summary(mymod)
#ILI=0.01374+5.81454*QUERIES
cor(data3$ILI,data3$Queries)#strong positive
data3$Week<-as.factor(data3$Week)
mytab<-as.data.frame(summarize(group_by(data3,Week)),ILI)
View(mytab)
which.max(data3$ILI)
data3$ILI[303]
data3$Week[303]
mymod$fitted.values[303]
mymod$residuals[303]
mymod$coefficients
