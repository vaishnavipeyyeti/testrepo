#naval data
#install.packages("titanic")
library(titanic)
df<-titanic::titanic_train
head(df)
df_subset<-subset(df,Sex=="male" & Survived==1 & Age<=18)
head(df_subset)
nrow(df_subset)
###above is the no. of survived male child
df2<-subset(df,Pclass==1)
nrow(df2)
#no. of 1 class passengers

#naval dataset
naval<-read.csv(file.choose(),header = F)
head(naval)
tail(naval)
colnames(naval)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","comp","turbine")
#density fun
d<-density(naval$comp)
plot(d)
#the curve is symmetric thus we can fit normal distribution

#scatter plot matrix
pairs(naval[,],pch=12)

#means and median
m1<-mean(naval$comp)
m1
m2<-mean(naval$turbine)
m2
med1<-median(naval$comp)
med1
med2<-median(naval$turbine)
med2
#apply min max scale
#apply(naval,2,min)
#apply(naval,2,max)
#naval.min.max<-t(apply(naval,1,nor.min.max))
library(caret)
process<-preProcess(as.data.frame(naval),method=c("range"))
norm_scale<-predict(process,as.data.frame(naval))

#Boston set
library(MASS)
bos<-MASS::Boston
head(bos)
