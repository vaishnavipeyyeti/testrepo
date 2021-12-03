#naval data
install.packages("titanic")
library(titanic)
df<-titanic::titanic_train
head(df)
df_subset<-subset(df,Sex=="male" & Survived==1)
head(df_subset)
nrow(df_subset)
###above is the no. of survived male child
df2<-subset(df,Pclass==1)
nrow(df2)
#no. of 1 class passengers
naval<-read.csv(file.choose(),header = F)
head(naval)
tail(naval)
colnames(naval)<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","comp","turbine")
d<-density(naval$comp)
plot(d)
pairs(head(naval))
m1<-mean(naval$comp)
m1
m2<-mean(naval$turbine)
m2
med1<-median(naval$comp)
med1
med2<-median(naval$turbine)
med2
apply(naval,2,min)
apply(naval,2,max)
#
library(MASS)
bos<-MASS::Boston
head(bos)
min(bos$tax)

