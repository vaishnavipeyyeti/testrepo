library(readxl)
library(dplyr)
library(ggplot2)
data<-read.csv("C:/Users/Vaishu/Downloads/CS1B_AutoClaims.csv",header=T)
View(data)
str(data)
data$CLASS<-as.factor(data$CLASS)
data$STATE<-as.factor(data$STATE)
data$GENDER<-as.factor(data$GENDER)
k<-as.data.frame(summarize(group_by(data$STATE)))

k
table=as.data.frame(summarize(group_by(data,STATE),mean=mean(PAID),stdev=sd(PAID),cv=mean(PAID)/sd(PAID)))
table
table2=as.data.frame(summarize(group_by(data,CLASS),mean=mean(PAID),stdev=sd(PAID),cv=mean(PAID)/sd(PAID)))
table2
boxplot(PAID~STATE,data = data)
boxplot(PAID~CLASS,data = data)
ct<-table(data$GENDER,data$CLASS)
chisq.test(ct)
m<-data[data$GENDER=="F",]
m
n<-data[data$GENDER=="M",]
n
t.test(m$PAID,n$PAID)
t.test(m$PAID,n$PAID,var.equal=FALSE)
var.test(m$PAID,n$PAID)
###################################################################
dim(data)
#normal distn
logl(x)<-function(x){
(1417*log(teta))-(teta*sum(x))
}
################################################################
str(data)
model<-lm(PAID~CLASS+AGE+GENDER+STATE,data = data)
model
summary(model)
plot(model$fitted.values,model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)
#qqnorm()
