library(dplyr)
library(ggplot2)



w<-as.data.frame(winequality.red)
head(w)
summary(w)
colSums(is.na(w))
table(w$quality)
#w$quality<-factor(w$quality,levels=c(0,1,2,3,4,5,6,7,8,9,10),ordered = T)

####################
outliers<-function(x){
  upper<-quantile(x,probs=.75)
  lower<-quantile(x,probs = .25)
  Iqr=upper-lower
  upper1=upper+(Iqr *1.5)
  lower1=lower-(Iqr*1.5)
  x>upper1|x<lower1
}



remove_outliers<-function(w,cols = names(wine)){
  for (col in cols) {
    w<-w[!outliers(w[[col]]),]
  }
  w
}

w<-remove_outliers(w,c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol" ))
ncol(w)
nrow(w)
################################
colnames(w)
w$quality<-ifelse(w$quality>5,1,0)
#w$quality<-factor(w$quality,levels = c(0,1),ordered = TRUE)
str(w)
#var1
plot<-ggplot(w,aes(x=factor(quality),y=fixed.acidity,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="blue")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$fixed.acidity)#outliers still exist in 2 cases
count<-table(w$fixed.acidity)
barplot(count)

#2
plot<-ggplot(w,aes(x=factor(quality),y=volatile.acidity,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$volatile.acidity)#outlier
barplot(table(w$volatile.acidity))
#3
plot<-ggplot(w,aes(x=factor(quality),y=citric.acid,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$citric.acid)
barplot(table(w$citric.acid))

#4
plot<-ggplot(w,aes(x=factor(quality),y=residual.sugar,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$residual.sugar)#out in 2 cases
barplot(table(w$residual.sugar))
#5
plot<-ggplot(w,aes(x=factor(quality),y=chlorides,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$chlorides)#out in 2 cases
barplot(table(w$chlorides))
#6
plot<-ggplot(w,aes(x=factor(quality),y=free.sulfur.dioxide,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$free.sulfur.dioxide)#out
barplot(table(w$free.sulfur.dioxide))
#7
plot<-ggplot(w,aes(x=factor(quality),y=total.sulfur.dioxide,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")
boxplot(w$total.sulfur.dioxide)#out
barplot(table(w$total.sulfur.dioxide))
#8
plot<-ggplot(w,aes(x=factor(quality),y=density,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="blue")+facet_wrap(~quality)
boxplot(w$density)
barplot(table(w$density))
#9
plot<-ggplot(w,aes(x=factor(quality),y=pH,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$pH)#out
barplot(table(w$pH))
#10
plot<-ggplot(w,aes(x=factor(quality),y=sulphates,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter(color="red")+facet_wrap(~quality)
boxplot(w$sulphates)#out
barplot(table(w$sulphates))
#11
plot<-ggplot(w,aes(x=factor(quality),y=alcohol,fill=factor(quality)))
plot+geom_boxplot()
plot+geom_violin()
plot+geom_bar(stat = "identity")
plot+geom_jitter()+facet_wrap(~quality)
boxplot(w$alcohol)
barplot(table(w$alcohol))
#ggplot(w,aes(x=fixed.acidity,y=volatile.acidity))+geom_point(aes(color=quality),size=3,alpha=0.8)+geom_smooth(aes(color=quality,fill=quality),method="lm",fullrange=T)+facet_wrap(~quality)+theme_bw()


##################################
#
library(scatterplot3d)
scatterplot3d(w$fixed.acidity,w$volatile.acidity,factor(w$quality),main = "3dplot")
scatterplot3d(w$citric.acid,w$residual.sugar,w$quality,main = "3dplot")
scatterplot3d(w$free.sulfur.dioxide,w$total.sulfur.dioxide,w$quality,main = "3dplot")
scatterplot3d(w$density,w$pH,w$quality,main = "3dplot")
scatterplot3d(w$sulphates,w$alcohol,w$quality,main = "3dplot")









############
ws<-w
ws_2 <- subset(ws, select= -c(quality))
x1<-cor(ws_2,method = "pearson") # correlation
x_cor<-round(x1,2)
x_cor
#correl
library(corrplot)
corrplot(cor(w[,1:11]),method = "number",type="upper")

#donot have high correl between var
#using mctest
library(mctest)

modq1<-glm(w$quality~.,data = w)
summary(modq1)
str(w)
omcdiag(modq1)
imcdiag(modq1)
#farrar chi sqr is very high
#critical f value at (11-1,1135-(11-1)) is 1.8391027
#colleniarty  in 9 cols
library(ppcor)
pcor(w[,1:11],method = "pearson")



#73.9
logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$alcohol+w$free.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)

#73.7%
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$alcohol+w$total.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$alcohol+w$free.sulfur.dioxide+w$citric.acid,family=binomial(),data=w)

#72
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$alcohol+w$total.sulfur.dioxide+w$citric.acid,family=binomial(),data=w)

#70%
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphate+w$density+w$free.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$density+w$free.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)

#70%but collinear density
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$density+w$total.sulfur.dioxide+w$citric.acid,family=binomial(),data=w)
#test==69.77
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$density+w$total.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$pH+w$total.sulfur.dioxide+w$volatile.acidity+w$residual.sugar,family=binomial(),data=w)

#test==68.88 
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$fixed.acidity+w$total.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$pH+w$total.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)
#logisticmod11<-glm(w$quality~w$chlorides+w$sulphates+w$alcohol+w$free.sulfur.dioxide+w$volatile.acidity,family=binomial(),data=w)



summary(logisticmod11)

omcdiag(logisticmod11)
imcdiag(logisticmod11)
pred<-predict(logisticmod11,type = "response")
#convert the 
s=w$quality==1
#str(pred)
#error value
error<-sum((pred >= 0.5) != s)/nrow(w[,1:11])
error
1-error
input_pred= predict(logisticmod11,w[,1:11],type = "response")

input_pred[input_pred>=0.5]<-1
input_pred[input_pred!=1]<-0
table(input_pred)
table(w$quality)

#acc
misac=mean(input_pred!=w$quality)
ac=1-misac
ac
#rsq
library(caret)
#res<-caret::postResample(input_pred,w$quality)
#res
library(ROCR)
pr<-prediction(input_pred,w$quality)
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
auc<-performance(pr,measure = "auc")
auc<-auc@y.values[[1]]
auc
library(pscl)
pR2(logisticmod11)


cm<-as.matrix(table(w$quality,input_pred))
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)
