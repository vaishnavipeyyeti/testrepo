library(dplyr)
library(ggplot2)



w<-as.data.frame(winequality.red)
head(w)
summary(w)
colSums(is.na(w))
table(w$quality)
w$quality<-factor(w$quality,levels=c(0,1,2,3,4,5,6,7,8,9,10),ordered = T)

####################
outliers<-function(x){
  upper<-quantile(x,probs=.95)
  lower<-quantile(x,probs = .05)
  #Iqr=upper-lower
  #upper1=upper+(Iqr *1.5)
  #lower1=lower-(Iqr*1.5)
  x>upper|x<lower
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
w$quality<-factor(w$quality,levels = c(0,1),ordered = T)
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
x<-cor(ds_2,method = "pearson") # correlation
x_cor<-round(x,2)
x_cor
#correl
library(corrplot)
corrplot(cor(w[,1:11]),method = "number",type="upper")

#donot have high correl between var
#using mctest
library(mctest)
mod<-glm(w$quality~.,data = w)
omcdiag(mod)
imcdiag(mod)
#farrar chi sqr is very high
#critical f value at (11-1,1135-(11-1)) is 1.8391027
#colleniarty  in 9 cols
library(ppcor)
pcor(w[,1:11], method = "pearson")



#
logisticmod<-glm(w$quality~w$chlorides+w$sulphates+w$fixed.acidity,family=binomial(),data=w)
summary(logisticmod)
pred<-predict(logisticmod,type = "response")
#convert the 
s=w$quality==1
str(pred)
#error value
error<-sum((pred >= 0.5) != s)/nrow(w[,1:11])
error
1-error
input_pred= predict(logisticmod,w[,1:11],type = "response")

input_pred[input_pred>=0.5]<-1
input_pred[input_pred!=1]<-0
table(input_pred)
table(w$quality)
