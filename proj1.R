#first we load the original data
#i already imported it so i will just check with head
head(winequality.red)
str(winequality.red)
#quality is int
#duplicate the original set as wine
wine<-winequality.red
head(wine)
summary(wine)

#classify data of quality binarily >5 and lessthan and = to
wine$quality<-ifelse(wine$quality>5,1,0)
#convert quality as factor
wine$quality<-factor(wine$quality,levels = c(0,1),ordered = T)
summary(wine)
str(wine)
#classification completed


#starting regression(logistic)
logisticmod<-glm(wine$quality~.,family = binomial(),data=wine)
pred<-predict(logisticmod,type = "response")

#create the input variables data set
inputs<-wine[,1:11]
head(inputs)

#convert the 
s=wine$quality==1

#error value
error<-sum((pred >= 0.5) != s)/nrow(inputs)
error
1-error

input_pred= predict(logisticmod,inputs,type = "response")

input_pred[input_pred>=0.5]<-1
input_pred[input_pred!=1]<-0
input_pred[is.na(input_pred)]<-0

#compare our predicted value and the predicted value of the data source
View(wine$quality)
View(input_pred)
write.table(input_pred, "predictions.csv",col.names = F,row.names=F,quote=FALSE)
