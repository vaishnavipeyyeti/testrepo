head(winequality.red)
str(winequality.red)
wine<-winequality.red

wine$quality [wine$quality >5] <- 1
wine$quality [wine$quality <= 5] <- 0
#data.frame$column.name <- as.integer(data.frame$column.name)
wine$quality<-factor(wine$quality,levels = c(0,1),ordered = T)
str(wine)
logisticmod<-glm(wine$quality~.,family = binomial(),data=wine)
pred<-predict(logisticmod,type = "response")
inputs<-wine[,1:11]
head(inputs)
s=wine$quality==1
error<-sum((pred >= 0.5) != s)/nrow(inputs)
error
1-error

input_pred= predict(logisticmod,inputs,type = "response")

input_pred[input_pred>=0.5]<-1
input_pred[input_pred!=1]<-0
input_pred[is.na(input_pred)]<-0


write.table(input_pred, "predictions.csv",col.names = F,row.names=F,quote=FALSE)
