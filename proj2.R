library(forecast)
library(tseries)
library(TSPred)
library(ggplot2)
dd<-daily.total.female.births
colSums(is.na(dd))
class(dd)
head(dd)
tail(dd)
dfb<-ts(dd[,2],start = 1959,frequency = 365)
class(dfb)
head(dfb)
plot(dfb)
tsdisplay(dfb)



##
train<-dfb[1:304]
plot.ts(train)
class(train)
head(train)
train<-ts(train,start = 1959,frequency = 365)
class(train)
tsdisplay(train,lag.max = 20)
barplot(table(train))
title(main="baby births", sub="barplot",
      xlab="number of births", ylab="count")
tst<-dd[305:365,2]
tst<-ts(tst)
class(tst)

#model with trn
dtr<-diff(train)
dtr2<-diff(dtr)

plot(train)
plot(dtr)
tsdisplay(dtr)
plot(dtr2)

plot(log(train))
tsdisplay(log(train))
#plot(dtr)
#plot(dtr2)

#####
kpss<-kpss.test(train)
kpss
k2<-kpss.test(dtr)
k2
k3<-kpss.test(dtr2)
k3

summary(train)#40
summary(log(train))
summary(dtr)#58
summary(dtr2)#95



adf.test(train)
adf.test(dtr)
adf.test(dtr2)

###############

#mavg
acf(train)
#before 0.05 and 0.06
acf(dtr)
#ma-0.00 and 0.01
acf(dtr2)

#reg
pacf(log(train))
#0.06
pacf(log(dtr))
#0.057
pacf(dtr2)

#ar-0.06
#i -0
#ma-0.06

#ar-0 i-1 ma-0.57

#################

auto.arima(train)
auto.arima(log(train))
#ar-0 i-1 ma-1
m1<-arima(train,order = c(0,1,1))

m2<-arima(train,order = c(0,1,0))
m3<-arima(log(train),order = c(0,1,0))
m4<-arima(train,order = c(2,0,2))

m5<-arima(train,order = c(2,1,2))



#lower aic check

fit<-predict(m1,61)
MAPE(tst,fit$pred)
ts.plot(dd$Births[305:365],fit$pred,lty=c(1,3),col=c(7,4))
p1<-predict(m1,91)
ans1<-p1$pred[62:91]
ans1


fit<-predict(m2,61)
MAPE(tst,fit$pred)
p2<-predict(m2,91)
ans2<-p2$pred[62:91]

fit<-predict(m3,61)
MAPE(tst,exp(fit$pred))
p3<-predict(m3,91)
ans3<-p3$pred[62:91]

fit<-predict(m4,61)
MAPE(tst,fit$pred)
p4<-predict(m4,91)
ans4<-p4$pred[62:91]

fit<-predict(m5,61)

MAPE(tst,fit$pred)
p5<-predict(m5,91)
ans5<-p5$pred[62:91]

#m1 is the best fit