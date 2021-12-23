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

#model with trn
dtr<-diff(train)
dtr2<-diff(dtr)

plot(train)
plot(dtr)
plot(dtr2)

plot(log(train))
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


library(tseries)
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

library(forecast)
auto.arima(log(train))
#ar-0 i-1 ma-1
m1<-arima(train,order = c(0,1,1))
m2<-arima(train,order = c(0,1,0))
m3<-arima(log(train),order = c(0,1,0))



#lower aic check
library(TSPred)

fit<-predict(m1,61)
MAPE(tst,fit$pred)
p1<-predict(m1,91)
ans<-p1$pred[62:91]

fit<-predict(m2,61)
MAPE(tst,fit$pred)
p2<-predict(m2,91)
ans<-p2$pred[62:91]

fit<-predict(m3,61)
MAPE(log(tst),fit$pred)
p3<-predict(m3,91)
ans<-p3$pred[62:91]

#m1 is the best fit
