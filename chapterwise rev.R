#chap6
set.seed(23)
xbar<-rep(0,1000)#array var
for (i in 1:1000){x<-rpois(40,5);xbar[i]<-mean(x)}
k<-data.frame(0:1000,xbar)
View(k)
hist(xbar,prob=T,ylim = c(0,1.2))# observed
curve(dnorm(x,mean=5,sd=sqrt(0.125)),add = T,lwd=2,col="red")#clt
qqnorm(1:1000,xbar)
dev.off()
# chap 8
ss<-rnorm(10)
sample(ss,replace = T)#single resample with replacement
set.seed(47)
es<-rep(0:1000)
for (i in 1:1000){x<-sample(ss,replace = T);es[i]<-1/mean(x)}
es<-replicate(1000,1/mean(sample(ss,replace = T)))
mean(es)
sd(es)
quantile(es,c(0.025,0.975))#95% CI for es
pes<-rep(0:1000)
for(i in 1:1000){x<-rexp(10,1/mean(ss));pes[i]<-1/mean(x)}
pes<-replicate(1000,1/mean(rexp(10,1/mean(ss))))
hist(xbar)
#chap 10
d<-rnorm(15,54,67)
t.test(d,conf=0.95)#small samples  unknown var
t.test(d,mu=32,conf=0.95)
qt(0.025,14)
plot(pt(0:10,14))
chical<-9*22.57/16
plot(pchisq(0:100,9))
qchisq(c(0.025,0.975),9)#tab
binom.test(3,4,0.1,conf=0.95)#prop test
poisson.test(116,500,conf=0.95)
t.test(rnorm(10,2,3),rnorm(10,6,12),var.equal = T)#2 ind samples
?var.test()
var.test(rnorm(10,2,3),rnorm(10,6,12))#ftest
prop.test(c(62,115),c(100,200),correct = F)
poisson.test(c(150,650),c(500,4500),conf.level = 0.95)
t.test(rnorm(10,2,3),rnorm(10,12,12),paired=T)
x=abs(rnorm(10,2,3))
y=rnorm(10,6,12)
results<-c(x,y)
index<-1:length(results)
#p<-combn()
p=rep(0:9)
for(i in 1:10){p[i]=dnorm(x[i],2,)}
p
chisq.test(x,p=p)
l<-matrix(c(3,7,0,32,15,16,5,4,11),byrow=T,nrow=3)
rownames(l)<-c("x1","x2","x3")
colnames(l)<-c("y1","y2","y3")
l
fisher.test(l,alternative = "less",conf.level =0.95)


# chap11
data(corrgram::baseball)
str(baseball)
plot(baseball$Hits,baseball$Runs)
cor(baseball$Hits,baseball$Runs,method = "pearson")
cor(baseball$Hits,baseball$Runs,method="spearman")
cor(baseball$Hits,baseball$Runs,method="kendall")
cor.test(baseball$Hits,baseball$Runs,method = "pearson")
cor.test(baseball$Hits,baseball$Runs,method = "spearman")
cor.test(baseball$Hits,baseball$Runs,method = "kendall")# exact p if n<50
plot(baseball$Hits,baseball$Runs)
t<-baseball[,c("Walks","Runs","Years","Hits")]
pairs(t)
cor(t,method = "pearson")
prcomp(t)
j<-princomp(t)
model<-lm(Runs~Hits,data = t)
summary(model)
abline(model)
plot(t$Hits,t$Runs)
anova(model)
confint(model,level = 0.95)
new<-data.frame(Hits=4)
predict(model,new)
predict(model,new,interval = "confidence",level=0.95)#mean response
predict(model,new,interval = "predict",level=0.95)#individual
m<-data.frame(fitted(model),residuals(model))
View(m)

model2<-lm(Runs~.,data = t)
summary(model2)
anova(model2)
confint(model,level = 0.95)
new2<-data.frame(Hits=4,Walks=12,Years=14)
predict(model2,new2)
predict(model2,new2,interval = "confidence",level=0.95)#mean response
predict(model2,new2,interval = "predict",level=0.95)#individual
m<-data.frame(t$Runs,fitted(model2),residuals(model2))
View(m)
model3<-lm(Runs~Hits,data=t)

# chap 14
M=10
n=5
alpha=5
beta=7;theta=0.5
pm<-rep(0:M)
for(i in 1:M)
{
  theta<-rbeta(1,alpha,beta)
  x<-rbinom(1,n,theta)
  pm[i]<-(x+alpha)/(n+alpha+beta)
}
mean(pm)
qgamma(0.025,15,5.3)#credible interval
qgamma(0.975,15,5.3)#at 90% level
library(bayestestR)
set.seed(3)
hdi(rgamma(100000,15,5.3),ci=0.95)

#chap 15
n=10;M=15;sigma1=3;sigma2=5;mu=2
z<-n/(n+sigma1^2+sigma2^2)
cp<-rep(0,M)
for(i in 1:M){
theta<-rnorm(1,mu,sigma2)
x<-rnorm(n,theta,sigma1)
cp[i]<-z*mean(x)+(1-z)*mu
}
mean(cp)

#chap 16







