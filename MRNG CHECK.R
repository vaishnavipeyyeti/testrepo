#chap 2
s=1:20
sample(s,100,replace=T)

rbinom(10,10,0.5)
dbinom(0,10,0.5)
pbinom(0:10,10,0.5)
qbinom(0.25,10,0.5)

rgeom(10,0.5)
dgeom(10,0.5)
pgeom(10,0.5)

rnbinom(10,10,0.5)
dnbinom(15,10,0.5)#k=10 type 2 nb
pnbinom(15,10,0.5)

rhyper(20,15,10,5)#

ppois(0:10,2)
dpois(0,2)
qpois(0.5,2)

runif(10,min=0,max=10)


rgamma(10,2,0.25)


rchisq(10,5)
pchisq(10,5)
qchisq(0.025,10)

beta(2,2)

rbeta(10,2,3)

rnorm(10,1,2)
1-plnorm(8.006,5,6)
qt(0.3,24)
qf(0.95,5,12)

plot(1:15,ppois(1:15,4))

# chap  6
set.seed(23)
xbar<-1:1000
for (i in 1:1000){ x<- rpois(40,5);xbar[i]<-mean(x)}
hist(xbar,prob=T)
curve(dnorm(x,5,sqrt(0.125)),add=T)

#chap 8
ss<-rgamma(10,1,5)
sample(ss,replace = T)
est<-1:1000
for (i in 1:1000){ x<- sample(ss,replace = T);xbar[i]<-1/mean(x)}
mean(est)
sd(est)
quantile(est,c(0.025,0.975))



#chap 9
t.test(rpois(100,5),mu=15,conf.level = 0.95)
quantile(replicate(1000,mean(sample(rpois(100,5),replace = T))),c(0.025,0.975))

cf<-sqrt(4*17.8/qchisq(0.025,4))
cf
cf2<-sqrt(4*17.8/qchisq(0.975,4))
cf2
x<-rnorm(10)
mu<-mean(x)
sigma<-sqrt(var(x))
conf_int<-c(mu+sigma*sqrt(1/10)*qt(0.025,9),mu+sigma*sqrt(1/10)*qt(0.975,9))
pred_int<-c(mu+sigma*sqrt(1+1/10)*qt(0.025,9),mu+sigma*sqrt(1+1/10)*qt(0.975,9))

predict(lm(x~1),data.frame(1),interval = "confidence")
predict(lm(x~1),data.frame(1),interval = "prediction")

binom.test(40,85,conf=0.95)
poisson.test(116,500,conf.level = 0.95)
 t.test(rnorm(10,2,5),rnorm(10,2.5,5),var.equal = T)
var.test(rnorm(10,2,5),rnorm(10,2.5,16))
k<-prop.test(c(25,20),c(100,150),correct = F)
names(k)
k$conf.int
poisson.test(c(150,650),c(500,4500),conf.level = 0.99)
t.test(rnorm(10,2,5),rnorm(10,2.5,5),paired = T)

power.t.test(rnorm(10,2,5),rnorm(10,2.5,5),var.equal = T)
power.t.test(rpois(100,5))

#chap10
#chisq.test(obsfreq,p=expprob)
#chisq.test(table)
 #if 2*2 apply correct=F cont corr
fisher.test(table(c(2,4),c(1,3)))
chisq.test(table(c(2,4),c(1,3)))

#chap 11
cor(mtcars$cyl,mtcars$carb)
cor(mtcars$cyl,mtcars$carb,method = "pearson")
cor(mtcars$cyl,mtcars$carb,method = "spearman")
cor(mtcars$cyl,mtcars$carb,method = "kendall")

cor.test(mtcars$cyl,mtcars$carb,method = "pearson")
cor.test(mtcars$cyl,mtcars$carb,method = "spearman")
cor.test(mtcars$cyl,mtcars$carb,method = "kendall")
plot(mtcars$cyl,mtcars$carb)

m<-cor(mtcars[,-1])
p<-prcomp(mtcars[,1:4])
p$scale
p$x
p$rotation
p$center
p$sdev
summary(p)
plot(p,type = "line")
princomp(mtcars[,1:4])



# chap 12
str(mtcars)
x=mtcars$disp
model<-lm(mtcars$mpg~x)
summary(model)
plot(mtcars$disp,mtcars$mpg)
abline(model)
anova(model)
confint(model,level=0.95)
newdata=data.frame(x=4)
predict(model,newdata,interval = "confidence")
predict(model,newdata,interval = "prediction")
f<-data.frame(mtcars$mpg,fitted(model),residuals(model))
head(f)

model<-lm(mpg~.,data=mtcars)
summary(model)
plot(mtcars$disp,mtcars$mpg)
abline(model)
anova(model)
confint(model,level=0.95)
newdata=data.frame(x=4)
predict(model,newdata,interval = "confidence")
predict(model,newdata,interval = "prediction")
f<-data.frame(mtcars$mpg,fitted(model),residuals(model))
head(f)

model<-lm(mtcars$mpg~x+mtcars$hp)
summary(model)
plot(mtcars$disp,mtcars$mpg)
abline(model)
anova(model)
confint(model,level=0.95)
newdata=data.frame(x=4)
predict(model,newdata,interval = "confidence")
predict(model,newdata,interval = "prediction")
f<-data.frame(mtcars$mpg,fitted(model),residuals(model))
head(f)

model<-glm(vs~1,data = mtcars,family=binomial(link = "logit"))
summary(model)
anova(model)
model2<-update(model,~.+disp)
summary(model2)
anova(model2)
anova(model2,model,test="F")
anova(model2,model,test="Chi")
model3<-update(model2,~.+wt)
summary(model3)
anova(model3)
anova(model2,model3,test="F")
anova(model2,model3,test="Chi")

modela<-glm(vs~wt*disp,data = mtcars,family=binomial(link = "logit"))
summary(modela)
anova(modela)
model2<-update(modela,~.-wt:disp)
summary(model2)
anova(model2)
model3<-update(model2,~.-wt)
summary(model3)
anova(model3)
anova(model2,model3,test="F")
anova(model3,modela,test="Chi")
newdata=data.frame(disp=180)
predict(model3,newdata,type = "response")
fitted(model3)
residuals(model3)

# chap 15

n=10;M=1500;sigma1=3;sigma2=5;mu=2
z<-n/(n+sigma1^2+sigma2^2)
cp<-rep(0,M)
for(i in 1:M){
  theta<-rnorm(1,mu,sigma2)
  x<-rnorm(n,theta,sigma1)
  cp[i]<-z*mean(x)+(1-z)*mu
}
mean(cp)























