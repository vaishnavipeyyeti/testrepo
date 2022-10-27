library(dplyr)
library(ggplot2)
library(abd)

data(iris)
names(iris)
str(iris)
dim(iris)
View(iris)
ls()
data(LakeHuron)
ls()
data(faithful)
View(faithful)
summary(faithful)
dim(faithful)
hist(faithful$eruptions)
hist(faithful$eruptions, n=15)
hist(faithful$eruptions, breaks=seq(1.5,5.25,.25), col="red")
hist(faithful$eruptions, freq=F, n=15, main="Histogram of Old Faithful Eruption Times", xlab="Duration (mins)")

###############################
require(lattice)
data(iris)
histogram(iris$Sepal.Length, breaks=seq(4,8,.25))
#a<-table(iris$Sepal.Length)
#a
histogram(~Sepal.Length, data=iris, main="Iris Sepals", xlab="Length")
histogram(~ Sepal.Length | Species, data=iris, col="red")
histogram(~ Sepal.Length | Species, data=iris, n=15, layout=c(1,3))

################################################
As.in.H2O = read.csv("http://www.calvin.edu/~scofield/data/comma/arsenicInWater.csv")
senate = read.table("http://www.calvin.edu/~scofield/data/tab/rc/senate99.dat", sep="\t", header=T)
View(As.in.H2O)
View(senate)
summary(As.in.H2O)
x<-As.in.H2O$arsenic
mean(x,na.rm=T)# remove na values
median(x,na.rm=T)
min(x)
max(x)
var(x,na.rm = T)
sd(x,na.rm = T)
sd(x)^2
?var()
quantile(x)
quantile(x,probs=seq(0,1,.2),na.rm = T)#probs is quantiles seq

###############################################
monarchs = read.csv("http://www.calvin.edu/~scofield/data/comma/monarchReigns.csv")
View(monarchs)
stem(monarchs$years)
table(monarchs$years)
?stem()
########################################################
#pol = read.csv("http://www.calvin.edu/~stob/data/csbv.csv")
View(senate)
str(senate)
senate$sex<-as.factor(senate$sex)
table(senate$sex)
table(senate$party)
table(senate$sex,senate$party)
xtabs(~sex, data=senate)
xtabs(~state + party, data=senate)

####################
table(monarchs$years)
xtabs(~years, data=monarchs)
?cut()
#cut divides the range of x into intervals and codes the values in x according to which interval they fall.
cut(monarchs$years,breaks = seq(0,65,5))
table(cut(monarchs$years,breaks = seq(0,65,5)))
xtabs(~cut(monarchs$years,breaks = seq(0,65,5)))

#############################################
barplot(table(monarchs$years))
barplot(table(senate$party))
barplot(table(senate$party),col=c("red","blue"),main = "states",names=c("democratic","republic"))
barplot(xtabs(~sex+party,data=senate),beside=T,legend=c("f","m"),col=c("red","blue"))



###################################
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length, col="yellow")
boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Length ~ Species, data=iris, col="yellow", ylab="Sepal length",main="Iris Sepal Length by Species")


###################################
data(faithful)
?plot()
plot(waiting~eruptions,data=faithful)
#pch means plot character shapes from 0 to 25
#The cex attribute is the integer which is an indicator of the scale factor, which describes the amount by which the elements of the plot can be scaled. 
plot(waiting~eruptions,data=faithful,cex=.8)
plot(waiting~eruptions,data=faithful,pch=6)
plot(waiting~eruptions,data=faithful,pch=19)
plot(waiting~eruptions,data=faithful,cex=.5,pch=19,col="blue")
plot(waiting~eruptions, data=faithful, cex=.5, pch=19, col="blue", main="Old Faithful Eruptions",
     ylab="Wait time between eruptions", xlab="Duration of eruption")

###################################################
dbinom(0, 5, .5)
#chap2.r
s=1:20
sample(s,100,replace = T)
n=10
p=0.3
rbinom(100,n,p)
dbinom(2,n,p)
pbinom(2,n,p)
qbinom(0.25,n,p)
rgeom(10,0.3)
dgeom(2,p)
pgeom(2,p)
qgeom(0.25,p)
rnbinom(100,n,p)#p(y=2) type2 nb n==k
dnbinom(2,n,p)# y is fail before k success
pnbinom(2,n,p)
qnbinom(0.25,n,p)
rhyper(20,15,10,5)
dhyper(2,10,15,5)
#?dhyper
lam=3.5
rpois(10,lam)
dpois(2,lam)
ppois(2,lam)
qpois(0.25,lam)
runif(100,min=0,max=1)
punif(1.2,min=0,max=1)
dunif(1.3,min=0,max=1)
gamma(3)
rgamma(100,2,0.25)
dgamma(3,2,0.25)
pgamma(3,2,0.25)
qgamma(0.25,3,0.25)
rexp(100,1)
dexp(2,1)
pexp(2,1)
qexp(0.25,1)
rchisq(10,12)
dchisq(2,12)
#p
#q
beta(1,2)
#rbeta(),q,p,d
#norm
#lnorm
#f,t,

##############################################
dbinom(0:5, 5, .5)
sum(dbinom(0:2, 5, .5)) # probability of 2 or fewer heads in 5 flips
pbinom(2, 5, .5) # same as last line
flip5 = replicate(10000, sum(sample(c("H","T"), 5, rep=T)=="H"))
table(flip5) / 10000 # distribution (simulated) of count of heads in 5 flips
table(rbinom(10000, 5, .5)) / 10000 # shorter version of previous 2 lines
qbinom(seq(0,1,.2), 50, .2) # approx. 0/.2/.4/.6/.8/1-quantiles in Binom(50,.2) distribution
binom.test(29, 200, .21) # inference on sample with 29 successes in 200 trials
prop.test(29, 200, .21) # inference on same sample, using normal approx. to binomial

######################################
1 - pchisq(3.1309, 5) # gives P-value associated with X-squared stat 3.1309 when df=5
pchisq(3.1309, df=5, lower.tail=F) # same as above
qchisq(c(.001,.005,.01,.025,.05,.95,.975,.99,.995,.999), 2) # gives critical values like Table A
qchisq(c(.999,.995,.99,.975,.95,.05,.025,.01,.005,.001), 2, lower.tail=F) # same as above
observedCounts = c(35, 27, 33, 40, 47, 51)
claimedProbabilities = c(.13, .13, .14, .16, .24, .20)
chisq.test(observedCounts, p=claimedProbabilities) # goodness-of-fit test, assumes df = n-1
############################################
blood = read.csv("http://www.calvin.edu/~scofield/data/comma/blood.csv")
t = table(blood$Rh, blood$type)
t
addmargins(t)
addmargins(t,1)
addmargins(t,2)

########################################################################################################
smoke = matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(smoke) = c("High","Low","Middle")

rownames(smoke) = c("current","former","never")
smoke = as.table(smoke)
rowSums(smoke)
colSums(smoke)
prop.table(smoke)

?prop.table()
prop.table(smoke,1)#x/rowsum
prop.table(smoke,2)#x/colsum
barplot(smoke,legend=T,beside=T,main='Smoking Status by SES')

################################################################################
tblood = xtabs(~Rh + type, data=blood)
tblood # contingency table for blood type and Rh factors
chisq.test(tblood)
fisher.test(tblood)

########################################CVALUES
qt(c(.95, .975, .995), df=9) # critical values for 90, 95, 99% CIs for means
pt(-2.1, 11) # gives Prob[T < -2.1] when df = 11
tSamp = rt(50, 11) # takes random sample of size 50 from t-dist with 11 dfs
# code for comparing several t distributions to standard normal distribution
xs = seq(-5,5,.01)
plot(xs, dnorm(xs), type="l", lwd=2, col="black", ylab="pdf values",main="Some t dists alongside standard normal curve")
lines(xs, dt(xs, 1), lwd=2, col="blue")
lines(xs, dt(xs, 4), lwd=2, col="red")
lines(xs, dt(xs, 10), lwd=2, col="green")
legend("topright",col=c("black","blue","red","green"),legend=c("std. normal","t, df=1","t, df=4","t, df=10"), lty=1)




#################################################################3
data(warpbreaks)
head(warpbreaks)
by(warpbreaks$breaks, warpbreaks$tension, mean)


##################################################################################################
data(sleep)
View(sleep)
t.test(extra ~ group, data=sleep) # 2-sample t with group id column
str(sleep)
sleepGrp1 = sleep$extra[sleep$group==1]
 sleepGrp2 = sleep$extra[sleep$group==2]
 t.test(sleepGrp1, sleepGrp2, conf.level=.99) # 2-sample t, data in separate vectors
 qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities", pch=19, cex=.6)
 qqline(precip) # Is this line helpful? Is it the one you would eyeball?

###################################################################
power.t.test(n=20, delta=.1, sd=.4, sig.level=.05) # tells how much power at these setting
power.t.test(power=0.8, delta=.1, sd=.4, sig.level=.05) # tells how much power at these setting


########################################################################################
require(lattice)
require(abd)
data(JetLagKnees)
head(JetLagKnees)
xyplot(shift ~ treatment, JetLagKnees, type=c('p','a'), col="navy", pch=19, cex=.5)
anova( lm( shift ~ treatment, JetLagKnees ) )

























