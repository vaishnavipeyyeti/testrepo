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

