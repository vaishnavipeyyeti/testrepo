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
















