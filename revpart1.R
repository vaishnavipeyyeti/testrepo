library(dplyr)
x="1"
class(x)
x=as.numeric(x)
class(x)
rm(x)
x=10.999999
sqrt(x)
abs(x)
round(x,0)
ceiling(x)
floor(x)
?trunc
trunc(x)
x=c(1,2,3)
y=c("a","b","c","b","d","l")
x[1]
y[2]
x[x>2]
min(x)
max(x)
mean(x)
sum(x)
x=1:20
sample(x,5)
x<-read.csv("C:/Users/Vaishu/Desktop/CS1/HealthClaims.csv")
x
head(x)
z="a"
class(z)
typeof(z)
#         +-*/
x**2
y<-factor(y)
y
i=1:9
mat<-matrix(i,nrow=3,ncol=6,byrow = F)
mat
array1<-array(i,dim = c(2,2,3))
array1
is.factor(y)
i=c(1,2,3,4,5,6,7,1,2,3,4,5,6,7)
l=as.factor(i)
l
levels(l)
levels(l)<-c(levels(l),"9")
l[16]="12"
###########################################################################################
x
head(x)
summary(x)
str(x)
a<-subset(x,select = c(3:5))
a
select(x,GENDER:CLAIM_AMOUNT)
arrange(a,CLAIM_AMOUNT)
arrange(a,GENDER,CLAIM_AMOUNT)
arrange(a,desc(CLAIM_AMOUNT))
arrange(x,GEOGRAPHY,GENDER,AGE)
filter(x,AGE==45 & GENDER=="M")
select(filter(x,AGE==45 & CLAIM_AMOUNT>40000),GENDER,AGE,CLAIM_AMOUNT)
U<-mutate(select(filter(x,AGE==45 & CLAIM_AMOUNT>40000),GENDER,AGE,CLAIM_AMOUNT),CLAIM_BON=CLAIM_AMOUNT*0.1)
TM<-transmute(select(filter(x,AGE==45 & CLAIM_AMOUNT>40000),GENDER,AGE,CLAIM_AMOUNT),CLAIM_AMOUNT=CLAIM_AMOUNT*0.1)
V<-data.frame(U,TM)
V
distinct(x,GENDER)
distinct(x,GENDER,PROFESSION)
arrange(distinct(x,GENDER,PROFESSION),PROFESSION)
summarize(x,total=sum(CLAIM_AMOUNT),mean=mean(CLAIM_AMOUNT))
group_by(x,PROFESSION)
summarize(group_by(x,PROFESSION),total=sum(CLAIM_AMOUNT),mean=mean(CLAIM_AMOUNT))
summarize(group_by(x,GENDER),total=sum(CLAIM_AMOUNT),mean=mean(CLAIM_AMOUNT))
