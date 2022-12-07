library(dplyr)
data<-read.csv(file.choose(),header=T)
k<-mutate(data,tax=PAID*0.5)
head(k)
distinct(data,CLASS)
distinct(data,CLASS,GENDER)
summarize(group_by(k,CLASS,GENDER),total=sum(PAID),rcount=n())
arrange(select(filter(k,GENDER=="F" & PAID>50000),CLASS,STATE,PAID,GENDER),PAID)
k%>%filter(PAID>45000)%>%select(CLASS,PAID,STATE,GENDER,tax)
x=10
if(x<5){print("good")}else{print("bad")}
if(x<5 | x>10){print("good")}else{print("bad")}
if(x<=10 & x>5){print("good")}else{print("bad")}
x<-1:12
if(any(x>15)){print("good")}else{print("bad")}
summary(k)
k$level<-ifelse(k$PAID<1100,"low",ifelse(k$PAID<3400,"med","high"))
k
head(k)
final=k%>%group_by(level)%>%summarise(total=sum(PAID),rcount=n())%>%arrange(rcount)
final=data.frame(final)
final
#10%2
sum=0
for(i in x){sum=sum+i };print(sum)
for(i in x){if(i>10) sum=sum+i };print(sum)
for(i in x){sum=sum+i ;print(sum);print(i)};print(sum)
m=1
rate=0
while(sum<5000){sum=150+sum;rate=0.15*sum;sum=sum+rate;m=1+m;print(rate);print(m);print(sum)}
x=0
repeat{print(x);x=x+1;if(x>5){break}}
 myfun=function(x,y){sum=x+y;print(sum)}
myfun(5,3) 

k
plot(final$total)
l=k%>% group_by(STATE)%>%summarize(total=sum(PAID),taxtot=sum(tax))%>%arrange(total)
l
plot(l$total,l$taxtot)
lines(l$total,l$taxtot,type = "p")
lines(l$total,l$taxtot,type = "c")
lines(l$total,l$taxtot,type = "h")
dev.off()
barplot(l$total,main="chart",xlab = "x",ylab="y",names.arg = l$STATE,col=c("blue","red","brown","green","yellow","black"))
library(ggplot2)
boxplot(total~STATE,data=l)
boxplot(PAID~STATE,data=data)
data()
mtcars
str(mtcars)
mt=mtcars[,-1]
str(mt)
mt=as.matrix(mt)
is.matrix(mt)
cor(mt,method = "pearson")
cor(mt,method = "spearman")
cor(mt,method = "kendall")
c=cor(mt)
library(corrplot)
corrplot(c,type="upper",method = "circle",col="blue")
corrplot(c,type="lower",method = "pie",bg="brown")
corrplot(c,type="upper",method = "number",col="blue")
corrplot(c,type="upper",method = "ellipse",col="blue")
corrplot(c,type="upper",method = "color",col="blue")
corrplot(c,type="upper",method = "shade",col="blue")
corrplot(c,type="upper",method = "square",col="blue")

corrplot.mixed(c,lower="number",upper="pie",order="AOE")
corrplot.mixed(c,lower="pie",upper="shade",order="FPC")
corrplot.mixed(c,lower="pie",upper="shade",order="hclust")
corrplot.mixed(c,lower="pie",upper="shade",order="alphabet")

corrplot(c,method="number",order="hclust",addrect=5)

library(corrgram)
corrgram(c,order=T,lower.panel = panel.shade,upper.panel = NULL,text.panel = panel.txt)
dev.off()


names(data)
dim(data)
nrow(data)
ncol(data)
table(data$STATE,data$GENDER)
k
k$posttax=apply(k[,5:6],1,sum)
sub=function(x){z=x-y;return(z)}
k$posttax=lapply(k[,5:6],sum)






















