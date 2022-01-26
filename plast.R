library(ggplot2)
library(dplyr)
dd<-Book1
head(dd)
str(dd)
colnames(dd)

dd$AGE<-as.factor(dd$AGE)
dd$reject<-as.factor(dd$reject)
dd$GENDER<-as.factor(dd$GENDER)
dd$QUAL<-as.factor(dd$QUAL)
dd$OCCUP<-as.factor(dd$OCCUP)
dd$`Does using plastic play an important role in your day-to-day life?`<-as.factor(dd$`Does using plastic play an important role in your day-to-day life?`)
dd$`Plastic can be`<-as.factor(dd$`Plastic can be`)
dd$`Do you follow the 3R’S principle while using plastic ?`<-as.factor(dd$`Do you follow the 3R’S principle while using plastic ?`)
dd$`For which of these  plastic item you will prefer to use plastic substitute:`<-as.factor(dd$`For which of these  plastic item you will prefer to use plastic substitute:`)
dd$`What among these plastic substitutes did you use more?`<-as.factor(dd$`What among these plastic substitutes did you use more?`)
dd$`Do you like to participate in an initiative to reduce plastic pollution?`<-as.factor(dd$`Do you like to participate in an initiative to reduce plastic pollution?`)
dd$`Do you know how to check chance of plastic recyclability  ?`<-as.factor(dd$`Did you ever check the following symbol on the plastic you use?`)

str(dd)
#aaa############################################
#OBJECTIVE 1
o1a<-table(dd$AGE,dd$`Do you follow the 3R’S principle while using plastic ?`)
chisq.test(o1a)
pvalue =  0.002168
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 18.719, df = 5, p-value = 0.002168

o1b<-table(dd$GENDER,dd$`Do you follow the 3R’S principle while using plastic ?`)
chisq.test(o1b)
pvalue =  0.63
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 0.23206, df = 1, p-value = 0.63

o1c<-table(dd$QUAL,dd$`Do you follow the 3R’S principle while using plastic ?`)
chisq.test(o1c)
pvalue =  0.2053
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 4.5795, df = 3, p-value = 0.2053


o1d<-table(dd$OCCUP,dd$`Do you follow the 3R’S principle while using plastic ?`)
chisq.test(o1d)
pvalue = 0.001695
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 19.293, df = 5, p-value = 0.001695


#OBJECTIVE2
o2a<-table(dd$AGE,dd$reject)
chisq.test(o2a)
pvalue =  0.1659
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 14.159, df = 10, p-value = 0.1659

o2b<-table(dd$GENDER,dd$reject)
chisq.test(o2b)
pvalue = 0.004888
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 10.642, df = 2, p-value = 0.004888

o2c<-table(dd$QUAL,dd$reject)
chisq.test(o2c)
pvalue =  0.09119
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 10.91, df = 6, p-value = 0.09119

o2d<-table(dd$OCCUP,dd$reject)
chisq.test(o2d)
pvalue = 0.7075
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 7.1891, df = 10, p-value = 0.7075

#OBJECTIVE 3
o3a<-table(dd$AGE,dd$`For which of these  plastic item you will prefer to use plastic substitute:`)
chisq.test(o3a)
pvalue =   0.3895
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 21.131, df = 20, p-value = 0.389

o3b<-table(dd$GENDER,dd$`For which of these  plastic item you will prefer to use plastic substitute:`)
chisq.test(o3b)
pvalue =  0.7066
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 2.1588, df = 4, p-value = 0.7066

o3c<-table(dd$QUAL,dd$`For which of these  plastic item you will prefer to use plastic substitute:`)
chisq.test(o3c)
pvalue =  0.3169
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 13.751, df = 12, p-value = 0.3169

o3d<-table(dd$OCCUP,dd$`For which of these  plastic item you will prefer to use plastic substitute:`)
chisq.test(o3d)
pvalue =  0.4157
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 20.689, df = 20, p-value = 0.4157

#OBJECTIVE 3@2
o32a<-table(dd$AGE,dd$`What among these plastic substitutes did you use more?`)
chisq.test(o32a)
pvalue =    0.2135
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 24.692, df = 20, p-value = 0.2135


o32b<-table(dd$GENDER,dd$`What among these plastic substitutes did you use more?`)
chisq.test(o32b)
pvalue =  0.5136
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 3.2704, df = 4, p-value = 0.5136

o32c<-table(dd$QUAL,dd$`What among these plastic substitutes did you use more?`)
chisq.test(o32c)
pvalue =  0.7816
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 8.0451, df = 12, p-value = 0.7816

o32d<-table(dd$OCCUP,dd$`What among these plastic substitutes did you use more?`)
chisq.test(o32d)
pvalue =  0.2563
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 23.688, df = 20, p-value = 0.2563


#OBJECTIVE4
o4<-table(dd$QUAL,dd$`Do you like to participate in an initiative to reduce plastic pollution?`)
chisq.test(o4)
pvalue = 0.8124
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}

o4<-table(dd$QUAL,dd$`Do you know how to check chance of plastic recyclability  ?`)
chisq.test(o4)
pvalue =  0.02579
if(pvalue>0.05){
  print("accept H0")
}else{
  print("reject  h0")
}
#X-squared = 9.28, df = 3, p-value = 0.02579


