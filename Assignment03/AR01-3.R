################################################################### 
# AR01: Answer key to Assignment 1
# By William Yu, Harvard Extension
# 9/5/2022
################################################################### 
setwd("C:/Users/wiyu/documents/Zip08/2022 Q4 Fall_Harvard/Assignment/Assignment01")
library(readxl) 

tax.df = read_excel("P02_Corporate tax.xlsx")  
head(tax.df)

eq01 = lm(ypcg ~ ctax, data=tax.df)
summary(eq01)   #R2=0.398
eq02 = lm(ypcg ~ ctax + ypc2000, data=tax.df)
summary(eq02)   #R2=0.528
eq03 = lm(ypcg ~ ctax + ypc2000 + dty + ctax*dty, data=tax.df)
summary(eq03)   #R2=0.636
eq03a = lm(ypcg ~ ypc2000 + ctax*dty, data=tax.df)
summary(eq03a)

#
# Prediction (Ans: 3.23%)
#
ctax=20
ypc2000=10000
dty=35
ypcg1=eq03$coef[[1]] + eq03$coef[[2]]*ctax + eq03$coef[[3]]*ypc2000 + eq03$coef[[4]]*dty + eq03$coef[[5]]*ctax*dty
ypcg1

# Alternative way I
input = data.frame(ctax=20, ypc2000=10000, dty=35)
ypcg1 = predict(eq03,input,interval="prediction")
ypcg1

# Alternative Way II
eq03_coef = as.matrix(eq03$coef) # 5 by 1
input = c(1, 20, 10000, 35, 20*35) # 1 by 5
ypcg1 = input %*% eq03_coef
ypcg1

#
# Plot the correlation and regression fit chart
#
plot(tax.df$ctax,tax.df$ypcg, pch=19, col="blue", xlim=c(10,45), ylim=c(-1,6),
     xlab="Average Corporate Tax Rate (%) '00-'08", ylab="Average GDP per capita Growth (%) '00-'15") 
abline(eq01, col="red", lwd=3) # add a regression line
abline(h=0, lwd=2) # add a zero line
text(tax.df[35,'ctax'], tax.df[35,'ypcg'], "USA", col="dark blue", cex=.7,pos=1)
text(tax.df[18,'ctax'], tax.df[18,'ypcg'], "Japan", col="dark blue", cex=.7, pos=4)
text(tax.df[17,'ctax'], tax.df[17,'ypcg'], "Italy", col="dark blue", cex=.7, pos=1)
text(tax.df[15,'ctax'], tax.df[15,'ypcg'], "Ireland", col="dark blue", cex=.7, pos=1)
text(tax.df[20,'ctax'], tax.df[20,'ypcg'], "Latvia", col="dark blue", cex=.7, pos=1)
grid()

# (2) Or this simple way
text(39, 0.8,"USA")
text(40, 0.6,"Japan")
text(37, -0.5,"Italy")
text(18, 4.7,"Latvia")
text(15, 2.5,"Ireland")

# (3) ggplot
library(ggplot2)
ggplot(tax.df, aes(x=ctax, y=ypcg))+
  geom_point(size = 3, color ='blue')+
  geom_line(aes(y = predict(eq01)), color="red")+
  geom_text(data=filter(tax.df, country%in%c("United States", "Japan", "Italy","Latvia", "Ireland")),
            aes(label=country))

# (4) ggplot II
library("ggrepel")
ggplot(tax.df, aes(x=ctax, y=ypcg))+
  geom_point(size = 3, color ='blue')+
  geom_line(aes(y = predict(eq01)), color="red", lwd=1.5)+
  geom_text_repel(aes(label=country), size=3)

ggplot(tax.df, aes(x=ctax, y=ypcg))+
  geom_point(size = 3, color ='blue')+
  geom_line(aes(y = predict(eq01)), color="red", lwd=1.5)+
  geom_text_repel(data=filter(tax.df, country%in%c("United States", "Japan", "Italy","Latvia", "Ireland")),
                  aes(label=country), size=3)
#
# In Search of the Best Model
#
corrplot(cor(tax.df[,-1]), type="lower")

eq04 = lm(ypcg ~ ctax*dty+ypc2000+trade, data=tax.df)
summary(eq04) #R2=0.6355
eq05 = lm(ypcg ~ ctax*dty+ypc2000+trade+ihc, data=tax.df)
summary(eq05) #R2=0.6799. 
eq06 = lm(ypcg ~ ctax*dty+ypc2000+trade+ihc+y2000, data=tax.df)
summary(eq06) #R2=0.6747
eq07 = lm(ypcg ~ ctax+dty+ypc2000+trade+ihc+y2000, data=tax.df)
summary(eq07) #R2=0.6314

plot(tax.df$ypc2000,tax.df$ypcg, pch=19)   # looks like a quadratic relationship 
plot(log(tax.df$ypc2000),tax.df$ypcg, pch=19)
eq08 = lm(ypcg ~ ctax*dty+log(ypc2000)+trade+ihc+y2000, data=tax.df)
summary(eq08) #R2=0.739.  This is the best model! 

eq08a = lm(ypcg ~ ctax*dty+log(ypc2000)+ihc, data=tax.df)
summary(eq08a) 

# An alternative way: using "regsubsets" but with no interaction variables
eq09 = regsubsets(ypcg~.-country,tax.df)
sum.eq09=summary(eq09)
sum.eq09

which.max(sum.eq09$adjr2)  
which.min(sum.eq09$cp) 
which.min(sum.eq09$bic)

coef(eq09,4)
coef(eq09,6)
eq09a = lm(ypcg ~ ctax+ypc2000+trade+ihc, tax.df)
summary(eq09a)
