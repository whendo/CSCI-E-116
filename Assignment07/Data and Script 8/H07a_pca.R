######################################################################################################
# H07a Principal Component Analysis
# By William Yu, UCLA Anderson Forecast
# Reference: http://mathworld.wolfram.com/Eigenvalue.html
# 10/17/2023
##################################################################################################### 
##
## Principal Component Analysis / Factor Analysis
##
library(dplyr) 
?USArrests
USArrests=USArrests
str(USArrests)
head(USArrests)
states=row.names(USArrests)
states
apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

pr.out=prcomp(USArrests, scale=TRUE)
# Scale=T means standardize or normalize the different scales of variables
summary(pr.out)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation  # loading factor
pr.out$x

# Alternative way: Eigenvalues / Eigenvectors (V)
# Eigenvalues are a set of scalars that are characteristic roots of a linear 
# system of equations: AV=ZV --> (A-ZI)V=0
arrest_scaled = apply(USArrests, 2, scale) # scale the data
arrest_cov = cov(arrest_scaled) # calculate the covariance matrix (X'X)
arrest_eigen = eigen(arrest_cov) # calculate the eigenvalues of the matrix
arrest_eigen   # See the eigen values and eigen vectors
# (50 by 4 vars) (4 vars by 4 PCs) = (50 by 4 PCs)
PC = as.matrix(arrest_scaled) %*% arrest_eigen$vectors
PC

library(corrplot)
corrplot(cor(USArrests), type="lower", method="number")
corrplot(cor(PC), type="lower", method="number")

biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation  # In R, eigenvectors are negative in default
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
# The Proportion of Variance Explained
pve=pr.var/sum(pr.var)
pve
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
par(mfrow=c(1,1))

##
## Principal Components Regression
##
install.packages("pls")
library(pls)
library(ISLR)

Hitters = Hitters

x=model.matrix(Salary~.,Hitters)[,-1]
y=na.omit(Hitters$Salary)

grid=10^seq(10,-2,length=100)
set.seed(1)

train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
names(pcr.fit)
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

