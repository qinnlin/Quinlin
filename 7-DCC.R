




### Modeling covariance and correlation

rm(list=ls())
library('fGarch')
retdata <- read.csv('7-SP500Tnote.csv', head=TRUE)
retsp500   <- diff(log(retdata$sp500))
rettnote   <- diff(log(retdata$tnote)) 
T            <- length(retsp500)

plot(retsp500, type='l', lwd=2, col='blue')
points(rettnote, type='l', lwd=2, col='red')



### Rolling window  ##simple covariance model
windowlength <- 25
rollingCov   <- numeric(T)

for (i in 25:T){
  rollingCov[i] <- sum(retsp500[(i-24):i]*rettnote[(i-24):i])/25
}

plot(rollingCov, type='l', col='blue', lwd=4) #show covariance
##why 2008 have negative cov: sell stock and buy bond,so stock price down while bond price up; negative movement.


### Exponentially smoothed covariance #risk metrics model #doesnot matter about the first number
lambda      <- 0.94

exponentialCov  <- numeric(T)
for (i in 2:T){
  exponentialCov[i] <- (1-lambda)*retsp500[i-1]*rettnote[i-1] + lambda*exponentialCov[i-1]
}

plot(exponentialCov, type='l', col='blue', lwd=2)
points(rollingCov, type='l', col='red', lwd=2)

### Garch Covariance
omega   <- 1e-6
alpha   <- 0.05
beta    <- 0.9
GarchCov  <- numeric(T)
for (i in 2:T){
  GarchCov[i] <- omega + alpha*retsp500[i-1]*rettnote[i-1] + beta*GarchCov[i-1]
}

plot(GarchCov, type='l', col='blue', lwd=2)
points(rollingCov, type='l', col='red', lwd=2)





### Dynamic conditional correlation ##garch version 
library('fGarch')
fit1  <- garchFit( formula = ~garch(1, 1), data = retsp500, trace = FALSE)
sigma1 <- fit1@sigma.t
retsp500stand <- retsp500/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = rettnote, trace = FALSE)
sigma2 <- fit2@sigma.t
rettnotestand <- rettnote/sigma2


alpha       <- 0.05 #given
beta        <- 0.9
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)
q11lr       <- mean(retsp500stand^2)
q12lr       <- mean(retsp500stand*rettnotestand)
q22lr       <- mean(rettnotestand^2)

for (i in 2:T){
  q11[i] <- q11lr + alpha*(retsp500stand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  q12[i] <- q12lr + alpha*(retsp500stand[i-1]*rettnotestand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  q22[i] <- q22lr + alpha*(rettnotestand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
}

GarchCorr <- q12/sqrt(q11*q22)
plot(q12,type='l',col='red') #beyond -1 not like covariance
plot(GarchCorr, type='l', col='blue', lwd=4)


###



