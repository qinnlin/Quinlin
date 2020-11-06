



### Download stock prices from Yahoo Finance
rm(list=ls())

library('quantmod')

# download data
getSymbols('BAC', from ="2010-01-01", to = "2019-04-02")
price <- BAC[,6]
BAC   <- diff(log(price))
BAC   <- as.numeric(BAC[-1])

getSymbols('VBTIX', from ="2010-01-01", to = "2019-04-02")
price <- VBTIX[,6]
bond   <- diff(log(price))
bond   <- as.numeric(bond[-1])






### Dynamic conditional correlation
library('fGarch')
fit1  <- garchFit( formula = ~garch(1, 1), data = BAC, trace = FALSE)
sigma1 <- fit1@sigma.t
BACstand <- BAC/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = bond, trace = FALSE)
sigma2 <- fit2@sigma.t
bondstand <- bond/sigma2

T           <- length(BAC)
alpha       <- 0.05
beta        <- 0.9
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)
q11lr       <- mean(BACstand^2)
q12lr       <- mean(BACstand*bondstand)
q22lr       <- mean(bondstand^2)

for (i in 2:T){
  q11[i] <- q11lr + alpha*(BACstand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  q12[i] <- q12lr + alpha*(BACstand[i-1]*bondstand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  q22[i] <- q22lr + alpha*(bondstand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
}

GarchCorr <- q12/sqrt(q11*q22)
plot(GarchCorr, type='l', col='blue', lwd=4)
abline(h=0, lwd=2)

