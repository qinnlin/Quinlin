

##Question: compare the correlation between stock market and bond market

### Download stock prices from Yahoo Finance
rm(list=ls())

library('quantmod')

# download data #we compare them only in panic period 
getSymbols('^GSPC', from ="2020-02-19", to = "2020-05-01")
price <- GSPC[,6]
stock   <- diff(log(price))
stock   <- as.numeric(stock[-1])

getSymbols('VFISX', from ="2020-02-19", to = "2020-05-01") #a kind of bond
price <- VFISX[,6]
bill   <- diff(log(price))
bill   <- as.numeric(bill[-1])


#cumsum of log return is cumulative performance in this period; taking exp is raw return
plot(exp(cumsum(stock)), type='b', ylim=c(0.6, 1.1))
points(exp(cumsum(bill)), type='b', col='red')
#bill return is high, safe place, not change




##lecture 7, correlation
# download data
getSymbols('^GSPC', from ="2000-01-01", to = "2020-05-01")
price <- GSPC[,6]
stock   <- diff(log(price))
stock   <- as.numeric(stock[-1])

getSymbols('VFISX', from ="2000-01-01", to = "2020-05-01")
price <- VFISX[,6]
bill   <- diff(log(price))
bill   <- as.numeric(bill[-1])



### Dynamic conditional correlation
library('fGarch')
fit1  <- garchFit( formula = ~garch(1, 1), data = stock, trace = FALSE)
sigma1 <- fit1@sigma.t
stockstand <- stock/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = bill, trace = FALSE)
sigma2 <- fit2@sigma.t
billstand <- bill/sigma2

T           <- length(stock)
alpha       <- 0.05
beta        <- 0.9
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)
q11lr       <- mean(stockstand^2)
q12lr       <- mean(stockstand*billstand)
q22lr       <- mean(billstand^2)

for (i in 2:T){
  q11[i] <- q11lr + alpha*(stockstand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  q12[i] <- q12lr + alpha*(stockstand[i-1]*billstand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  q22[i] <- q22lr + alpha*(billstand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
}

GarchCorr <- q12/sqrt(q11*q22)
plot(GarchCorr, type='l', col='blue', lwd=4)
abline(h=0, lwd=2)

#bond and stock market is negetive, and very negative in bad time.
