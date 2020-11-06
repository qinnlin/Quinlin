



### Garch simulation and maximum likelihood estimation

rm(list=ls())
#install.packages('fGarch')
library('fGarch')
spec   <- garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta=0.8))
dd     <- garchSim(spec, n = 100000)
plot(dd)
fit1   <- garchFit( formula = ~garch(1,2), data=dd, trace=FALSE)
sigma  <- sqrt(fit1@h.t)
#

### Maximumum likelihood estimation on SP500
rm(list=ls())
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)  #trace = TRUE  process of finding the maximum likelihood estimation 
fit2
sigma <- sqrt(fit2@h.t) #fitted sigma

plot(sigma, type='l')

##

t<-length(Ret)

omega<-fit2@fit@coef[2]
a<-fit2@fit@coef[3]
b<-fit2@fit@coef[4]

sqrt(omega+a*Ret[T]^2+b*sigma[t]^2)# garch forcast sigma t+1



### Maximum likelihood estimation for AAPL
### Download stock prices from Yahoo Finance
rm(list=ls())
library(quantmod)
getSymbols('JPM', from ="1990-01-03", to = "2017-02-21")
JPM <- JPM$JPM.Adjusted
Ret <- diff(log(JPM))
Ret <- Ret[-1,] #omit last NA
fit3 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit3@h.t)
#sigma<-fit3@sigma.t   same as sigma
plot(sigma, type='l')






### Leverage effect
fit3 <- garchFit( formula = ~garch(1, 1), data = Ret, delta=2, leverage=TRUE, trace = FALSE)






