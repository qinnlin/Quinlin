


### Diagnostic check of GARCH models: Autocorrelation of standardized returns

rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))



fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- sqrt(fit2@h.t) ###=segma.t  package improvment
#sigma <- fit2@sigma.t



Retstand <- Ret/sigma
par(mfrow=c(2,2))
acf(Ret,20)
acf(Retstand,20)
acf(Ret^2, 20)
acf(Retstand^2, 20)  ##strong test hat_retrun^2  can see the change


