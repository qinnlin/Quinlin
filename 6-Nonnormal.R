




### QQ-plots to reveal non-normality
rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))

fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- fit2@sigma.t

Retstand <- Ret/sigma

par(mfrow=c(1,2))
qqnorm(Ret, main='S&P500 Returns')            ### Unconditional non-normality  #main is title    
qqline(Ret, col='red',lwd=4)            #lwd is wide   ### straight line correspond to normal distribution
##in the middle [-1,1] it follows normal distribution,but two tails are not

qqnorm(Retstand, main='Standardized S&P500 Returns')   ### Conditional non-normality   
qqline(Retstand, col='red',lwd=4)
# retstand diviation is smaller than ret













