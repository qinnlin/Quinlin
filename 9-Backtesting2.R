
#### Backtesting


rm(list=ls())                                  

sp500 <- read.csv('1-sp500.csv', header=TRUE)
ret   <- diff(log(sp500$Close))
N     <- length(ret)


#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(N)
var1_fhs  <- numeric(N)
var1_garch <- numeric(N)
var1_t   <- numeric(N)

for (i in 501:N){
  retwindow   <- ret[(i-500):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$par[2]
  alpha <- fit2@fit$par[3]
  beta  <- fit2@fit$par[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*ret[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE, cond.dist='std')
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  shape <- fit2@fit$coef[5]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_t[i] <- -qt(0.01, df=shape)/sqrt(shape/(shape-2))*sigmapred 
}



violation <- numeric(N-500)
ind       <- which(ret[501:N] <= -var1_t[501:N])
violation[ind] <- 1
plot(violation, type='l')
pii <- sum(violation)/length(violation)

##independent testing
T  <- length(violation)
violation <- violation[1:(T-1)]*10 +  violation[2:T]
T00 <- length(which(violation==0))
T01 <- length(which(violation==1))
T10 <- length(which(violation==10))
T11 <- length(which(violation==11))
pii01 <- T01/(T00 + T01)
pii11 <- T11/(T10 + T11)

Lpii <- (1-pii)^T00*pii^T01*(1-pii)^T10*pii^T11#null
Lpii1 <- (1-pii01)^T00*pii01^T01*(1-pii11)^T10*pii11^T11#alternative

LR  <- -2*log(Lpii/Lpii1)
LR
1-pchisq(LR, df=1)

##all 4 test pass lower the better,but garch and student t are better than  hs and fhs
#hs is slowing movement,thus more pii11, 








