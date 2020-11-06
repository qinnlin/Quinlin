
#### Backtesting


rm(list=ls())                                  

sp500 <- read.csv('1-sp500.csv', header=TRUE)
ret   <- diff(log(sp500$Close))
N     <- length(ret)


#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(N) #historical simulation
var1_fhs  <- numeric(N)#filtered historical simulation
var1_garch <- numeric(N)#garch version
var1_t   <- numeric(N)#student T version

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

#backtesting unconditional coverage test
violation <- numeric(N-500)
ind       <- which(ret[501:N] <= -var1_t[501:N])#change to hs.fhs.garch.t;backtesting student t verstion
violation[ind] <- 1 #violation at those location change to 1
plot(violation)
sum(violation)/length(violation)


T  <- length(violation)
T1 <- sum(violation)
T0 <- T - T1
pii <- T1/T #pi hat; estimate coverage rate

Lp <- (1-0.01)^T0*0.01^T1
Lpii <- (1-pii)^T0*pii^T1

LR  <- -2*log(Lp/Lpii)
LR
1-pchisq(LR, df=1)



### Get the cutoff value for the Chi-squared distribution
qchisq(0.95, df=1)#q means quantile


#backtest_coverage is the name of the function;(x,p) are the inputs of the function
#x: hit sequence; P: coverage rate
#outputs:(LR,Pii)
backtest_coverage <- function(x, p){
  T <- length(x)
  T1 <- sum(x)
  T0 <- T - T1
  pii <- T1/T
  
  Lp <- (1-p)^T0*p^T1
  Lpii <- (1-pii)^T0*pii^T1
  
  LR  <- -2*log(Lp/Lpii)
  c(LR, pii)
}


backtest_coverage(violation, 0.01)



### Monte Carlo Simulation #understand the underlying meaning not for test

MC <- 10000
LR_MC <- numeric(MC)
pi_MC <- numeric(MC)
for (i in 1:MC){
  violation <- rbinom(T, 1, 0.01)#rbinom simulate bernoulli random variable; simulate 1and0 with 1 happens 1%, T is the length
  LR_MC[i] <- backtest_coverage(violation, 0.01)[1]
  pi_MC[i] <- backtest_coverage(violation, 0.01)[2]#pi hat
  }

length(which(LR_MC <= LR))/MC

plot(pi_MC, LR_MC, type='l', col='blue', lwd=2)
#when the estimate coverage rate=pi hat too large or too low, it will give you larger LR>3.84,which means fail the unconditional coverage test





