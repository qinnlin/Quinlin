






rm(list=ls())
library('fGarch')
sp500 <- read.csv('SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:1000], trace = FALSE)
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
sigma  <- result@sigma.t
Retstand <- Ret[1:1000]/sigma
sigmapred <- sqrt(omega + alpha*Ret[1000]^2 + beta*sigma[1000]^2)

sigmapred/sqrt(omega/(1-alpha-beta))


MC <- 10000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
	shock[, i] <- sample(Retstand, MC, replace=T) 
}


ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
  sigmapredMC <- sigmapred
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
  }
}


ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
	ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}




### Value at Risk
VaRMC     <- numeric(T)
for (i in 1:T){
  VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.01)
}
par(mfrow=c(1,2))
plot(VaRMC, type='l', lwd=4)
plot(VaRMC/sqrt(1:T)/VaRMC[1], type='l', lwd=4)




### Expected Shortfall
ESMC     <- numeric(T)
for (i in 1:T){
  indES    <- which(ReturnMCT[,i] <= -VaRMC[i])
  ESMC[i] <- -mean(ReturnMCT[indES,i])
}
par(mfrow=c(1,2))
plot(ESMC, type='l', lwd=4)
plot(ESMC/sqrt(1:T)/ESMC[1], type='l', lwd=4)





























