






rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:500], trace = FALSE)
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
sigma <- result@sigma.t
sigmapred <- sqrt(omega + alpha*Ret[500]^2 + beta*sigma[500]^2) ##sigma t+1

sigmapred/sqrt(omega/(1-alpha-beta))## compare with longrun sigma

sigmapred <- sqrt(omega/(1-alpha-beta))*2 ##if the begining sigma is 2 times bigger than long run sigma  downward sloping 
                                          ##change to *0.5   upward sloping

##step 2
MC <- 10000
T  <- 500
shock <- matrix(0, MC, T) #create a shock(Z) matrix, all zero
for (i in 1:T){
	shock[, i] <- rnorm(MC, 0, 1)
}
##colume by colume, because column only 500 times, row by row need 10000times, save time.


ReturnMC <- matrix(NA, MC, T) #return matrix
for (i in 1:MC){
	sigmapredMC <- sigmapred
	for (j in 1:T){
		ReturnMC[i, j] <- sigmapredMC*shock[i, j]
		sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
	}
}

##step 3
ReturnMCT <- matrix(NA, MC, T) ##cumulated returns
for (i in 1:MC){
	ReturnMCT[i, ] <- cumsum(ReturnMC[i, ]) ##cumulative sum
}



###step 4 Value at Risk
VaRMC     <- numeric(T)
for (i in 1:T){
	VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.05)
}
par(mfrow=c(1,2))
plot(VaRMC, type='l', lwd=4)
plot(VaRMC/sqrt(1:T)/VaRMC[1], type='l', lwd=4)



### Expected Shortfall
ESMC     <- numeric(T)
for (i in 1:T){
	indES    <- which(ReturnMCT[,i] <= -VaRMC[i]) ##which tells the location
	ESMC[i]  <- -mean(ReturnMCT[indES,i])
}
par(mfrow=c(1,2))
plot(ESMC, type='l', lwd=4)
plot(ESMC/sqrt(1:T)/ESMC[1], type='l', lwd=4)



















