

### Decompsition of Correlation Matrix

rm(list=ls())
A <- matrix(c(1, -0.4, -0.4, 1), 2, 2) #correlation matrix
Tau <- chol(A) #cholesty decomposition
t(Tau)%*%Tau  #transpose(t) matrix mutiply t

T <- 1000000
shockA  <- rnorm(T, 0, 1)
shockB  <- rnorm(T, 0, 1)
shock   <- cbind(shockA, shockB) #column bind

cor(shock)
##step two
shockNew <- shock%*%Tau
cor(shockNew)








### Cholesky decompostion for a 3*3 matrix
rm(list=ls())
A <- matrix(c(1, 0.9, 0.6, 0.9, 1, 0.4, 0.6, 0.4, 1), 3, 3)
Tau <- chol(A)
t(Tau)%*%Tau

















