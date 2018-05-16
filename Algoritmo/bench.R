# oracle, uma especie de random walk
a0=0; oracle <- vector(); tx = .00
for (t in 2:nrow(reward)) {
  a1 <- which.max(reward[t-1,2:4])
  oracle[t-1] <- as.numeric(reward[t,a1+1] - ifelse(a0==a1,0,tx))
  a0 <- a1
}

plot(ts(oracle))
plot(ts(cumsum(oracle)))
sum(oracle)

CalcReward <- function(action_0, action, t, tx = 0){
  if(sum(action_0) == 0){action_0 <- rep(0,3)
  }else action_0 <- actions[action_0,]
  trans <- actions[action,] - action_0
  r <- sum(reward[t,2:4] * actions[action,] - tx*unlist(ifelse(trans > 0, trans, 0)) )
  return(r)
}
# =====================================================================
# O benchmark será calculado para 4 modelos: Qlearn e Oracle com e sem
# taxa de transação
# 
# calcular métricas de retorno e sharpe ratio alé do teste SPA
# =====================================================================
devtools::install_github("PedroBSB/mlRFinance")

library(mlRFinance)
D <- matrix(c(rew, rew), ncol=2) %>% na.omit()
#Generate the relative performance benchmark
b <- oracle
#Call the function 
hansen.spa(Dmat=D,bVec=b,typeFunc=1,B=500,geomMean=20)

D <- matrix(c(ETH[-1], rew), ncol=2) %>% na.omit()
#Generate the relative performance benchmark
b <- oracle
#Call the function 
hansen.spa(Dmat=D,bVec=b,typeFunc=1,B=500,geomMean=20)


plot(ts(cumsum(ETH[-1])) )
lines(ts(cumsum(rew)))
lines(ts(cumsum(oracle)))

plot(cumsum(rew-oracle))
# rm(list=ls())
#Invoke the libraries
library(mlRFinance)
#Generate some fake relative performance variables (NxK)
D<-matrix(runif(10000,-1,+1),ncol=10,nrow = 10)
#Generate the relative performance benchmark
b<-runif(1000,-1,+1)
#Call the function 
hansen.spa(Dmat=D,bVec=b,typeFunc=1,B=500,geomMean=20)


rm(list=ls())
#Invoke the libraries
library(mlRFinance)
#Generate some fake relative performance variables (Nx2)
D<-matrix(sample(c(-1,+1), 10000, replace = T),ncol=2)
#Generate the relative performance benchmark
a  <- runif(3000, -1,+1)
a1 <- runif(3000, -1,1)
b <- sapply(a, function(x)x-runif(1,0,.05))
D <-matrix(c(a,a), ncol = 2, byrow = T)

#Call the function 
hansen.spa(Dmat=D,bVec=b,typeFunc=1,B=100,geomMean=20)


  
  
