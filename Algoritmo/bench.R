# oracle, uma especie de random walk
a0=0; oracle <- vector(); tx = 0
for (t in 2:nrow(reward)) {
  a1 <- which.max(reward[t-1,2:4])
  oracle[t-1] <- as.numeric(reward[t,a1+1] - ifelse(a0==a1,0,tx))
  a0 <- a1
}

plot(ts(oracle))
plot(ts(cumsum(oracle)))
sum(oracle)


# oracle, uma especie de random walk
a0=0; oracle_ <- vector(); tx = .005
for (t in 2:nrow(reward)) {
  a1 <- which.max(reward[t-1,2:4])
  oracle_[t-1] <- as.numeric(reward[t,a1+1] - ifelse(a0==a1,0,tx))
  a0 <- a1
}
plot(ts(oracle_))
plot(ts(cumsum(oracle_)))
sum(oracle_)

Q <- readRDS("Matriz_Q.RDS")$`Matriz Q`
Q_ <- readRDS("Matriz_Q_custo.RDS")$`Matriz Q`

r_Q <- vector();state <- states[1]; action_0 <- rep(0,3)
for(t in 3901:length(states)){ action <- which.max(Q[state, ])
     r_Q[t-3900] <- CalcReward(action_0, action, t, 0); state <- states[t]; action_0 <- action }
sum(r_Q)
plot(ts(cumsum(r_Q)))


r_Q_ <- vector();state <- states[1]; action_0 <- rep(0,3)
for(t in 3901:length(states)){ action <- which.max(Q_[state, ])
r_Q_[t-3900] <- CalcReward(action_0, action, t, tx); state <- states[t]; action_0 <- action }
sum(r_Q_)
plot(ts(cumsum(r_Q_)))


r_O <- oracle[3900:length(oracle)]
sum(r_O); plot(ts(cumsum(r_Q)))

r_O_ <- oracle_[3900:length(oracle_)]
sum(r_O_); plot(ts(cumsum(r_O_)))

rewc1 <- cumsum(rew1)
rewc2 <- cumsum(rew2)
rewc3 <- cumsum(rew3)
rewc4 <- cumsum(rew4)
rewc5 <- cumsum(rew5)

data <- data.frame(Time = (coins$Time[3901:length(states)]),
                   r_Q = cumsum(r_Q), r_Q_ = cumsum(r_Q_),
                   r_O = cumsum(r_O), r_O_ = cumsum(r_O_) )
                   
data$Time <- anytime::anytime(data$Time) 
data$Time <- lubridate::floor_date(data$Time , '3 hours')


data <- data %>% gather(policy, r, -Time) %>% mutate()
ggplot(data, aes(x=Time, y=r, group=policy, colour=policy)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Política de Seleção",
                                     labels=c("Oracle", "Oracle*", "Q-learning","Q-learning*")) +  
  xlab("Ano")+ ylab("Retorno Acumulado") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15)) + geom_hline(yintercept = 0, linetype="dashed")


# -----------------------------------------------------------------------
data %>% group_by(policy) %>% summarise(ret_mean = mean(r),
                                        vol      = var(r),
                                        sharpeR  = mean(r)/sd(r))


# =======================================================================

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


  
  
