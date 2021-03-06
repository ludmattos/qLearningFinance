# =========================================================== #
# ===      Q learning for portifolio selection            === #
# =========================================================== #
# 15/05/2018
rm(list = ls())
library(tidyverse)

# Proporção a ser alocada, AÇÕES DO MODELO
sequencia <- seq(0,1, by=.1)
actions <- expand.grid(sequencia,sequencia)
colnames(actions) <- c("BTC", "ETH")
actions <- actions %>% mutate(LTC = round(1 - (BTC + ETH), digits = 2)) %>%
  filter(BTC + ETH + LTC == 1 & LTC >= 0) 


# Minhas ações são as alocações
# Meus estados devem estar associados aos valores dos retornos. Pundakar utilizou
# uma combinação da informação se o retorno foi negativo ou não, por ser dois ativos
# isto gerou 4 estados. Mas e se eu considerasse uma informação de qual ativo foi o 
# maior? e etc... Qual a consequência de se ter muitos estados?
coins <- readRDS("Coins.RDS") 
coins <- coins[c("Time","BTC","LTC","ETH")] %>% na.omit() %>% gather(Coin, Close, -Time) %>%
  group_by(Coin) %>%
  mutate(Retorno = (Close - lag(Close))/lag(Close))   ##################

states <- na.omit(coins) %>% mutate(state = case_when(Retorno >0 ~ "up",
                                                      TRUE       ~ "down")) %>%
  select(-Close,-Retorno) %>% spread(Coin, state) %>% na.omit() %>%
  mutate(state = paste0(BTC,"-",LTC,"-",ETH))

table(states$state) # mostra que as moedas são muito correlacionadas 

# Calculando os retornos q irão compor as recompensas
reward <- na.omit(coins) %>% select(-Close) %>% spread(Coin, Retorno) %>% 
  na.omit()

# =========================== Gráficos ============================================= #
ggplot(coins, aes(x=Time, y=(Retorno), group=Coin, colour=Coin)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Criptomoeda",
                                     labels=c("Bitcoin", "Ethereum", "Litcoin")) +  
  xlab("Ano") + ylab("Preço de Fechamento") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))


ggplot(coins, aes(x=Time, y=Close, group=Coin, colour=Coin)) + geom_line(lwd=1) +
  theme_bw() + scale_colour_discrete(name="Criptomoeda",
                                     labels=c("Bitcoin", "Ethereum", "Litcoin")) +  
  xlab("Ano")+ ylab("Preço de Fechamento") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))

# Q-Learning --------------------------------------------------------------
# 15 actions with 2^3 = 8 states and Rewards = Returns*allocation
# Q tem dimensões 8X15

# O INVESTIDOR NÃO ALTERA O MERCADO!
states <- unlist(states[,5])
n_action <- actions %>% transmute(paste(BTC, ETH, LTC, sep = "-")) %>% .[,1]
n_state <- unique(states)

# retorna a recompensa do retorno associado a ação no tempo t
# taxa de transação igual a 1%
CalcReward <- function(action_0, action, t, tx = 0){
  if(sum(action_0) == 0){action_0 <- rep(0,3)
  }else action_0 <- actions[action_0,]
  trans <- actions[action,] - action_0
  r <- sum(reward[t,2:4] * actions[action,] - tx*unlist(ifelse(trans > 0, trans, 0)) )
  return(r)
}



learnEpisode <- function(s0, t_initial, t_final,
                         epsilon, learning_rate, dicount, Q, tx=0){
  state <- s0 # set cursor to initial state
  action_0 <- 0 # initial action, nothing allocated
  for (t in (t_initial+1):t_final) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      action <- sample(1:length(n_action), 1) # pick random action
    } else {
      action <- which.max(Q[state, ]) # pick first best action
    }
    # get reward from environment
    response <- CalcReward(action_0, action, t, tx)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response + dicount*max(Q[states[t], ]) - Q[state, action]) # colocar discount factor
    action_0 <- action
    state <- states[t] # move to next state
  }
  return(Q)
}


Qlearning <- function(n, t_initial=1, t_final=1000, 
                      epsilon, learning_rate, dicount, tx=0) {
  rew <- vector()
  s0 = states[t_initial] # First observable state
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(n_state), ncol=length(n_action),
              dimnames=list(n_state, n_action))
  # s11 <- vector()
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s0, t_initial, t_final, epsilon, learning_rate, dicount, Q, tx)

    rew_ <- vector(); state <- states[t_initial]; action_0 <- rep(0,3)
    for (t in 2:length(states)){ action <- which.max(Q[state, ])
      rew_[t-1] <- CalcReward(action_0, action, t)
      state <- states[t]; action_0 <- action
      
      rew[i] <- sum(rew_)
    }
  }
  
  return(list(`Matriz Q` = Q, `Recompensas` = rew))
}


#Choose learning parameters
epsilon <- 0.1
learning_rate <- 0.1
dicount <- .2
tx=.005
#Calculate state-action function Q after 1000 episodes
set.seed(0)
Q <- Qlearning(100, 1, 3900, epsilon, learning_rate, dicount)
# (Q$Recompensas)
plot(ts(Q$Recompensas))

saveRDS(Q, "Matriz_Q.RDS")


set.seed(0)
Q_ <- Qlearning(100, 1, 3900, epsilon, learning_rate, dicount, tx)
# (Q$Recompensas)
plot(ts(Q_$Recompensas))

saveRDS(Q_, "Matriz_Q_custo.RDS")


# ----------------------------------------------------------------------------------------------------------
# set.seed(0)
# Q10 <- Qlearning(10000, 1, 3900, epsilon, learning_rate, dicount)
# # (Q$Recompensas)
# plot(ts(Q$Recompensas))
# 
# saveRDS(Q10, "Matriz_Q.RDS")
# 
# 
# set.seed(0)
# Q_10 <- Qlearning(10000, 1, 3900, epsilon, learning_rate, dicount, tx)
# # (Q$Recompensas)
# plot(ts(Q$Recompensas))

saveRDS(Q_10, "Matriz_Q_custo.RDS")
# ----------------------------------------------------------------------------------------------------------

# ================================================================================================ #
library(mailR)
sender <- "unb.rafa@gmail.com" # Replace with a valid address
recipients <- c("rafa.unb.2012@gmail.com") # Replace with one or more valid addresses
email <- send.mail(from = sender,
                   to = recipients, subject="Q-LEARNING",
                   body = "Body of the email",
                   smtp = list(host.name = "aspmx.l.google.com", port = 25),
                   authenticate = FALSE,
                   send = T,
                   attach.files = list.files(pattern = "*.RDS"),
                   debug = T)
# ================================================================================================ #


# rew <- vector()
# state <- states[1] # set cursor to initial state
# for (t in 2:length(states)) {
#   action_0 <- rep(0,3)
#   action <- which.max(Q$`Matriz Q`[state, ]) # pick first best action
#   rew[t-1] <- CalcReward(action_0, action, t)
#   state <- states[t] # move to next state
#   action_0 <- action
# }
# sum(rew) #*100
# plot(ts(cumsum(rew[3901:6066])))

plot(ts(cumsum(rew)))

sum((rew[3901:6066]))

saveRDS(Q, "MatrizQ.RDS")
#Optimal policy
# note: problematic for states with ties
n_action[max.col(Q$`Matriz Q`)]

# ======================================= Base comparativa ======================================= #
base <- na.omit(coins) %>% filter(Time>="2017-05-19") %>% group_by(Coin) %>% mutate(ret_acum = cumsum(Retorno))
ggplot(base, aes(x=Time, y=ret_acum, group=Coin, colour=Coin)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Criptomoeda",
                                     labels=c("Bitcoin", "Ethereum", "Litcoin")) +  
  xlab("Ano") + ylab("Preço de Fechamento") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))

ETH <- filter(base, Coin == "ETH") %>% ungroup() %>% select(Retorno) %>% unlist()
# ================================================================================================ #

set.seed(0)
Q1 <- Qlearning(1, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q1, "MatrizQ1.RDS")

Q2 <- Qlearning(100, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q2, "MatrizQ2.RDS")

set.seed(0)
Q3 <- Qlearning(1000, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q3, "MatrizQ3.RDS")

set.seed(0)
Q4 <- Qlearning(5000, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q4, "MatrizQ4.RDS")

set.seed(0)
Q5 <- Qlearning(10000, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q5, "MatrizQ5.RDS")

set.seed(0)
Q6 <- Qlearning(30000, 1, 3900, epsilon, learning_rate, dicount)
saveRDS(Q6, "MatrizQ6.RDS")
#===========================================================

for (i in 1:5) {
  assign(paste0("Q",i), readRDS(paste0("MatrizQ",i,".RDS")) )
}


rew1 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q1[state, ])
  rew1[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew1)
plot(ts(cumsum(rew1)))

rew1 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q1[state, ])
rew1[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew1)
plot(ts(cumsum(rew1)))

rew2 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q2[state, ])
rew2[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew2)
plot(ts(cumsum(rew2)))

rew3 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q3[state, ])
rew3[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew3)
plot(ts(cumsum(rew3)))

rew4 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q4[state, ])
rew4[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew4)
plot(ts(cumsum(rew4)))

rew5 <- vector();state <- states[1]
for (t in 3901:length(states)) { action <- which.max(Q5[state, ])
rew5[t-3900] <- CalcReward(action, t); state <- states[t] }
sum(rew5)
plot(ts(cumsum(rew5)))


# Q <- Qlearning(1, 1, 3900, epsilon, learning_rate)
# rewq <- vector();state <- states[1]
# for (t in 3901:length(states)) { action <- which.max(Q[state, ])
# rewq[t-3900] <- CalcReward(action, t); state <- states[t] }
# sum(rewq)
# plot(ts(cumsum(rewq)))


rewc1 <- cumsum(rew1)
rewc2 <- cumsum(rew2)
rewc3 <- cumsum(rew3)
rewc4 <- cumsum(rew4)
rewc5 <- cumsum(rew5)


data <- data.frame(cbind(Time = (coins$Time[3901:length(states)]),
                         rewc1,rewc3,rewc4,rewc5))

data$Time <- anytime::anytime(data$Time) 
# data$Time <- lubridate::floor_date(data$Time , '3 hours')


data <- data %>% gather(iter, rew, -Time) %>% mutate()
ggplot(data, aes(x=Time, y=rew, group=iter, colour=iter)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Número de Iterações",
                                     labels=c("1", "1000", "5000","10000")) +  
  xlab("Ano")+ ylab("Retorno Acumulado") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))
# =======================================================================
rew_1 <- vector();state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q1[state, ])
rew_1[t-1] <- CalcReward(action, t); state <- states[t] }

rew_2 <- vector();state <- states[1];for (t in 2:length(states)) { action <- which.max(Q2[state, ])
rew_2[t-1] <- CalcReward(action, t); state <- states[t] }

rew_3 <- vector();state <- states[1];for (t in 2:length(states)) { action <- which.max(Q3[state, ])
rew_3[t-1] <- CalcReward(action, t); state <- states[t] }

rew_4 <- vector();state <- states[1];for (t in 2:length(states)) { action <- which.max(Q4[state, ])
rew_4[t-1] <- CalcReward(action, t); state <- states[t] }

rew_5 <- vector();state <- states[1];for (t in 2:length(states)) { action <- which.max(Q5[state, ])
rew_5[t-1] <- CalcReward(action, t); state <- states[t] }

rewc_1 <- cumsum(rew_1)
rewc_2 <- cumsum(rew_2)
rewc_3 <- cumsum(rew_3)
rewc_4 <- cumsum(rew_4)
rewc_5 <- cumsum(rew_5)


data_ <- data.frame(cbind(Time = (coins$Time[1:(length(states)-1)]),
                         rewc_1,rewc_3,rewc_4,rewc_5))

data_$Time <- anytime::anytime(data_$Time) 
# data$Time <- lubridate::floor_date(data$Time , '3 hours')

data_ <- data_ %>% gather(iter, rew, -Time) %>% mutate()
ggplot(data_, aes(x=Time, y=rew, group=iter, colour=iter)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Número de Iterações",
                                     labels=c("1", "1000", "5000","10000")) +  
  xlab("Ano")+ ylab("Retorno Acumulado") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))

# ======================================= Base comparativa ======================================= #
base_ <- na.omit(coins) %>% group_by(Coin) %>% mutate(ret_acum = cumsum(Retorno))
ggplot(base_, aes(x=Time, y=ret_acum, group=Coin, colour=Coin)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Criptomoeda",
                                     labels=c("Bitcoin", "Ethereum", "Litcoin")) +  
  xlab("Ano") + ylab("Preço de Fechamento") +
  theme(legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=15))
# ================================================================================================ #

