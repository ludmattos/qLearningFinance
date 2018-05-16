library(tidyverse)

sequencia <- seq(0,1, by=.25)
actions <- expand.grid(sequencia,sequencia)
colnames(actions) <- c("BTC", "ETH")
actions <- actions %>% mutate(LTC = 1 - (BTC + ETH)) %>%
  filter(BTC + ETH + LTC == 1 & LTC >= 0) 


# Minhas ações são as alocações
# Meus estados devem estar associados aos valores dos retornos. Pundakar utilizou
# uma combinação da informação se o retorno foi negativo ou não, por ser dois ativos
# isto gerou 4 estados. Mas e se eu considerasse uma informação de qual ativo foi o 
# maior? e etc... Qual a consequência de se ter muitos estados?
coins <- readRDS("Coins.RDS")
coins <- coins[c("Time","BTC","LTC","ETH")] %>% gather(Coin, Close, -Time) %>%
  group_by(Coin) %>%
  mutate(Retorno = Close - lag(Close))

states <- na.omit(coins) %>% mutate(state = case_when(Retorno >0 ~ "up",
                                                      TRUE       ~ "down")) %>%
  select(-Close,-Retorno) %>% spread(Coin, state) %>% na.omit() %>%
  mutate(state = paste0(BTC,"-",LTC,"-",ETH))

table(states$state) # mostra que as moedas são muito correlacionadas 

reward <- na.omit(coins) %>% select(-Close) %>% spread(Coin, Retorno) %>% 
  na.omit()

ggplot(coins, aes(x=Time, y=Retorno, group=Coin, colour=Coin)) + geom_line() +
  theme_bw() + scale_colour_discrete(name="Criptomoeda",
                                     labels=c("Bitcoin", "Ethereum", "Litcoin")) +  
  xlab("Ano")+ ylab("Preço de Fechamento") +
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
# 15 actions with 2^3 = 8 states and Rewards = Returns
# Q tem dimensões 8X15

# O INVESTIDOR NÃO ALTERA O MERCADO!
states <- unlist(states[,5])
n_action <- actions %>% transmute(paste(BTC,ETH,LTC, sep = "-")) %>% .[,1]
n_state <- unique(states)

CalcReward <- function(action, t){
  r <- sum(reward[t,2:4]*actions[action,])
  return(r)
}

Qlearning <- function(n, t_initial=1, t_final=1000, 
                      epsilon, learning_rate) {
  s0 = states[t_initial] # First observable state
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(n_state), ncol=length(n_action),
              dimnames=list(n_state, n_action))
  # s11 <- vector()
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s0, t_initial, t_final, epsilon, learning_rate, Q)
    # s11[i] <- max(Q[1,])
  }
  return(Q)
}

learnEpisode <- function(s0, t_initial, t_final,
                         epsilon, learning_rate, Q) {
  state <- s0 # set cursor to initial state
  for (t in (t_initial+1):t_final) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      action <- sample(1:15, 1) # pick random action
    } else {
      action <- which.max(Q[state, ]) # pick first best action
    }
    # get reward from environment
    response <- CalcReward(action, t)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response + max(Q[states[t], ]) - Q[state, action])
    state <- states[t] # move to next state
  }
  return(Q)
}

#Choose learning parameters
epsilon <- 0.1
learning_rate <- 0.1

#Calculate state-action function Q after 1000 episodes
set.seed(0)
Q <- Qlearning(1, 1, 3900, epsilon, learning_rate)
Q

rew <- vector()
state <- states[1] # set cursor to initial state
for (t in 2:length(states)) {
    action <- which.max(Q[state, ]) # pick first best action
  # get reward from environment
  rew[t-1] <- CalcReward(action, t)
  state <- states[t] # move to next state
}
sum(rew)
plot(ts(cumsum(rew)))

saveRDS(Q, "MatrizQ.RDS")
#Optimal policy
# note: problematic for states with ties
n_action[max.col(Q)]


#Calculate state-action function Q after 1000 episodes
set.seed(0)
Q1 <- Qlearning(1, 1, 3900, epsilon, learning_rate)
saveRDS(Q1, "MatrizQ1.RDS")

Q2 <- Qlearning(2, 1, 3900, epsilon, learning_rate)
saveRDS(Q2, "MatrizQ2.RDS")

set.seed(0)
Q3 <- Qlearning(1000, 1, 3900, epsilon, learning_rate)
saveRDS(Q3, "MatrizQ3.RDS")

set.seed(0)
Q4 <- Qlearning(5000, 1, 3900, epsilon, learning_rate)
saveRDS(Q4, "MatrizQ4.RDS")

set.seed(0)
Q5 <- Qlearning(10000, 1, 3900, epsilon, learning_rate)
saveRDS(Q5, "MatrizQ5.RDS")

n_action[max.col(Q1)]
n_action[max.col(Q2)]
n_action[max.col(Q3)]
n_action[max.col(Q4)]
n_action[max.col(Q5)]

rew <- vector()
state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q1[state, ]); rew[t-1] <- CalcReward(action, t)
  state <- states[t] }; sum(rew); plot(ts(cumsum(rew)))


rew <- vector()
state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q2[state, ]); rew[t-1] <- CalcReward(action, t)
state <- states[t] }; sum(rew); plot(ts(cumsum(rew)))


rew <- vector()
state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q3[state, ]); rew[t-1] <- CalcReward(action, t)
state <- states[t] }; sum(rew); plot(ts(cumsum(rew)))


rew <- vector()
state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q4[state, ]); rew[t-1] <- CalcReward(action, t)
state <- states[t] }; sum(rew); plot(ts(cumsum(rew)))


rew <- vector()
state <- states[1]; for (t in 2:length(states)) { action <- which.max(Q5[state, ]); rew[t-1] <- CalcReward(action, t)
state <- states[t] }; sum(rew); plot(ts(cumsum(rew)))
