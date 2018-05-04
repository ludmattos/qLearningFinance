
# ===================================================================== #
# ===== I - Learning with 4 states and 4 actions, fixed reward ======== #
# ===================================================================== #

# --------------------------------------------------------------------- #
# DOUBTS: i) Performance metrics
#        ii) Number of interactions
#       iii) Decision Policy  
# --------------------------------------------------------------------- #
actions <- c("up", "left", "down", "right")
states <- c("s0", "s1", "s2", "s3")

#       s0      s3
#       ||      ||
#       s1 <==> s2

simulateEnvironment <- function(state, action) {
  next_state <- state
  if (state == "s0" && action == "down") next_state <- "s1"
  if (state == "s1" && action == "up") next_state <- "s0"
  if (state == "s1" && action == "right") next_state <- "s2"
  if (state == "s2" && action == "left") next_state <- "s1"
  if (state == "s2" && action == "up") next_state <- "s3"
  if (state == "s3" && action == "down") next_state <- "s2"
  # Calculate reward
  if (next_state == "s3") {
    reward <- 10
  } else {
    reward <- -1
  }
  return(list(state=next_state, reward=reward))
}

Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), ncol=length(actions),
              dimnames=list(states, actions))
  s11 <- vector()
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate, Q)
    s11[i] <- max(Q[1,])
  }
  return(list(Q, s11))
}

learnEpisode <- function(s_0, s_terminal, epsilon, learning_rate, Q) {
  state <- s_0 # set cursor to initial state
  while (state != s_terminal) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      action <- sample(actions, 1) # pick random action
    } else {
      action <- which.max(Q[state, ]) # pick first best action
    }
    # get next state and reward from environment
    response <- simulateEnvironment(state, action)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response$reward + max(Q[response$state, ]) - Q[state, action])
    state <- response$state # move to next state
  }
  return(Q)
}

#Choose learning parameters
epsilon <- 0.1
learning_rate <- 0.1

#Calculate state-action function Q after 1000 episodes
set.seed(0)
Q <- Qlearning(800, "s0", "s3", epsilon, learning_rate)
Q[[1]]

plot(Q[[2]], type="l")

#Optimal policy
# note: problematic for states with ties
actions[max.col(Q)]
## [1] "down" "right" "up" "up"

# Pelo que vejo o valor em si não diz muito, alguns procedimentos padronizam
# a matriz Q. O importante parece ser a política se manter invariável.

# =============================================================+++=====+=== #
# === II - Learning with 4 states and 4 actions, time dependent reward ==== #
# ========================================================================= #

simulateEnvironment <- function(state, action) {
  next_state <- state
  if (state == "s0" && action == "down") next_state <- "s1"
  if (state == "s1" && action == "up") next_state <- "s0"
  if (state == "s1" && action == "right") next_state <- "s2"
  if (state == "s2" && action == "left") next_state <- "s1"
  if (state == "s2" && action == "up") next_state <- "s3"
  if (state == "s3" && action == "down") next_state <- "s2"
  # Calculate reward
  if (next_state == "s3") {
    reward <- 10*rexp(1)
  } else {
    reward <- -1*rchisq(1,1)
  }
  return(list(state=next_state, reward=reward))
}

Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), ncol=length(actions),
              dimnames=list(states, actions))
  s11 <- vector()
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate, Q)
    s11[i] <- max(Q[1,])
  }
  return(list(Q, s11))
}

learnEpisode <- function(s_0, s_terminal, epsilon, learning_rate, Q) {
  state <- s_0 # set cursor to initial state
  while (state != s_terminal) {
    # epsilon-greedy action selection
    if (runif(1) <= epsilon) {
      action <- sample(actions, 1) # pick random action
    } else {
      action <- which.max(Q[state, ]) # pick first best action
    }
    # get next state and reward from environment
    response <- simulateEnvironment(state, action)
    # update rule for Q-learning
    Q[state, action] <- Q[state, action] + learning_rate *
      (response$reward + max(Q[response$state, ]) - Q[state, action])
    state <- response$state # move to next state
  }
  return(Q)
}

#Choose learning parameters
epsilon <- 0.1
learning_rate <- 0.1

#Calculate state-action function Q after 1000 episodes
set.seed(0)
Q <- Qlearning(800, "s0", "s3", epsilon, learning_rate)
Q[[1]]

plot(Q[[2]], type="l")

#Optimal policy
# note: problematic for states with ties
actions[max.col(Q[[1]])]
## [1] "down" "right" "up" "up"
