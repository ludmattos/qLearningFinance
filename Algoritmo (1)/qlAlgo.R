#Learning an agent finding a destination in a 2×2 grid with a wall
#I Initialize 4 states and 4 actions

actions <- c("up", "left", "down", "right")
states <- c("s0", "s1", "s2", "s3")

#Adding a function that mimics the environm

simulateEnvironment <- function(state, action) {
  # Calculate next state (according to sample grid with wall)
  # Default: remain in a state if action tries to leave grid
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


#Add a Q-learning function that performs a given number n of episodes

Qlearning <- function(n, s_0, s_terminal,
                      epsilon, learning_rate) {
  # Initialize state-action function Q to zero
  Q <- matrix(0, nrow=length(states), ncol=length(actions),
              dimnames=list(states, actions))
  # Perform n episodes/iterations of Q-learning
  for (i in 1:n) {
    Q <- learnEpisode(s_0, s_terminal,
                      epsilon, learning_rate, Q)
  }
  return(Q)
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
Q <- Qlearning(1000, "s0", "s3", epsilon, learning_rate)
Q
## up left down right
## s0 -79.962619 -81.15445 -68.39532 -79.34825
## s1 -73.891963 -52.43183 -52.67565 -47.91828
## s2 -8.784844 -46.32207 -17.97360 -20.29088
## s3 0.000000 0.00000 0.00000 0.00000

#Optimal policy
# note: problematic for states with ties
actions[max.col(Q)]
## [1] "down" "right" "up" "up"

