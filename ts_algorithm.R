

set.seed(1004)

# simulate 10 bandit with different probabilities
prob.interact = runif(1,min=0,max=0.35)
bandit_one <- rbinom(10000, size = 1, prob = 0.2)
bandit_two <- rbinom(10000, size = 1, prob = 0.5-prob.interact)
bandit_three <- rbinom(10000, size = 1, prob = 0.3)
bandit_four <- rbinom(10000, size = 1, prob = 0.33)
bandit_five <- rbinom(10000, size = 1, prob = 0.3)
bandit_six <- rbinom(10000, size = 1, prob = prob.interact)
bandit_seven <- rbinom(10000, size = 1, prob = 0.4)
bandit_eight <- rbinom(10000, size = 1, prob = 0.399)
bandit_nine <- rbinom(10000, size = 1, prob = 0.38)
bandit_ten <- rbinom(10000, size = 1, prob = 0.37)
combinedDf <- cbind(bandit_one, bandit_two,bandit_three, bandit_four,
                    bandit_five, bandit_six,bandit_seven, bandit_eight,
                    bandit_nine, bandit_ten)

0.2-0.4


# Implementing Thompson Sampling
N = 10000
d = 10
bandit_selected_ts = integer()
count_reward_1 = integer(d)
count_reward_0 = integer(d)
total_reward = 0
regret_global = vector()
total_regret = vector()
for (n in 1:N) {
  bandit_max_random = 0
  max_random = 0
  for (i in 1:d) {
    random_selected = rbeta(n = 1,
                            shape1 = count_reward_1[i] + 1,
                            shape2 = count_reward_0[i] + 1)
    if (random_selected > max_random) {
      max_random = random_selected
      bandit_max_random = i
    }
  }
  bandit_selected_ts = append(bandit_selected_ts, bandit_max_random)
  reward = combinedDf[n, bandit_max_random]
  if (reward == 1) {
    count_reward_1[bandit_max_random] = count_reward_1[bandit_max_random] + 1
  } else {
    count_reward_0[bandit_max_random] = count_reward_0[bandit_max_random] + 1
  }
  total_reward = total_reward + reward
}


#visualization
histogramOfbandit_selected_ts <- function(bandit_selected_ts){
  
  png("ts_bandit_selected.png")
  
  hist(bandit_selected_ts,
       col = 'darkgreen',
       main = 'Histogram(TS Algorithm) of bandit selections',
       xlab = 'bandit',
       ylab = 'Count of selected')
  
  
  dev.off()
  
}

histogramOfbandit_selected_ts(bandit_selected_ts)


