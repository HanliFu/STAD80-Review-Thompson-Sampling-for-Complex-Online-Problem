# Upper Confidence Bound
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


# Implementing UCB
N = 10000
d = 10
bandit_selected = integer(0)
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0
for (n in 1:N) {
  bandit_max_random = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
        upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      bandit_max_random = i
    }
  }
  bandit_selected = append(bandit_selected, bandit_max_random)
  numbers_of_selections[bandit_max_random] = 
    numbers_of_selections[bandit_max_random] + 1
  reward = combinedDf[n, bandit_max_random]
  sums_of_rewards[bandit_max_random] = 
    sums_of_rewards[bandit_max_random] + reward
  total_reward = total_reward + reward
}

#visualization
histogramOfbandit_selected <- function(bandit_selected){
  
  png("ucb_bandit_selected.png")
  
  hist(bandit_selected,
       col = 'darkgreen',
       main = 'Histogram(UCB algorithm) of bandit selections',
       xlab = 'bandit',
       ylab = 'Count of selected')
  
  
  dev.off()
  
}

histogramOfbandit_selected(bandit_selected)

