# set.seed(42)
# Monte Carlo Simulation
num_simulations <- 1000

# Define ranges per ha per year Euro
# lower and upper
apple_income_range <- c(3000, 60000)
apple_costs_range <- c(15000, 30000)

apple_income <- runif(n = num_simulations, 
                      min = apple_income_range[1], 
                      max = apple_income_range[2])

apple_costs <- runif(n = num_simulations, 
                      min = apple_costs_range[1], 
                     max = apple_costs_range[2])

apple_profits <- apple_income - apple_costs

hist(apple_profits)

abline(v = quantile(apple_profits, 
                    c(0.1, 0.5, 0.9), 
                    lwd = 10)) # lwd = line width

# Add sheep to the horticulture system 
# Now it is silvopastoral 

# Euro per ha per year
sheep_income_range <- c(2000, 5000)
sheep_costs_range <- c(1000, 2500)

sheep_income <- runif(n = num_simulations, 
                      min = sheep_income_range[1], 
                      max = sheep_income_range[2])

sheep_costs <- runif(n = num_simulations, 
                     min = sheep_costs_range[1], 
                     max = sheep_costs_range[2])

sheep_profits <- sheep_income - sheep_costs

total_profits <- apple_profits + sheep_profits

hist(total_profits, col = "white")
hist(sheep_profits, add = TRUE, col = "black")
hist(apple_profits, add = TRUE, col = "pink")


### Bayesian updating #######

# Find a way to work in a new set of inut tables 
# Ideally in an interactive interface like a google doc

# Observed data
observed_apple_incomes <- c(40000, 45000, 50000, 42000, 47000)
observed_apple_costs <- c(20000, 22000, 21000)

# Number of observations
n <- length(observed_apple_incomes)

# Prior distributions for mean income and costs (Normal priors)
# Mean and standard deviation of priors
prior_mean_income <- mean(apple_income_range)
# Calculate the approximate prior standard deviation for a uniform distribution 
# The range of the uniform distribution is the difference between the maximum
# and minimum values: (b - a)
# U(a, b) is (b - a) / sqrt(12) (this is approximately 3.46)
# PKR Note cite lit on sqrt12 ###
prior_sd_income <- (apple_income_range[2] - apple_income_range[1]) / sqrt(12)
# same for costs
prior_mean_costs <- mean(apple_costs_range)
prior_sd_costs <- (apple_costs_range[2] - apple_costs_range[1]) / sqrt(12)

# Bayesian updating for mean income
posterior_mean_income <- (prior_mean_income/prior_sd_income^2 + 
                            sum(observed_apple_incomes)/prior_sd_income^2) / 
                          (1/prior_sd_income^2 + n/prior_sd_income^2)
posterior_sd_income <- sqrt(1 / (1/prior_sd_income^2 + n/prior_sd_income^2))

# Bayesian updating for mean costs
posterior_mean_costs <- (prior_mean_costs/prior_sd_costs^2 + 
                           sum(observed_apple_costs)/prior_sd_costs^2) / 
                        (1/prior_sd_costs^2 + n/prior_sd_costs^2)
posterior_sd_costs <- sqrt(1 / (1/prior_sd_costs^2 + n/prior_sd_costs^2))

# Monte Carlo simulation using updated parameters

apple_updated_income <- rnorm(n = num_simulations, 
                              mean = posterior_mean_income, 
                              sd = posterior_sd_income)
apple_costs <- rnorm(n = num_simulations, 
                     mean = posterior_mean_costs, 
                     sd = posterior_sd_costs)

apple_updated_profits <- apple_income - apple_costs

# Plot updated profit distribution
hist(apple_updated_profits, main = "Updated Apple Profits Distribution", xlab = "Profit", col = "pink")
abline(v = quantile(apple_updated_profits, c(0.1, 0.5, 0.9)), col = "blue", lwd = 2)

### Bayesian updating as function #######

