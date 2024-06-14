# set.seed(42)
# Monte Carlo Simulation
num_simulations <- 1000000


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
