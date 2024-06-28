
input_estimates <- read.csv("data/input_estimates.csv")
  
model_function <- function(){
  income <- Yield * Market_price
  overall_costs <- Labor_cost + Management_cost
  final_result <- income - overall_costs
  return(list(final_result = final_result))
}

example_mc_simulation <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 1000,
  functionSyntax = "plainNames"
)


decisionSupport::plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


#### compound figure

# Create the estimate object:

cost_benefit_table <- data.frame(label = c("Revenue", "Costs"),
                                 variable = c("annual_revenue", "annual_costs"),
                                 distribution = c("norm", "norm"),
                                 lower = c(800,  500),
                                 median = c(NA, NA),
                                 upper = c(8000, 5000))

# (a) Define the model function without name for the return value:

profit1 <- function() {
  revenue_nyears <- vv(var_mean = annual_revenue, 
                       n = 20, var_CV = 3, relative_trend = 5)
  costs_nyears <- vv(var_mean = annual_costs, 
                     n = 20, var_CV = 3)
  revenue <- sum(revenue_nyears)
  costs <- sum(costs_nyears)
  Decision <- revenue - costs
  cashflow <- revenue_nyears - costs_nyears
  return(list(Revenues = revenue,
              Costs = costs, 
              cashflow = cashflow, 
              Decision = Decision))
}

compound_figure(model = profit1, 
                input_table = cost_benefit_table, 
                decision_var_name = "Decision",
                cashflow_var_name = "cashflow",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')

### bayesplot

test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

library(bayesplot)

color_scheme_set("brewer-BrBG")
mcmc_intervals(test, prob = 0.5, 
               prob_outer = 0.9, 
               point_est = "median")

## cashflow 

# Plotting the cashflow:

# Create the estimate object (for multiple options):

variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 20, 8000,  500)
upper = c(100000, 50000, 20, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

# Define the model function without name for the return value:

profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1, 
                         n = n_years, var_CV = 100, relative_trend = 3)
  cashflow_option2 <- vv(revenue_option2 - costs_option2, 
                         n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

# Perform the Monte Carlo simulation:

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


# Plot the cashflow distribution over time

plot_cashflow(mcSimulation_object = predictionProfit1, 
              cashflow_var_name = "Cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", 
              color_5_95 = "green1",
              color_median = "red")
