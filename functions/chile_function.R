###### Chile function #######

input_estimates <- read.csv("data/input_estimates.csv")


# create the model function
model_function <- function(){
  income <- Yield * Market_price
  overall_costs <- Labor_cost + Management_cost
  final_result <- income - overall_costs
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function ####
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 100,
                                      functionSyntax = "plainNames")