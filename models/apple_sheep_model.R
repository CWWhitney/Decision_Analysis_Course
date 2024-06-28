#### Apple Sheep Model ####

# Inputs

input_estimates <- data.frame(
  variable = c(
    "sheep_income",
    "sheep_cost",
    "apple_income",
    "apple_cost",
    "discount_rate"
  ),
  lower = c(3000, 1000, 30000, 15000, 10),
  median = NA,
  upper = c(5000, 2500, 60000, 30000, 10),
  distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "const"),
  label = c(
    "Income from sheep (euro/year)",
    "Cost of sheep (euro/year)",
    "Income from apple (euro/year)",
    "Cost of apple (euro/year)",
    "Discount Rate"
  )
)


###### Apple Sheep #########
# Model function 

model_function <- function() {
  # Estimate the income in a normal season
  AF_income <- sheep_income + apple_income
  AF_cost <- sheep_cost + apple_cost
  # Estimate the final results from the model
  AF_final_result <- AF_income - AF_cost
  sheep_only <- sheep_income - sheep_cost
  
  Decision_benefit <- AF_final_result - sheep_only
  #Calculating NPV
  #AF System
  AF_NPV <- discount(AF_final_result,
                     discount_rate = discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  NPV_sheep_only <- discount(sheep_only,
                             discount_rate = discount_rate,
                             calculate_NPV = TRUE) #NVP of grassland
  NPV_decision <- discount(Decision_benefit,
                           discount_rate = discount_rate,
                           calculate_NPV = TRUE)
  # Generate the list of outputs from the Monte Carlo simulation
  return(
    list(
      NPV_Agroforestry_System = AF_NPV,
      NPV_Treeless_System = NPV_sheep_only,
      NPV_decision = NPV_decision
    )
  )
}

# simulation result
apple_sheep_mc_simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 80,
  functionSyntax = "plainNames"
)

