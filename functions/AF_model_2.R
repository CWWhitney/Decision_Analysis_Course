library(decisionSupport)

make_variables <- function(est,n=1){
  x <- random(rho=est,n=n)
  for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("functions/input_hoa.csv",sep="")))

decision_model <- function(){
  n_years <- 25  # simunation time period 
  # create a empty vector of 25 values for 25 years
  mono_maize_cost <- rep(0, n_years)
  mono_yield <- rep(0, n_years)
  AF_setup_cost <- rep(0, n_years)
  AF_maintain_cost <- rep(0, n_years)
  fruit_yield <- rep(0, n_years)
  AF_maize_yield <- rep (0, n_years)
  grass_benefit <- rep(0, n_years)
  reduced_soil_loss <- rep(0, n_years)
  
  # Simulation of risk events happening during the simulation time
  
  fruit.risk <- chance_event(fruit_risk,
                             value_if = yield_fruit_risk, 
                             value_if_not = 0, 
                             n = n_years)
  maize.risk <- chance_event(maize_risk,
                             value_if = yield_maize_risk, 
                             value_if_not = 0, 
                             n = n_years)
  
  # Cost-benefit calculation of Agroforestry
  
  ## Establishment costs
  AF_setup_cost[1] <- establis_cost
  AF_setup_cost[2:25] <- 0
  
  ## Maintanance costs
  AF_maintain_cost[1] <- 0
  AF_maintain_cost[2:4] <- vv(main_cost_stage1,CV_cost,3) 
  AF_maintain_cost[5:10] <- vv(main_cost_stage2,CV_cost,6)
  AF_maintain_cost[11:25] <- vv(main_cost_stage3,CV_cost,15)
  
  # Calculating longan yield
  fruit_yield <- gompertz_yield(max_harvest = max_fruit_harvest,
                                time_to_first_yield_estimate = time_first_fruit_est,
                                time_to_second_yield_estimate = time_sec_fruit_est,
                                first_yield_estimate_percent = first_fruit_est_per,
                                second_yield_estimate_percent = sec_fruit_est_per,
                                n_years = n_years,
                                var_CV = CV_fruit_yield,
                                no_yield_before_first_estimate = TRUE)
  
  fruit_yield <- fruit_yield * (1 - fruit.risk) * num_of_fruit
  fruit_benefit <- fruit_yield * fruit_price
  
  ## Benefit from maize in agroforetry systems 
  time <- 1: n_years
  AF_maize_yield <- min_AFmaize + (max_AFmaize - min_AFmaize) * (1 - decay_rate_AF)^(time-1)
  AF_maize_yield <- AF_maize_yield * (1 - maize.risk)
  AF_maize_yield <- vv(AF_maize_yield, CV_maize_yield, n_years)
  AF_maize_benefit <- (AF_maize_yield * vv(maize_price,CV_maize_price,n_years))
  
  ## Benefits from forage grass
  grass_benefit <- vv(grass_profit,CV_grass_profit,n_years)
  
  ## Total revenue from Agroforestry
  
  AF_benefit <- fruit_benefit + AF_maize_benefit + grass_benefit
  
  ## Total costs from Agroforestry
  AF_cost <- AF_setup_cost + AF_maintain_cost
  
  ## Profit from Agroforestry
  AF_bottomline <- AF_benefit - AF_cost
  

  # Cost-benefit calculation from maize monoculture
  mono_maize <- max_monomaize * (1 - decay_rate_mono)^(time - 1)
  mono_maize <- mono_maize * (1-maize.risk)
  mono_maize <- vv(mono_maize, CV_maize_yield, n_years)
  mono_revenue <- (mono_maize * vv(maize_price,CV_maize_price,n_years))
  mono_costs <- vv(mono_cost,CV_cost,n_years)
  mono_bottomline <- mono_revenue - mono_costs
  
  # Calculating difference between two options
  decision_outcome <- AF_bottomline - mono_bottomline
  
  # NPV calculation for agroforestry, maize monoculture and decision of choosing agroforestry over maize monoculture
  NPV_AF <- discount(AF_bottomline, discount_rate, calculate_NPV = TRUE)
  cash_flow <- discount(AF_bottomline, discount_rate, calculate_NPV = FALSE)
  cum_cash_flow <- cumsum(cash_flow)
  NPV_mono <- discount(mono_bottomline, discount_rate, calculate_NPV = TRUE)
  NPV_decision <- discount(decision_outcome, discount_rate, calculate_NPV = TRUE)
  
  return(list(NPV_decision = NPV_decision,
              NPV_maize = NPV_mono,
              NPV_AF = NPV_AF,
              cashflow = cum_cash_flow))
}

simulation_results <- mcSimulation(estimate = estimate_read_csv("input.csv"),
                                   model_function = decision_model,
                                   numberOfModelRuns = 10000, # run model 10000 times
                                   functionSyntax = "plainNames")

### Visualization of outcome
## Cash flow
cash_flow <- plot_cashflow(mcSimulation_object = simulation_results,
                            color_25_75 = "grey60",
                            color_5_95 = "grey80",
                            color_median = "blue",
                            cashflow_var_name = "cashflow")

# Extract NPV data for display
NPV_data <- data.frame(NPV_decision = simulation_results$y$NPV_decision,
                   NPV_maize = simulation_results$y$NPV_maize,
                   NPV_AF = simulation_results$y$NPV_AF)
library(ggplot2)
library(tidyr)

# Transform data to long form
NPV_data_long <- pivot_longer(NPV_data,
                              cols = c(NPV_maize, NPV_AF),
                              names_to = "NPV",
                              values_to = "value")
# Compare two options: Agroforestry vs maize
ggplot(NPV_data_long, aes(x = value, fill = NPV)) +
  geom_histogram(
    bins = 200,
    position = "identity",
    alpha = 0.5,
    color = "white") +
  labs(x = "Profit (million VND/ha)",
       y = "Frequency") +
  coord_cartesian(xlim = c(-100, 1500)) +
  theme_minimal() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text  = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.80))

# Display NPV of decision (NPV difference between Agroforestry and maize)
ggplot(NPV_data, aes(x = NPV_decision)) +
  geom_histogram(bins = 200,
                 fill = "rosybrown1",
                 color = "white") +
  labs(title = NULL,
       x = "Profit (million VND/ha)",
       y = "Frequency") +
  coord_cartesian(xlim = c(-100, 1000)) +
  theme_minimal()+
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 14))

### Calculate and display the Value of Information (EVPI)
mc_table <- data.frame(simulation_results$x,
                       simulation_results$y[1])
evpi <- multi_EVPI(mc = mc_table , first_out_var = "NPV_decision")
plot_evpi(evpi,"NPV_decision")
