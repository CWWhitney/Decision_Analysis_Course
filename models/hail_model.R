###### HAIL Risk #######

hail_estimates <- read.csv("data/hail_estimates.csv")

hail_function <- function(){
  
  yields <- vv(var_mean = yield,
               var_CV = var_CV,
               n = 20)
  
  prices <- vv(var_mean = price,
               var_CV = var_CV,
               n = 20)
  
  # use 'rep' to simulate the investment
  # only in the first year (assuming the net lasts 20 years)
  invest_costs <- c(initial_investment, rep(0, 19))
  
  # use 'chance_event' to adjust yield for potential hail
  hail_adjusted_yield <- chance_event(chance = p_hail,
                                      value_if = 0,
                                      value_if_not = yield,
                                      n = 20)
  
  # calculate profit without net
  profit_no_net <- hail_adjusted_yield*prices
  
  # calculate profit with the net
  profit_with_net <- (yields*prices)-invest_costs
  
  # use 'discount' to calculate net present value
  # 'discount_rate' is expressed in percent
  NPV_no_net <- discount(profit_no_net, discount_rate = 5, calculate_NPV = TRUE)
  NPV_net <- discount(profit_with_net, discount_rate = 5, calculate_NPV = TRUE)
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_net-NPV_no_net
  
  return(list(NPV_no_net =  NPV_no_net,
              NPV_net =  NPV_net,
              NPV_decision = NPV_decision))
}

###### Run HAIL Risk model #######
hail_mc_simulation <- mcSimulation(estimate = as.estimate(hail_estimates),
                                   model_function = hail_function,
                                   numberOfModelRuns = 100,
                                   functionSyntax = "plainNames")

######  HAIL Risk evpi #######

mcSimulation_table_hail <- data.frame(hail_mc_simulation$x,
                                      hail_mc_simulation$y[3])

evpi_hail <- multi_EVPI(mc = mcSimulation_table_hail,
                        first_out_var = "NPV_decision")
