# reduce var_CV to 5 and change n to 40. Name the output 'valvar' and plot with base R  

number_of_years <- 10
tractor_value <- vv(var_mean = 10000, 
                var_CV = 2, 
               n = number_of_years, 
             absolute_trend = -5) #depreciation
plot(tractor_value)

#Giang's eco-village example
hepa_eco_house <- vv(var_mean = 1000, 
                    var_CV = 20, #whole number percentage
                    n = number_of_years, 
                    absolute_trend = -20) #depreciation
plot(hepa_eco_house)

#Farimah's house loan example
house_loan <- vv(var_mean = 10000, 
                     var_CV = 10, 
                     n = number_of_years, 
                     relative_trend = 5) #interest
plot(house_loan)

# Tractor break down
# is different in the first five years
tractor_break_down_event_years1to5 <- c(0.3)
tractor_mechanic_cost_years1to5 <- 100
tractor_repair_costs_years1to5 <- chance_event(chance = tractor_break_down_event_years1to5, 
                                     value_if = vv(tractor_mechanic_cost_years1to5, 
                                                   var_CV = 10, 
                                                   relative_trend = 5, 
                                                   n=number_of_years), 
                                     n=1, 
                                     value_if_not = c(0,0,0,0,0,0,0,0,0,0),
                                     CV_if = 10)
tractor_repair_costs_years1to5[6:10] <- 0
# is different in the second five years
tractor_break_down_event_years6to10 <- c(0.6)
tractor_mechanic_cost_years6to10 <- 300
tractor_repair_costs_years6to10 <- chance_event(chance = tractor_break_down_event_years6to10, 
                                               value_if = vv(tractor_mechanic_cost_years6to10, 
                                                             var_CV = 10, 
                                                             relative_trend = 5, 
                                                             n=number_of_years), 
                                               n=1, 
                                               value_if_not = c(0,0,0,0,0,0,0,0,0,0),
                                               CV_if = 10)
tractor_repair_costs_years6to10[1:5] <- 0

tractor_repair_costs <- tractor_repair_costs_years1to5 + tractor_repair_costs_years6to10


result <- (tractor_value + hepa_eco_house + 
  house_loan) -  tractor_repair_costs

npv <- discount(x = result, discount_rate = 10, calculate_NPV = TRUE)


