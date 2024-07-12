# Micro-dosing question

# baseline per ha yield
Shea_yield <- c(100, 500)
  maize_yield <- c(800, 1000)
  unfert_yield <- Shea_yield + maize_yield
  
  normal_fertilizer_increase <- c(0.1, 0.5)
  baseline_yield <- unfert_yield + unfert_yield*normal_fertilizer_increase
    # Intervention to add micro-dosing
  micro_dose_increase <- c(0.2, 0.8)
  mico_dose_yield <- unfert_yield + unfert_yield*micro_dose_increase
  
  num_simulations <- 10000
  
  baseline <- runif(n = num_simulations, 
                        min = baseline_yield[1], 
                        max = baseline_yield[2])
  
  microdose <- runif(n = num_simulations, 
                       min = mico_dose_yield[1], 
                       max = mico_dose_yield[2])

  
  hist(baseline, col = "white")
  hist(microdose, add = TRUE, col = "pink")
  