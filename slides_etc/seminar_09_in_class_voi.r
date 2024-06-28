# This is a simple decision analysis model to show the intervention of converting a sheep grazing grassland into apple agroforestry.
# pacakges
library(decisionSupport)
library(ggplot2)
#library(dplyr)

# define the inputs of teh model. You can also provide it using a csv file.
input_estimates <- data.frame(variable = c("sheep_income", "sheep_cost","apple_income", "apple_cost", "discount_rate"),
                              lower = c(3000, 1000, 30000, 15000, 10),
                              median = NA,
                              upper = c(5000, 2500, 60000, 30000, 10),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "const"),
                              label = c("Income from sheep (euro/year)", "Cost of sheep (euro/year)", "Income from apple (euro/year)", "Cost of apple (euro/year)", "Discount Rate")
                              )
# make variables function from seminar 6
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}
make_variables(as.estimate(input_estimates)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part

# write a function for the decision
model_function <- function(){
  
  # Estimate the income and cost of AF system
  AF_income <- sheep_income + apple_income
  AF_cost <- sheep_cost + apple_cost
  
  # Estimate the final results from the model
  AF_final_result <- AF_income - AF_cost
  sheep_only <- sheep_income - sheep_cost
  
  #Estimate tradeoff (decision) from having AF system
  Tradeoff_benefit <- AF_final_result - sheep_only
  
  #Calculating NPV
  AF_NPV <- discount(AF_final_result, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  NPV_sheep_only <- discount(sheep_only, discount_rate = discount_rate,
                                  calculate_NPV = TRUE) #NVP of grassland
  NPV_tradeoff <- discount(Tradeoff_benefit, discount_rate = discount_rate,
                           calculate_NPV = TRUE)# NPV of Tradeoff
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(NPV_Agroforestry_System = AF_NPV,
              NPV_Treeless_System = NPV_sheep_only,
              NPVtrade_off = NPV_tradeoff))
}

# Run the Monte Carlo simulation using the model function with DecisionSupport package and plot teh results
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 5000,
                                      functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = example_mc_simulation, 
                   vars = c("NPVtrade_off", "NPV_Treeless_System"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7,
                   x_axis_name = "Outcome of AF intervention",
                   scale_x_continuous(labels = function(x) x / 100000),
                   ggtitle("Net Present Value of the system"),
                   legend.position="bottom")
# save the plot on your machine
ggsave(
  filename = "NPV_tardeoff_sheep_vs_AppleAF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

#Perfrom Projection to Latent Structures (PLS) analysis and plot teh results
pls_result_AF <- plsr.mcSimulation(object = example_mc_simulation,
                                   resultName = names(example_mc_simulation$y)[3], ncomp = 1)

plot_pls(pls_result_AF, input_table = input_estimates, cut_off_line = 1, threshold = 0.5)
# save the plot on your machine
ggsave(
  filename = "images/PLS_sheep_vs_AppleAF.png",
  plot = last_plot(),
  width = 5, 
  height = 3
)

#Read results of MC simulation 
MCall <- read.table(file="MCResults/mcSimulationResults.csv",
                    header = TRUE,sep=",")
#extract only input variables and decision columns
mc <- MCall[,c(2:6,9)]
# compute EVPI
EVPI <- multi_EVPI(mc,"NPVtrade_off",write_table=TRUE)
# plot EVPI
plot_evpi(EVPI, "NPVtrade_off")

# #save input data to a folder named MCResults as a csv file
# df <- data.frame(input_estimates)
# write.csv(df, file = "MCResults/input_table.csv", row.names = FALSE)

# # use welfare function covered in seminar - refer DescisionSupport Manual for more information
# decisionSupport(inputFilePath = paste("MCResults/input_table.csv",sep=""),
#                 outputPath = paste0("MCResults", "/", sep=""),
#                 write_table = TRUE,
#                 welfareFunction = model_function,
#                 numberOfModelRuns = 80, #run 10,000 times
#                 functionSyntax = "plainNames")
