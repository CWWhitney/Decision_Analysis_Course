#' ---
#' title: From impact pathway to executable code
#' author: Cory Whitney, Eduardo Fernández
#' date: "`r format(Sys.Date(), format = '%B %d, %Y')`"
#' output: html_document
#' ---

#' ### A very simple example
#' In this example, we show how we can transform one of the simplest impact pathways into code for estimating 
#' Net Present Value distributions.

#' Load the required libraries
library(DiagrammeR)
library(decisionSupport)

#' Plot the graphical impact pathway
#+ fig.cap="Figure 1. Graphical impact pathway of a simple model", fig.align="center"
mermaid("
        graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        C(Production cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")

#' Generate an input table to feed the model function
input <- data.frame(variable = c("Yield", "Market_price", "Production_cost"),
                    lower = c(6000, 3, 5000),
                    median = NA,
                    upper = c(14000, 8, 10000),
                    distribution = c("posnorm", "posnorm", "posnorm"),
                    label = c("Yield (kg/ha)", "Price (USD/kg)", "Production cost (USD/kg)"),
                    Description = c("Yield in a sweet cherry farm under normal conditions",
                                    "Price of sweet cherry in a normal season",
                                    "Production cost in a normal season"))

input <- as.estimate(input)

#' Implement a model function that describe the graphical impact pathway
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the final results from the model
  final_result <- income - Production_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

#' Run the Monte Carlo simulation using the model function
mc_simulation <- mcSimulation(estimate = input,
                              model_function = model_function,
                              numberOfModelRuns = 10e4,
                              functionSyntax = "plainNames")

#' Draw some preliminary results
#+ fig.cap="Figure 2. Result of Monte Carlo simulation (10,000 model runs) for estimating the profits in sweet cherry orchards", fig.align="center"
plot_distributions(mcSimulation_object = mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")






