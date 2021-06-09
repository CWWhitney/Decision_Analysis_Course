library(decisionSupport)


#defined in input table:
yield<-50
var_CV<-20
initial_investment<-1000
price<-3

decision_model <- function(){

yields <- vv(var_mean = yield, 
             var_CV = var_CV, 20)

prices <- vv(var_mean = price, 
             var_CV = var_CV, 20)

p_hail <- 0.2

# calculate costs for 20 years with 'rep'
costs <- c(initial_investment, rep(0, 19))

# use 'chance_event' to adjust yield for potential hail
hail_adjusted_yield <- chance_event(p_hail, 
                                    value_if = 0,
                                    value_if_not = yield,
                                    n = 20,
                                    CV_if = var_CV)

# calculate profit
profit <- (hail_adjusted_yield*prices)-costs

# use 'discount' to calculate net present value
NPV <- discount(profit, 5, calculate_NPV = TRUE)

return(NPV = NPV) }
