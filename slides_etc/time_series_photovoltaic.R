library(decisionSupport)


#defined in input table:
yield<-50
var_CV<-20
initial_investment<-1000
price<-3

# value-varier

decision_model<-function(x, var)
{

yields<-vv(var_mean=yield,var_CV=var_CV,20)
prices<-vv(var_mean=price,var_CV=var_CV,20)
p_hail<-0.2

costs<-c(initial_investment,rep(0,19))




plot(gompertz_yield(max_harvest=1000,
               time_to_first_yield_estimate=5,
               time_to_second_yield_estimate=15,
               first_yield_estimate_percent=10,
               second_yield_estimate_percent=90,
               n_years=30,
               var_CV=0,
               no_yield_before_first_estimate=FALSE))


yield_hail_adjusted<-chance_event(p_hail,value_if=0,value_if_not=yield,n=20,CV_if=var_CV)

profit<-yield_hail_adjusted*prices-costs
#sum_cashflow<-sum(profit)


return(discount(profit,5,calculate_NPV=TRUE))
}