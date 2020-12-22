#Estimation of BCF 

#Let user define method for estimation of pi hat
#Let user give other inputs if neccessary

#####BCF_estimation####
# Function estimates BCF model for given inputs
# Output:
# Posterior sample for treatment effect from BCF that can be used to approximate the posterior treatment effect function.   
# This posterior sample includes values for other variables such that this posterior can also be used to construct CATE's
#
# Inputs:
# @outcome - variable of interest
# @cvars - control variables
# @mvars - effect moderating variables
# @ps_estimates - propensity score estimates provided by user, can be multiple draws
# @errordistribution - gives user the option to change the error distribution, by default this is normal
# @sensitivity_parameters - Lets the user include parameters to assess sensititvity to unobserved confounding from the posterior
BCF_estimation <- function(outcome, cvars, mvars,  ps_estimates, errordistribution = "normal", sensitivity_parameters){
  library(bcf)
  
  
  
  
  }