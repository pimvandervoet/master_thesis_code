#' Propensity_score_estimation
#'
#' @description  This function provides estimates of the propensity scores that can be used in the BCF model
#'
#' Output: @return propensity_estimates .... n x samples draws of propensity score estimates
#' Input:  @param predictors ... variables used to estimate the propensity scores
#'         @param treatment ... variable that indicates treatment or not for each individual
#'         @param samples  .... number of posterior samples that user wants 
#'         @param technique ....  technique speciified by the user that is used for estimation. Default = BART
#'         

ps_estimator <- function(predictors, treatment, samples, technique = "BART"){
  #Preprocessing steps if needed
  
  #Build in a loop for cross validation
  
  train_predictors <-"NA"
  test_predictors < "NA"
  train_treatment <- "NA"
  test_treatment <- "NA"  
  
  
  #Actual estimation - default method is BART
  if(technique == "BART"){
    library(BART)
    
    BART_execution <- gbart(
      x.train = train_predictors,
      y.train = train_treatment,
      x.test = test_predictors,
      type ='pbart', #Probit - can use logit by lbart
      rho = NULL,
      sigest = NA, #Sigma prior specification - not used for binary outcome
      sigdf = 3, 
      sigquant = 0.90,
      k = 2, #Conservativeness of estimate
      power = 2, #Tree prior specification
      base = 0.95,
      ntree = 50L, #Number of trees
      ndpost = 1000L, #Number of posterior draws
      nskip = 100L, #Burn in 
      keepevery = c(1L, 10L, 10L)[2], #Thinning
      printevery = 100L, #print draws
      transposed = FALSE,
    )
    #Need to apply pnorom still on yhat test. Note that each row is a draw. 
    
  }
  output(propensity_estimates)
}

#' ps_analysis
#'
#' This function analyses the estimated propensity scores. This is necessary to look whether enough support is provided in estimatoin
#' One key point to look out for is that there are not too much values close to 0 or 1. This makes the estimates uninformative
#' Output:  Some graphs and statistics perhaps
#' Input:   @estimated_ps ... propoensity score estimates
#'          @control_variables ... control variables for which support needs to be checked 
ps_analysis <- function(estimated_ps, control_variables, ...){
  
}