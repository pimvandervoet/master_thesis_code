#' Propensity_score_estimation
#'
#' @description  This function provides estimates of the propensity scores that can be used in the BCF model
#'
#' Output: @return propensity_estimates .... n x samples*repeats or n x repeats draws of propensity score estimates
#' Input:  @param predictors ... variables used to estimate the propensity scores
#'         @param treatment ... variable that indicates treatment or not for each individual
#'         @param samples  .... number of posterior samples that user wants per individual per cross fold, default 1000
#'         @param technique ....  technique speciified by the user that is used for estimation. Default = "BART"
#'         @param take_means_draws ... Logical variable indicating whether mean of all posterior draws for each k_fold needs to be taken, default TRUE
#'         @param k_fold_cv .... number of cross validations, default 10
#'         @param repeats ... number of repeats of cross validation, default 1

ps_estimator <-
  function(predictors,
           treatment,
           samples = 1000,
           technique = "BART",
           take_means_draws = TRUE,
           k_fold_cv = 10,
           repeats = 1) {
    #Preprocessing steps if needed
    set.seed(30121997)
    
    #Empty propensity scores matrix for all repeats
    if(take_means_draws){
      propensity_scores_total <-
        matrix(NA, nrow(predictors), ncol = repeats)
    }else{
    propensity_scores_total <-
      matrix(NA, nrow(predictors), ncol = repeats * samples)
    }
    
    #Make input from list to matrix if necessary (treatment is vector already so need no changes)
    if (typeof(predictors) == "list") {
      predictors <- matrix(unlist(predictors), nrow = dim(predictors)[1], byrow = TRUE)
    }
    
    #Set number of results per cross validation step
    if (take_means_draws) {
      results_per_cv = 1
    } else
      (
        results_per_cv = samples
      )
    
    #Do repeats for repeated cross validation
    for (rp in 1:repeats){
      
      #Make emtpty matrix for results of each repeat
      propensity_scores_repeat <- matrix(NA, nrow(predictors), ncol = results_per_cv)
      
      #Make sample for each repeat
      individuals_shuffle <- sample(nrow(predictors))
      shuffled_predictors <- predictors[individuals_shuffle, ]
      shuffled_treatment <- treatment[individuals_shuffle, ]
      
      folds <- cut(seq(1, nrow(shuffled_predictors)), breaks = 10, labels = FALSE)
      
      #Loop over k cross folds
      for (k_fold in 1:k_fold_cv){
        
        #Select train and test
        test_indicator <- which(folds == k_fold, arr.ind=TRUE)
        train_predictors <- shuffled_predictors[-test_indicator, ]
        test_predictors <- shuffled_predictors[test_indicator, ]
        train_treatment <- shuffled_treatment[-test_indicator, ]
        train_treatment <- pull(train_treatment, expRDAll)
        
        #Make sure train and test predictors are dataframe (unlist function)
        
        #Actual estimation - default method is BART
        if(technique == "BART"){
          library(BART)
          
          model_execution <- gbart(
            x.train = train_predictors,
            y.train = train_treatment,
            x.test = test_predictors,
            #Probit - can use logit by lbart
            type ='pbart',
            rho = NULL,
            #Sigma prior specification - not used for binary outcome
            sigest = NA,
            sigdf = 3,
            sigquant = 0.90,
            #Conservativeness of estimate
            k = 2,
            #Tree prior specification
            power = 2,
            base = 0.95,
            #Number of trees
            ntree = 50L,
            #Number of posterior draws
            ndpost = samples,
            #Burn in
            nskip = 100L,
            #Thinning
            keepevery = c(1L, 10L, 10L)[2],
            #print draws
            printevery = 100L,
            transposed = FALSE,
          )
          #Use bart execution pnorm output. Make user choose to take averages or not -- need to transpose if not taking averages!
        } else if(technique == "LOGIT")
        {
          #Some other technique for estimation -- to be added if desired
          #Need to do some logit form here. 
        }
        
        #Fill output matrix with output for crossfold k
        if(take_means_draws){
        predictions <- model_execution$prob.test.mean
        } else{
          #Note that the output needs to be transposed as the gbart function provides each posterior draw in each row
          predictions <- t(model_execution$prob.test)
        }
        propensity_scores_repeat[test_indicator, ] <- predictions 
      }
      
      #Fill total matrix for with the numbers for each repeat
      if(take_means_draws){
        propensity_scores_total[, rp] <- propensity_scores_repeat
      } else{
        propensity_scores_total[, ((rp - 1) * samples + 1):(rp*samples)] <- propensity_scores_repeat
      }
    }
    
    #Return all the propensity score estimates
    return(propensity_scores_total)
  }

#' ps_analysis
#'
#' This function analyses the estimated propensity scores. This is necessary to look whether enough support is provided in estimatoin
#' One key point to look out for is that there are not too much values close to 0 or 1. This makes the estimates uninformative
#' Output:  Some graphs and statistics perhaps
#' Input:   @estimated_ps ... propoensity score estimates
#'          @control_variables ... control variables for which support needs to be checked 
ps_analysis <- function(estimated_ps, control_variables, ...){
  #Wat voor analysis is hier nodig
  
  #Kijk naar de verdeling van de propensity scores
  #Deel dit op op basis van bepaalde kenmerken
  #En normaal
  
  #Kijk naar de 'accuracy' van de propensity scores
  
  #Kijk in hoeverre bepaalde variables zijn meegenomen in het model (mogelijk?)
}