#' Propensity_score_estimation
#'
#' @description  This function provides estimates of the propensity scores that can be used in the BCF model
#'
#' Output: @return propensity_estimates .... n x samples*repeats or n x repeats draws of propensity score estimates
#' Input:  @param predictors ... variables used to estimate the propensity scores
#'         @param treatment ... variable that indicates treatment or not for each individual
#'         @param samples  .... number of posterior samples that user wants per individual per cross fold, default 1000
#'         @param technique ....  technique speciified by the user that is used for estimation. Default = "BARTMACHINE"
#'         @param take_means_draws ... = note used anymore, default is to take posterior mean -- Logical variable indicating whether mean of all posterior draws for each k_fold needs to be taken, default TRUE
#'         @param k_fold_cv .... number of cross validations, default 10
#'         @param repeats ... number of repeats of cross validation, default 1

ps_estimator <-
  function(predictors,
           treatment,
           samples = 1000,
           technique = "BARTMACHINE",
           take_means_draws = TRUE,
           k_fold_cv = 10,
           repeats = 1) {
    #Preprocessing steps if needed
    set.seed(30121997)
    
    #Empty propensity scores matrix for all repeats
    # if(take_means_draws){
    #  propensity_scores_total <-
    #    matrix(NA, nrow(predictors), ncol = repeats)
    #}else{
    propensity_scores_total <-
      matrix(NA, nrow(predictors), ncol = repeats)
    #}
    
    #Make input from list to matrix if necessary (treatment is vector already so need no changes) ----- volgens mij gaat het hier fout???
    # if (typeof(predictors) == "list") {
    #  predictors <- matrix(unlist(predictors), nrow = dim(predictors)[1], byrow = TRUE)
    #}
    
    #Set number of results per cross validation step
    # if (take_means_draws) {
    #  results_per_cv = 1
    #} else
    #  (
    #   results_per_cv = samples
    #  )
    
    #Do repeats for repeated cross validation
    for (rp in 1:repeats) {
      #Make emtpty matrix for results of each repeat
      propensity_scores_repeat <- matrix(NA, nrow(predictors), 1)
      
      #Make sample for each repeat
      original_order <- seq(1:nrow(predictors))
      individuals_shuffle <- sample(nrow(predictors))
      
      predictors$original_order <- original_order
      treatment$original_order <- original_order
      
      shuffled_predictors <- predictors[individuals_shuffle,]
      shuffled_treatment <- treatment[individuals_shuffle,]
      
      if (k_fold_cv > 1) {
        folds <-
          cut(seq(1, nrow(shuffled_predictors)), breaks = k_fold_cv, labels = FALSE)
      }
      
      #Loop over k cross folds
      for (k_fold in 1:k_fold_cv) {
        #Select train and test
        if (k_fold_cv > 1) {
          test_indicator <- which(folds == k_fold, arr.ind = TRUE)
          train_predictors <-
            shuffled_predictors[-test_indicator,!names(shuffled_predictors) %in% c("original_order")]
          test_predictors <-
            shuffled_predictors[test_indicator,!names(shuffled_predictors) %in% c("original_order")]
          train_treatment <-
            shuffled_treatment[-test_indicator,!names(shuffled_treatment) %in% c("original_order")]
          train_treatment <- pull(train_treatment, expRDAll)
          
          test_treatment <-
            shuffled_treatment[test_indicator,!names(shuffled_treatment) %in% c("original_order")]
          test_treatment <- pull(test_treatment, expRDAll)
          
        } else if (k_fold_cv == 1) {
          train_predictors <-
            shuffled_predictors[,!names(shuffled_predictors) %in% c("original_order")]
          train_treatment <-
            shuffled_treatment[,!names(shuffled_treatment) %in% c("original_order")]
          train_treatment <- pull(train_treatment, expRDAll)
        }
        
        
        #Make sure train and test predictors are dataframe (unlist function)
        
        #Actual estimation - default method is BART
        if (technique == "BART") {
          library(BART)
          
          model_execution <- gbart(
            x.train = train_predictors,
            y.train = train_treatment,
            x.test = test_predictors,
            #Probit - can use logit by lbart
            type = 'pbart',
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
        } else if (technique == "BARTMACHINE") {
          options(java.parameters = "-Xmx10g")
          library(bartMachine)
          set_bart_machine_num_cores(7)
          #Check if train treatment is binary
          if (typeof(train_treatment) == "double") {
            train_treatment <- as_factor(train_treatment)
            if (k_fold_cv > 1) {
              test_treatment <- as_factor(test_treatment)
            }
          }
          
          
          if (k_fold_cv > 1) {
            #Estimate model
            model_execution_obj <- bartMachine(
              X = train_predictors,
              y = train_treatment,
              num_trees = 50,
              num_burn_in = 250,
              num_iterations_after_burn_in = samples,
              alpha = 0.95,
              beta = 2,
              k = 2,
              q = 0.9,
              nu = 3,
              prob_rule_class = 0.5,
              mh_prob_steps = c(2.5, 2.5, 4) / 9,
              debug_log = FALSE,
              run_in_sample = FALSE,
              s_sq_y = "mse",
              sig_sq_est = NULL,
              cov_prior_vec = NULL,
              use_missing_data = FALSE,
              covariates_to_permute = NULL,
              num_rand_samps_in_library = 10000,
              use_missing_data_dummies_as_covars = FALSE,
              replace_missing_data_with_x_j_bar = FALSE,
              impute_missingness_with_rf_impute = FALSE,
              impute_missingness_with_x_j_bar_for_lm = TRUE,
              mem_cache_for_speed = TRUE,
              flush_indices_to_save_RAM = TRUE,
              serialize = FALSE,
              seed = NULL,
              verbose = TRUE
            )
            
            
            #Obtain predictions - note that no treatment is predicted
            model_execution <-
              bart_predict_for_test_data(model_execution_obj,
                                         test_predictors,
                                         test_treatment,
                                         prob_rule_class = 0.5)
          } else if (k_fold_cv == 1) {
            model_execution_obj <- bartMachine(
              X = train_predictors,
              y = train_treatment,
              num_trees = 50,
              num_burn_in = 250,
              num_iterations_after_burn_in = samples,
              alpha = 0.95,
              beta = 2,
              k = 2,
              q = 0.9,
              nu = 3,
              prob_rule_class = 0.5,
              mh_prob_steps = c(2.5, 2.5, 4) / 9,
              debug_log = FALSE,
              run_in_sample = TRUE,
              s_sq_y = "mse",
              sig_sq_est = NULL,
              cov_prior_vec = NULL,
              use_missing_data = FALSE,
              covariates_to_permute = NULL,
              num_rand_samps_in_library = 10000,
              use_missing_data_dummies_as_covars = FALSE,
              replace_missing_data_with_x_j_bar = FALSE,
              impute_missingness_with_rf_impute = FALSE,
              impute_missingness_with_x_j_bar_for_lm = TRUE,
              mem_cache_for_speed = TRUE,
              flush_indices_to_save_RAM = TRUE,
              serialize = FALSE,
              seed = NULL,
              verbose = TRUE
            )
            
            # model_execution_obj <- bartMachineCV(
            #   X = train_predictors,
            #   y = train_treatment,
            #   Xy = NULL,
            #   num_tree_cvs = c("10", "50" ,"100", "200"),
            #   k_cvs = c(1,2,3,4,5),
            #   nu_q_cvs = NULL,
            #   k_folds <- 5,
            #   verbose = TRUE)
            model_execution <- model_execution_obj$p_hat_train
          }
        }
        
        #Fill output matrix with output for crossfold
        if (k_fold_cv > 1) {
          predictions <- 1 - model_execution$p_hat
        } else if (k_fold_cv == 1) {
          predictions <- 1 - model_execution
        }
        
        #Note that the output needs to be transposed as the gbart function provides each posterior draw in each row
        if (k_fold_cv > 1) {
          propensity_scores_repeat[test_indicator,] <- predictions
        } else if (k_fold_cv == 1) {
          propensity_scores_repeat <- predictions
        }
      }
      
      #Fill total matrix for with the numbers for each repeat and put back in order!
      # if(take_means_draws){ 
     # propensity_scores_repeat <- cbind(propensity_scores_repeat, shuffled_treatment$original_order)
      #propensity_scores_repeat_ordered <- propensity_scores_repeat[order(propensity_scores_repeat[, 2])]
      propensity_scores_total[, rp] <- propensity_scores_repeat[order(shuffled_treatment$original_order)]
      # } else{
      #   propensity_scores_total[, ((rp - 1) * samples + 1):(rp*samples)] <- propensity_scores_repeat
      # }
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