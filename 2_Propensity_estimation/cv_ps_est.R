#' Cross validation for ps estimation
#'
#' @description  function provides table with AUC for different parameter tuning and oversampling or not
#'
#' @return results table
#' @param data ... data for input
#' @param  k_fold ... k_fold_cross validation
#' @param  k_par ... vector with options for k
#' @param m_par ... vector with options for m
#'
cv_ps_est <-
  function(data = NULL,
           k_fold = 10,
           k_par = c(2, 3, 4),
           m_par = c(50, 100, 200)) {
    library(ROCR)
    library(imbalance)
    
    source("2_Propensity_estimation/propensity_score_estimation.R")
    source("3_Model_estimation/make_model_matrix_1.R")
    
    predictors_unb <- data$controls
    treatment_unb <- data$treatment
    
    aucs_all <- array(0, dim = c(length(k_par), length(m_par), 2))
    
 
    for (k_param in k_par) {
      for (m_param in m_par) {

        simple_ps_est <- ps_estimator(
          predictors_unb,
          treatment_unb,
          samples = 1000,
          technique = "BARTMACHINE",
          take_means_draws = TRUE,
          k_fold_cv = k_fold,
          repeats = 1,
          k_parameter = k_param,
          m_parameter = m_param
        )
        
        
        pred_simple <- prediction(simple_ps_est, treatment_unb$expRDAll)
        perf_simple <- performance(pred_simple, "tpr", "fpr")
        auc_ROCR_simple_1 <-
          performance(pred_simple, measure = "auc")
        auc_ROCR_simple_1 <- auc_ROCR_simple_1@y.values[[1]]
        
        model_dataset <-
          make_model_matrix_1(data, simple_ps_est)
        aucs_smote <- vector(length = k_fold)
        
        predictors <-
          model_dataset$controls[, 2:40] #Take out sample weight
        treatment <- model_dataset$treatment
        
        individuals_shuffle <- sample(nrow(predictors))
        
        shuffled_predictors <- predictors[individuals_shuffle,]
        shuffled_treatment <- treatment[individuals_shuffle,]
        
        
        folds <-
          cut(seq(1, nrow(shuffled_predictors)), breaks = k_fold, labels = FALSE)
        
        
        for (fold in 1:k_fold) {
          test_indicator <- which(folds == k_fold, arr.ind = TRUE)
          
          training_pred <-  shuffled_predictors[-test_indicator, ]
          test_pred <-      shuffled_predictors[test_indicator, ]
          test_pred <- as_tibble(test_pred)
          
          training_treat <- shuffled_treatment[-test_indicator]
          test_treat <- shuffled_treatment[test_indicator]
          test_treat <- as_tibble(test_treat)
          names(test_treat) <- c("expRDAll")
          test_treat$expRDAll <- as_factor(test_treat$expRDAll)
          
          
          
          os_input_1 <-
            as.data.frame(cbind(training_pred, training_treat))
          oversample_SMOTE_1 <-
            oversample(os_input_1,
                       0.8,
                       "SMOTE",
                       filtering = FALSE,
                       classAttr =  "training_treat")
          treatment_tib <- tibble(oversample_SMOTE_1$training_treat)
          names(treatment_tib) <- c("expRDAll")
          
          
          oversampled_ps_est <-
            ps_estimator(
              oversample_SMOTE_1[, !names(oversample_SMOTE_1) %in% c("training_treat")],
              treatment_tib,
              samples = 1000,
              technique = "BARTMACHINE",
              take_means_draws = TRUE,
              k_fold_cv = 1,
              repeats = 1,
              k_parameter = k_param,
              m_parameter = m_param,
              smote_kfold = TRUE
            )
          
          #Obtain predictions - note that no treatment is predicted
          #names(test_pred) <- names(  oversample_SMOTE_1[,!names(oversample_SMOTE_1) %in% c("training_treat")])
          ps_estimates <-
            bart_predict_for_test_data(oversampled_ps_est,
                                       test_pred,
                                       test_treat$expRDAll,
                                       prob_rule_class = 0.5)
          ps_estimates <- 1 - ps_estimates$p_hat
          
          pred_smote_1 <-
            prediction(ps_estimates,
                       test_treat)
          perf_smote_1 <- performance(pred_smote_1, "tpr", "fpr")
          auc_ROCR_smote_1 <-
            performance(pred_smote_1, measure = "auc")
          auc_ROCR_smote_1 <- auc_ROCR_smote_1@y.values[[1]]
          aucs_smote[fold] <- auc_ROCR_smote_1
          
        }
        aucs_all[match(k_param, k_par), match(m_param, m_par), 1] <- auc_ROCR_simple_1
        aucs_all[match(k_param, k_par), match(m_param, m_par), 2] <- mean(aucs_smote)
      }
    }
    
    return(aucs_all)
  }