#' simple_knn
#'
#' @description This function imputes missing values in a dataset simply by using k-nearest neighbours
#'
#' @return complete_data
#' @param incomplete_data .. data to be filed
#' @param k_fold ... k in knn
#' 
simple_knn <- function(incomplete_data = seperated_data, k_fold = 10){
  
  
  #categorical_names <- c("wealth_bin", "education", "mStat11", "mStat13", "prevRetStat", "workingNow", "married", "drinker13", "drinker11", "smoker13", "smoker11", "smokenow", "vigAct", "modAct", "milAct") 
  
  complete_data <- incomplete_data  
  incomplete_controls <- incomplete_data$controls
  no_controls <- dim(incomplete_controls)[2]
  
  incomplete_controls <- incomplete_controls[,!names(incomplete_controls) %in% c("everSmoke", "nSmokepw", "married", "nSmokemos") ]

  #Do KNN
  complete_data$controls <- VIM::kNN(incomplete_controls, colnames(incomplete_controls), k = k_fold, impNA = TRUE)
  complete_data$controls <- complete_data$controls[, 1:no_controls]
  complete_rows <- complete.cases(complete_data$controls) 
  
  complete_data$treatment <- complete_data$treatment[complete_rows, ]
  complete_data$controls <- complete_data$controls[complete_rows,] 
  complete_data$moderators <- complete_data$moderators[complete_rows, ]
  complete_data$outcomes <- complete_data$outcomes[complete_rows, ]
  
  return(complete_data)
}

#' knn_impute_moEducation
#'
#' @description This function imputes missing values in a dataset simply by using k-nearest neighbours
#'
#' @return complete_data
#' @param incomplete_data .. data to be filed
#' @param k_fold ... k in knn
#' 
knn_impute_moEducation <- function(incomplete_data = seperated_data, k_fold = 10){
  
  
  complete_data <- incomplete_data  
  incomplete_controls <- incomplete_data$controls
  no_controls <- dim(incomplete_controls)[2]
  
  incomplete_controls_to_impute <- c("moEducation")

  #Do KNN
  complete_data$controls <-
    VIM::kNN(
      data = incomplete_controls,
      variable = incomplete_controls_to_impute,
      k = k_fold,
      dist_var = c("sex", "race", "wealth_bin", "age", "education"),
      impNA = TRUE
    )
  complete_data$controls <- complete_data$controls[, 1:no_controls]

  return(complete_data)
}

#' mode_impute_education
#'
#' @description This function imputes missing values in a dataset simply by using k-nearest neighbours
#'
#' @return complete_data
#' @param incomplete_data .. data to be filed
#' @param k_fold ... k in knn
#' 
mode_impute_education <- function(incomplete_data = seperated_data, k_fold = 10){
  
 
library(dplyr)  
  
  complete_data <- incomplete_data  
  incomplete_controls <- incomplete_data$controls
 
  #Auxiliarry function
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  #Do imputation
  complete_data$controls <- mutate(complete_data$controls, education = coalesce(education, getmode(education)))
    
  
  return(complete_data)
}