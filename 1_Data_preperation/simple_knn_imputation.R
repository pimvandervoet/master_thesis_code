#' simple_knn
#'
#' @description This function imputes missing values in a dataset simply by using k-nearest neighbours
#'
#' @return complete_data
#' @param incomplete_data .. data to be filed
#' @param k ... k in knn
#' @param cat_vars ... all categorical variables
#' 
simple_knn <- function(incomplete_data = seperated_data, k = 10){
  
  
  #categorical_names <- c("wealth_bin", "education", "mStat11", "mStat13", "prevRetStat", "workingNow", "married", "drinker13", "drinker11", "smoker13", "smoker11", "smokenow", "vigAct", "modAct", "milAct") 
  
    
  incomplete_controls <- incomplete_data$controls
  
  #for(variable in categorical_names){
  #incomplete_controls[, variable] <- as_factor(incomplete_controls[, variable])
  #}
  
  incomplete_controls <- incomplete_controls[,!names(incomplete_controls) %in% c("everSmoke", "nSmokepw", "married", "nSmokemos") ]
  #Make dataframe to matrix. 
  
  #incomplete_controls <- as.matrix(incomplete_controls)
  
  #Do KNN
  complete_data <- VIM::kNN(incomplete_controls, colnames(incomplete_controls), k = k, impNA = TRUE)
  complete_data <- complete_data[, 1:26]
  complete_data <- complete_data[complete.cases(complete_data),]
  return(complete_data)
}