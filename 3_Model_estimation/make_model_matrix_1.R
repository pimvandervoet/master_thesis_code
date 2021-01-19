#' Make BCF model matrix 
#' 
#' Own adaptation of make model matrix from dbarts package - tailored to our use case
#' 
#' @description Constructs a matrix for the BCF function from the data
#' @return list with output data
#' @param input_data
make_model_matrix_1 <- function(input_data, propensityscores){
  
  matrix_data <- input_data
  matrix_data$ps_estimates <- propensityscores
  
  #PS estaimtes, Treatment and outcomes to matrix, already numeric
  matrix_data$treatment <- as.matrix(matrix_data$treatment)
  matrix_data$outcomes <- as.matrix(matrix_data$outcomes)
  matrix_data$ps_estimates <- as.matrix(matrix_data$ps_estimates)
  
  #moderator matrix. Trasnfer non-ordered categorical data to dummies
  library(caret)
  catvars <- c("race")
  dmy_m <- dummyVars(" ~ race", data = matrix_data$moderators)
  trsf_m <- data.frame(predict(dmy_m, newdata = matrix_data$moderators))
  matrix_data$moderators <- cbind(matrix_data$moderators[,!names(matrix_data$moderators)%in% catvars], trsf_m)
  matrix_data$moderators <- data.matrix(matrix_data$moderators)
  
  #Control variable matrix
  catvars <- c("race", "mStat11", "mStat13")
  dmy_c <- dummyVars(" ~ race + mStat11 + mStat13", data = matrix_data$controls)
  trsf_c <- data.frame(predict(dmy_c, newdata = matrix_data$controls))
  matrix_data$controls <- cbind(matrix_data$controls[,!names(matrix_data$controls)%in% catvars], trsf_c)
  matrix_data$controls <- data.matrix(matrix_data$controls)
  
  return(matrix_data)
}