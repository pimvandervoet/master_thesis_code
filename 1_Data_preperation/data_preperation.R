#' data_seperator
#'
#'
#' @description   This function retreieves the seperate matrices form the data that need to be used for further analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return data_list ... List with data in the rigt matrices 
#' Input: @param raw_data .... Raw data as obtained from data_loader function
#' @param control_variables ... column numbers of control variables
#' @param moderator_variables ... column numbers of moderator variables
#' @param treatment_variables ... column numbers of treatment variables
#' @param outcomes .... column numbers of outcome variables
data_seperator <- function(data = raw_data, controL_columns = NA, moderator_columns = NA, treatment_columns = NA, outcome_columns = NA){
  
  #Make seperate dataframes
  if(!is.na(control_columns)){
    control_variables <- raw_data[, control_columns]
  }else{
    control_variables <- NA
  }
  
  if(!is.na(moderator_columns)){
    moderator_variables <- raw_data[, moderator_columns]
  }else{
    moderator_variables <- NA
  }
  
  if(!is.na(treatment_columns)){
    treatment <- raw_data[, treatment_columns]
  }else{
    treatment <- NA
  }
  
  if(!is.na(outcome_columns)){
    outcomes <- raw_data[, outcome_columns]
  }else{
    outcomes <- NA
  }
  
  #Return list 
  data_list <- list("controls" = control_variables, "moderators" = moderator_variables, "treatment" = treatment, "outcomes" = outcomes)
  return(data_list)
  #Seperate treatment, outcomes and control variables - do not seperate moderating variables out yet
  
  #Make sure certain variables are factors (level variables)
  
  #Make sure that the multiple choice variables (q29 and q30 become one variable)
  
  #Make sure it in the right data format(dataframe?)
}

#' data_formatter
#' 
#' @description This function constructs variables for analysis. Custom-made for master thesis analysis. 
#' 
#' Output:
#' @return formatted_data
#' 
#' Input:
#' @param non_formatted_data .... list of dataframe of non-formatted data to be formatted
#'
data_formatter <- function(non_formatted_data = NA) {
  if (!is.na(non_formatted_data)) {
    
  } else{
    (print("No data has been provided"))
  }
}

#' general_data_preperation
#'
#'=
#' @description   This function puts the data in the right format to be used for analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return data_incl_racial_discrimination ... data list with variables for racial discrimination accorindg to user input
#' Input: @param data_cleaned .... cleaned data as obtained from general_data_preperation function
#'        @param method .... method used to define racial discrimination. Default is "Proposal" 
define_racial_discrimination <- function(data = data_cleaned, method = "Proposal"){

  }