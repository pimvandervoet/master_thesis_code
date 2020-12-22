#' general_data_preperation
#'
#'
#' @description   This function puts the data in the right format to be used for analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return Data_prepared ... List with data in right format
#' Input: @param Raw_data .... Raw data as obtained from data_loader function
general_data_preperation <- function(data = raw_data){
  
  #Seperate treatment, outcomes and control variables - do not seperate moderating variables out yet
  
  #Make sure certain variables are factors (level variables)
  
  #Make sure that the multiple choice variables (q29 and q30 become one variable)
  
  #Make sure it in the right data format(dataframe?)
  
  
}

#' general_data_preperation
#'
#'
#' @description   This function puts the data in the right format to be used for analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return data_incl_racial_discrimination ... data list with variables for racial discrimination accorindg to user input
#' Input: @param data_cleaned .... cleaned data as obtained from general_data_preperation function
#'        @param method .... method used to define racial discrimination. Default is "Proposal" 
define_racial_discrimination <- function(data = data_cleaned, method = "Proposal"){

  }