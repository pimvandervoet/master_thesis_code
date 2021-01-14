#' Simple resampler
#'
#' @description This function creates an artificial 'representative sample' by resampling each respondent sampleWeight times
#' 
#' @return resampled data
#' 
#' @param dataset
#' 
simple_resampler <- function(dataset = analysis_dataset){
  
  weights <- analysis_dataset$sampleWeight
  resampled_data <- analysis_dataset[rep(seq_len(nrow(dataset)), weights),]
  
  return(resampled_data)
}