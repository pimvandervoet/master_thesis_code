#' SMOTE resampler
#'
#' @description This function resamples the datset for propensity score estimation by SMOTE. This function is based on DMwR - SMOTE but tailored to this use case
#'
#' @return balanced dataset
#' @param unbalanced_dataset
#' @param perc.over
#' @param perc.under
#' @param k 

SMOTE_resampler <- function(unbalanced_dataset = NULL, perc.over = 200, perc.under = 200, k = 10){
  
    # the column where the target variable is
    tgt <- which(names(unbalanced_dataset) == "expRDAll")
    minCl <- levels(unbalanced_dataset$expRDAll)[2]
    
    # get the cases of the minority class
    minExs <- which(unbalanced_data[,tgt] == minCl)
    
    # generate synthetic cases from these minExs
    if (tgt < ncol(unbalanced_dataset)) {
      cols <- 1:ncol(unbalanced_dataset)
      cols[c(tgt,ncol(unbalanced_dataset))] <- cols[c(ncol(unbalanced_dataset),tgt)]
      unbalanced_dataset <-  unbalanced_dataset[,cols]
    }
    newExs <- smote.exs(unbalanced_dataset[minExs,],ncol(unbalanced_dataset),perc.over,k)
    if (tgt < ncol(unbalanced_dataset)) {
      newExs <- newExs[,cols]
      unbalanced_dataset <- unbalanced_dataset[,cols]
    }
    
    # get the undersample of the "majority class" examples
    selMaj <- sample((1:NROW(unbalanced_dataset))[-minExs],
                     as.integer((perc.under/100)*nrow(newExs)),
                     replace=T)
    
    # the final data set (the undersample+the rare cases+the smoted exs)
    newdataset <- rbind(data[selMaj,],data[minExs,],newExs)
    
    # learn a model if required
    if (is.null(learner)) return(newdataset)
    else do.call(learner,list(form,newdataset,...))
  
  
}