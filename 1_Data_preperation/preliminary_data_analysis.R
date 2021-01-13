#' summary-statistics
#'
#' @description This function provides basic summary statistics used in the report.
#'
#' @return sumstats ... list with summary statistics per split
#'
#' @param input_data ... list of input data
#' @param main_split ...  main split for the data - default: treatment
#' @param further_splits ... if further splits are desired this can be taken into account (for looking at heterogeneous effects: suggested to split on moderators)
#'
summary_statistics <-
  function(input_data = data_seperated,
           main_split = "treatment",
           further_splits = NULL) {
    
    #Select the different kind of variables form the full dataset
    control <- input_data[[1]]
    moderator <- input_data[[2]]
    treatment <- input_data[[3]]
    outcomes <- input_data[[4]]
    
    sumstats <- list()
    listofsumstats <- list("outcomes" = outcomes, "moderator" = moderator, "control" = control)
    
    #Make subsamples to cycle through
    if (!is.null(main_split)) {
      if (main_split == "treatment") {
        #Select rownumbers for which treatment is 1 and 0
        yes_treatment <- which(treatment$expRDAll == 1)
        no_treatment <- which(treatment$expRDAll == 0)
    
        listofsumstats[["Treated_outcomes"]] <- outcomes[yes_treatment, ]
        listofsumstats[["Non_treated_outcomes"]] <- outcomes[no_treatment,]
        listofsumstats[["Treated_moderators"]] <- moderator[yes_treatment,]
        listofsumstats[["Non_treated_moderators"]] <- moderator[no_treatment,]
        listofsumstats[["Treated_control"]] <- control[yes_treatment, ]
        listofsumstats[["Non_treated_control"]]  <- control[no_treatment, ]
        
        
        if (!is.null(further_splits)) {
          
          for (i in further_splits) {
            
            if (i == "race") {
              yes_split <- which(control$race == "1.white/caucasian")
              no_split <-
                which(control$race == "2.black/african american")
              
            } else if (i == "sex") {
              yes_split <- which(control$sex == 1) #male
              no_split <- which(control$sex == 0) #female
              
            } else if (i == "age") {
              yes_split <- which(control$age <= mean(control$age))
              no_split <- which(control$age > mean(control$age))
              
            } else if (i == "wealthCalc") {
              yes_split <- which(control$wealth_bin <= 5)
              no_split <- which(control$wealth_bin > 5)
            }
            
            listofsumstats[[paste(i,"yes treatment" ,"split")]] <- outcomes[prob::intersect(yes_treatment, yes_split),]
            listofsumstats[[paste(i, "no treatment" ,"split")]] <- outcomes[prob::intersect(no_treatment, yes_split),]
            listofsumstats[[paste(i, "yes treatment" ,"split")]] <- moderator[prob::intersect(yes_treatment, yes_split),]
            listofsumstats[[paste(i, "no treatment","split")]] <- moderator[prob::intersect(no_treatment, yes_split),]
            listofsumstats[[paste(i, "yes treatment" ,"split")]] <- control[prob::intersect(yes_treatment, yes_split), ]
            listofsumstats[[paste(i, "no treatment","split")]] <- control[prob::intersect(no_treatment, yes_split), ]
            
            listofsumstats[[paste(i, "yes treatment" ,"no_split")]] <- outcomes[prob::intersect(yes_treatment, no_split),]
            listofsumstats[[paste(i, "no treatment","no_split")]] <- outcomes[prob::intersect(no_treatment, no_split),]
            listofsumstats[[paste(i, "yes treatment" ,"no_split")]] <- moderator[prob::intersect(yes_treatment, no_split),]
            listofsumstats[[paste(i, "no treatment","no_split")]] <- moderator[prob::intersect(no_treatment, no_split),]
            listofsumstats[[paste(i, "yes treatment" ,"no_split")]] <- control[prob::intersect(yes_treatment, no_split), ]
            listofsumstats[[paste(i, "no treatment","no_split")]] <- control[prob::intersect(no_treatment, no_split), ]
          }
          
        }
        
        
      }
      
    }
    

    #Cycle through all sets and obtain summary statistics
    for (i in 1:length(listofsumstats)) {
      sumstats[[names(listofsumstats)[i]]] <- summary(listofsumstats[[i]])
    }
    
    
    #produce the summary statistics by cycling through the list
    
    #We want to calc means for doubles
    #We want to do counts for levels of factors
    #We want to compute standard deviations for others?
    
    return(sumstats)
}

#' preliminary_treatment_analysis
#'
#' @description This function provides basic summary statistics used in the report.
#'
#' @return treatment_analysis ... list with treament_analysis: boxplots for no treatment and treatment
#'
#' @param data_seperated ... list of input data
#' @param varsOfInterest ...  variables of interest to consider in the treatment (propensity score) analysis
#'
treatment_analysis <-
  function(input_data = data_seperated,
           varsOfInterest = c("race", "sex", "age", "wealthCalc", "sex")) {
    
  }