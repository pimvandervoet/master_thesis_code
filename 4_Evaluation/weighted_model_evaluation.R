w_an_eval_prep <- function(loopsize_used, drawsPL_used, iterations_used, cross_long = "cross"){
  library(miceadds)
  
  #Pre initializer
  saved_posteriordraws <- vector()
  
  if(cross_long == "cross"){
  
    for(run in 1:iterations_used){
    
  saved_posteriordraws <- c(saved_posteriordraws, paste("C_Results_Weighted_Analysis/run_", run, "_cross_sample.RData", sep = "")) 
  
    }
  
  load("C_Results_Weighted_Analysis/moderators_cross.RData") #This is the moderator matrix from wanalysis_data_seperated 
  moderators <- moderators_cross
  remove(moderators_cross)
  
  }else if(cross_long == "long"){
    
    for(run in 1:iterations_used){
      
      saved_posteriordraws <- c(saved_posteriordraws, paste("C_Results_Weighted_Analysis/run_", run, "_long_sample.RData", sep = "")) 
      
    }
    
  load("C_Results_Weighted_Analysis/moderators_long.RData")
  moderators <- moderators_long
  remove(moderators_long)
    
  }
  
  nr_individuals <- dim(moderators)[1] 
  loopsize <- loopsize_used
  drawsPL <- drawsPL_used
  
  n_ATE_draws_PI <- loopsize * drawsPL

  #Select individual IDs for certain characteristics
  white_individuals <- which(moderators$race == "1.white/caucasian")
  black_individuals <- which(moderators$race == "2.black/african american")
  other_individuals <- which(moderators$race == "3.other")
  
  male_individuals <- which(moderators$sex == 1)
  female_individuals <- which(moderators$sex == 0)
  
  #Loop through results
  for(iteration in 1:iterations_used){
  #Load the results and fill matrices
    load.Rdata(saved_posteriordraws[iteration], "iteration_posteriordraws") 

    #Results savers
    PRS_results_syBP <- matrix(NA, nr_individuals, loopsize * drawsPL)
    PRS_results_BMI <- matrix(NA, nr_individuals, loopsize * drawsPL)
    PRS_results_waist <- matrix(NA, nr_individuals, loopsize * drawsPL)
    individual_matrix <-  matrix(NA, nr_individuals, loopsize)
    
    ATE_matrix <- matrix(NA, n_ATE_draws_PI, 3)
    
    CATE_list <- list("CATE_race" = list(
      "white.caucasian" = matrix(NA, n_ATE_draws_PI, 3),
      "black.african.american" = matrix(NA, n_ATE_draws_PI, 3),
      "other" =  matrix(NA, n_ATE_draws_PI, 3)
    ),
    "CATE_sex" = list(
      "male" =  matrix(NA, n_ATE_draws_PI, 3),
      "female" =  matrix(NA, n_ATE_draws_PI, 3)
    ))
    
    ITE_list <- list("syBP" =  matrix(NA, nr_individuals, loopsize),
                     "BMI" = matrix(NA, nr_individuals, loopsize),
                     "waist" = matrix(NA, nr_individuals, loopsize))
    
    
  #Seperate out the results for ease of working
  for(n_res in 1:loopsize){
    PRS_results_syBP[, ((n_res - 1) * drawsPL  + 1):(n_res * drawsPL)] <- iteration_posteriordraws[[(n_res-1)*6 + 1]]
    PRS_results_BMI[, ((n_res - 1) * drawsPL  + 1):(n_res * drawsPL)] <- iteration_posteriordraws[[(n_res-1)*6 + 2]]
    PRS_results_waist[, ((n_res - 1) * drawsPL  + 1):(n_res * drawsPL)] <- iteration_posteriordraws[[(n_res-1)*6 + 3]]
    individual_matrix[, n_res] <- iteration_posteriordraws[[(n_res-1)*6 + 4]]
    #ps_estimates[, n_res] <- saved_posteriordraws[[(n_res-1)* 6 + 5]]
  }
    
  remove(iteration_posteriordraws)
  
  #compute and save ate
  ATE_matrix[1: n_ATE_draws_PI, ] <- base::cbind(colMeans(PRS_results_syBP), colMeans(PRS_results_BMI), colMeans(PRS_results_waist))
  filename <- paste("ATE ", cross_long, iteration, ".RData" , sep = "")
  save(ATE_matrix, file = filename)
  remove(ATE_matrix)
  
  #compute and save cates
  for(PRS in 1:loopsize){
    
  #Obtain CATES  
  
  drawstofill <-((PRS-1)*drawsPL + 1):(PRS*drawsPL)
    
  CATE_list$CATE_race$white.caucasian[drawstofill, ] <- base::cbind(colMeans(PRS_results_syBP[which(individual_matrix[ , PRS] %in% white_individuals), drawstofill]),
                                                              colMeans(PRS_results_BMI[which(individual_matrix[ , PRS] %in% white_individuals), drawstofill]),
                                                              colMeans(PRS_results_waist[which(individual_matrix[ , PRS] %in% white_individuals), drawstofill]))
  CATE_list$CATE_race$black.african.american[drawstofill, ] <- base::cbind(colMeans(PRS_results_syBP[which(individual_matrix[ , PRS] %in% black_individuals), drawstofill]),
                                                                     colMeans(PRS_results_BMI[which(individual_matrix[ , PRS] %in% black_individuals), drawstofill]),
                                                                     colMeans(PRS_results_waist[which(individual_matrix[ , PRS] %in% black_individuals), drawstofill]))
  CATE_list$CATE_race$other[drawstofill, ] <- base::cbind(colMeans(PRS_results_syBP[which(individual_matrix[ , PRS] %in% other_individuals), drawstofill]),
                                                    colMeans(PRS_results_BMI[which(individual_matrix[ , PRS] %in% other_individuals), drawstofill]),
                                                    colMeans(PRS_results_waist[which(individual_matrix[ , PRS] %in% other_individuals), drawstofill]))
  
  CATE_list$CATE_sex$male[drawstofill, ] <- base::cbind(colMeans(PRS_results_syBP[which(individual_matrix[ , PRS] %in% male_individuals), drawstofill]),
                                                  colMeans(PRS_results_BMI[which(individual_matrix[ , PRS] %in% male_individuals), drawstofill]),
                                                  colMeans(PRS_results_waist[which(individual_matrix[ , PRS] %in% male_individuals), drawstofill]))
  CATE_list$CATE_sex$female[drawstofill, ] <- base::cbind(colMeans(PRS_results_syBP[which(individual_matrix[ , PRS] %in% female_individuals), drawstofill]),
                                                    colMeans(PRS_results_BMI[which(individual_matrix[ , PRS] %in% female_individuals), drawstofill]),
                                                    colMeans(PRS_results_waist[which(individual_matrix[ , PRS] %in% female_individuals), drawstofill]))
  
  }
  
  filename <- paste("CATE ", cross_long, iteration, ".RData" , sep = "")
  save(CATE_list, file = filename)
  remove(CATE_list)
  
  #Compute and save ITES
  for(PRS in 1:loopsize){
    #Obtain ITES
    
    drawstofill <-((PRS-1)*drawsPL + 1):(PRS*drawsPL)
    
    ITE_list$syBP[, PRS] <- rowMeans(PRS_results_syBP[, drawstofill]) 
    ITE_list$BMI[, PRS] <- rowMeans(PRS_results_BMI[, drawstofill]) 
    ITE_list$waist[, PRS] <- rowMeans(PRS_results_waist[, drawstofill])
    
  }
  
  ITE_list_big <- list("ITE" = ITE_list, "individuals" = individual_matrix)
  filename <- paste("ITE ", cross_long, iteration, ".RData" , sep = "")
  save(ITE_list, file = filename)
  remove(ITE_list_big)
  remove(ITE_list)
  
  #Remove results to save memory 

  remove(PRS_results_syBP)
  remove(PRS_results_BMI)
  remove(PRS_results_waist)
  remove(individual_matrix)
  
  }
  
}


w_an_eval <-  function(loopsize_used, drawsPL_used, iterations_used, cross_long = "cross"){
  
  library(bayestestR)
  library(miceadds)
  library(see)
  library(insight)
  #library(rstanarm)
  library(ggplot2)
  
  theme_set(theme_modern())
  
  #Add the results to eachother for analysis

  #Obtain names of RData files to load
  saved_ATES <- vector()
  for(iteration in 1:iterations_used){
    saved_ATES <- c(saved_ATES, paste("C_Results_Weighted_Analysis/ATE long", iteration, ".RData", sep = "")) 
  }
  
  for(iteration in 1:iterations_used){
    load.Rdata(saved_ATES[iteration], "iteration_posteriordraws") 
  }
  
  #ATE analysis
  for(outcome in outcomes){
    ATE_point_estimate <- mean("ATEDRAWS")
    ATE_syBP_CI <- ci(ATE_syBP_pd, method = "HDI")
    ATE_syBP_CI_plot <- plot(ATE_syBP_CI, ATE_syBP_pd, show_zero = TRUE)
    syBP_ltz <-
      length(ATE_syBP_pd[ATE_syBP_pd > 0]) / length(ATE_syBP_pd)
    evaluation_results[["ATE CIs"]][["syBP"]] <-
      list(
        "ATE CI syBP" = ATE_syBP_CI,
        "ATE CI plot" = ATE_syBP_CI_plot,
        "LTZ syBP" = syBP_ltz
      )
  }
  
}