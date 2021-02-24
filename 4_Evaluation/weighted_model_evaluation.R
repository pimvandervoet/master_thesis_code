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
  save(ITE_list_big, file = filename)
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
  library(rstanarm)
  library(ggplot2)
  library(dplyr)
  library(easystats)
  
  theme_set(theme_modern())
  resultslist <- list("ATES" = list(), "CATES" = list(), "ITES"  = list())
  
  #Add the results to eachother for analysis

  #ATES####
  #Obtain names of RData files to load
  saved_ATES <- vector()
  for(iteration in 1:iterations_used){
    saved_ATES <- c(saved_ATES, paste("C_Results_Weighted_Analysis/ATE ", cross_long, iteration, ".RData", sep = "")) 
  }
  
  #Join those files in ATEs
  n_ATES_PI <- drawsPL_used * loopsize_used 
  ATES <- matrix(NA, n_ATES_PI * iterations_used, 3)
  
  for(iteration in 1:iterations_used){
    load.Rdata(saved_ATES[iteration], "ATE_iteration") 
    
    rowstofill <- ((iteration - 1)* n_ATES_PI + 1):(iteration * n_ATES_PI)
    ATES[rowstofill, ] <- ATE_iteration 
    
  }
 
  
  #ATE analysis
  
  ATE_point_estimates <- apply(ATES, 2, mean)
  ATE_CIs <- apply(ATES, 2, ci)
  
  ATE_CI_plot_syBP <- plot(ATE_CIs[[1]], ATES[,1], show_zero = TRUE)
  ATE_CI_plot_BMI <- plot(ATE_CIs[[2]], ATES[,2], show_zero = TRUE)
  ATE_CI_plot_waist <- plot(ATE_CIs[[3]], ATES[,3], show_zero = TRUE)
  
  ATES_vector <- as.vector(ATES)
  ATES_names <- c(rep("systolic Blood Pressure", length(ATES_vector)/3), rep("BMI", length(ATES_vector)/3), rep("waist", length(ATES_vector)/3))

  ATES_plotdata <- data.frame(ATES_vector, ATES_names)
  ATES_plotdata[,2] <- as.factor(ATES_plotdata[,2])

  ATES_plotdata <- ATES_plotdata %>%
    group_by(ATES_names)
  
  #Make nice plots - need to get working
  #ATES_estimates <- estimate_density(ATES_plotdata)
  
  #ATE_plots <- plot(ATES_estimates)
  
  ATE_pd <- p_direction(as.data.frame(ATES))
  
  resultslist[["ATES"]] <- list("ATE point estimates" = ATE_point_estimates, 
                                "ATE CIs" = ATE_CIs, 
                                "ATE CI plots" = list("syBP" = ATE_CI_plot_syBP, "BMI" = ATE_CI_plot_BMI, "waist" = ATE_CI_plot_waist),
                                "ATE pd" = ATE_pd)

  #CATES####
  saved_CATES <- vector()
  for(iteration in 1:iterations_used){
    saved_CATES <- c(saved_CATES, paste("C_Results_Weighted_Analysis/CATE ", cross_long, iteration, ".RData", sep = "")) 
  }
  
  n_CATES_PI <- drawsPL_used * loopsize_used 
  CATES <- matrix(NA, n_CATES_PI * iterations_used, 3*5)
  
  for(iteration in 1:iterations_used){
    load.Rdata(saved_CATES[iteration], "CATE_iteration") 
    
    rowstofill <- ((iteration - 1)* n_CATES_PI + 1):(iteration * n_CATES_PI)
    CATES[rowstofill, 1:3] <- CATE_iteration$CATE_race$white.caucasian
    CATES[rowstofill, 4:6] <- CATE_iteration$CATE_race$black.african.american
    CATES[rowstofill, 7:9] <- CATE_iteration$CATE_race$other 
    CATES[rowstofill, 10:12] <- CATE_iteration$CATE_sex$male 
    CATES[rowstofill, 13:15] <- CATE_iteration$CATE_sex$female
    
    
  }
  
  
  #CATE analysis
  
  CATE_point_estimates <- apply(CATES, 2, mean)
  CATE_CIs <- apply(CATES, 2, ci)
  
  white_CATE_CI_plot_syBP <- plot(CATE_CIs[[1]], CATES[,1], show_zero = TRUE)
  white_CATE_CI_plot_BMI <- plot(CATE_CIs[[2]], CATES[,2], show_zero = TRUE)
  white_CATE_CI_plot_waist <- plot(CATE_CIs[[3]], CATES[,3], show_zero = TRUE)
  black_CATE_CI_plot_syBP <- plot(CATE_CIs[[4]], CATES[,4], show_zero = TRUE)
  black_CATE_CI_plot_BMI <- plot(CATE_CIs[[5]], CATES[,5], show_zero = TRUE)
  black_CATE_CI_plot_waist <- plot(CATE_CIs[[6]], CATES[,6], show_zero = TRUE)
  other_CATE_CI_plot_syBP <- plot(CATE_CIs[[7]], CATES[,7], show_zero = TRUE)
  other_CATE_CI_plot_BMI <- plot(CATE_CIs[[8]], CATES[,8], show_zero = TRUE)
  other_CATE_CI_plot_waist <- plot(CATE_CIs[[9]], CATES[,9], show_zero = TRUE)
  male_CATE_CI_plot_syBP <- plot(CATE_CIs[[10]], CATES[,10], show_zero = TRUE)
  male_CATE_CI_plot_BMI <- plot(CATE_CIs[[11]], CATES[,11], show_zero = TRUE)
  male_CATE_CI_plot_waist <- plot(CATE_CIs[[12]], CATES[,12], show_zero = TRUE)
  female_CATE_CI_plot_syBP <- plot(CATE_CIs[[13]], CATES[,13], show_zero = TRUE)
  female_CATE_CI_plot_BMI <- plot(CATE_CIs[[14]], CATES[,14], show_zero = TRUE)
  female_CATE_CI_plot_waist <- plot(CATE_CIs[[15]], CATES[,15], show_zero = TRUE)
  
  CATES_vector <- as.vector(CATES)
  CATES_groups <- c(rep("White.Caucasian", length(CATES_vector)/5), 
                    rep("Black.African.American", length(CATES_vector)/5),
                    rep("Other", length(CATES_vector)/5),
                    rep("Male", length(CATES_vector)/5),
                    rep("Female", length(CATES_vector)/5))
  CATES_names <- c(rep("systolic Blood Pressure", length(CATES_vector)/15), rep("BMI", length(CATES_vector)/15), rep("waist", length(CATES_vector)/15))
  CATES_names <- rep(CATES_names, 5)
  
  CATES_plotdata <- data.frame(CATES_vector, CATES_names, CATES_groups)
  CATES_plotdata[,2] <- as.factor(CATES_plotdata[,2])
  CATES_plotdata[,3] <- as.factor(CATES_plotdata[,3])
  
  CATES_plotdata <- CATES_plotdata %>%
    group_by(CATES_names, CATES_groups)
  
  
  #Make nice plots - need to get working
  #CATES_estimates <- estimate_density(CATES_plotdata)
  
  #CATE_plots <- plot(CATES_estimates)
  
  CATE_pd <- p_direction(as.data.frame(CATES))
  
  resultslist[["CATES"]] <- list("CATE point estimates" = CATE_point_estimates, 
                                "CATE CIs" = CATE_CIs, 
                                "CATE CI plots" = list("White" = list("syBP" = white_CATE_CI_plot_syBP, "BMI" = white_CATE_CI_plot_BMI, "waist" = white_CATE_CI_plot_waist),
                                                        "Black" = list("syBP" = black_CATE_CI_plot_syBP, "BMI" = black_CATE_CI_plot_BMI, "waist" = black_CATE_CI_plot_waist),
                                                       "other" = list("syBP" = other_CATE_CI_plot_syBP, "BMI" = other_CATE_CI_plot_BMI, "waist" = other_CATE_CI_plot_waist),
                                                       "male" = list("syBP" = male_CATE_CI_plot_syBP, "BMI" = male_CATE_CI_plot_BMI, "waist" = male_CATE_CI_plot_waist),
                                                       "female" = list("syBP" = female_CATE_CI_plot_syBP, "BMI" = female_CATE_CI_plot_BMI, "waist" = female_CATE_CI_plot_waist)
                                                       ),
                                "CATE pd" = CATE_pd)
  
  ### ITES ####

  #NEED TO fix when ITE list big is created form previous one....
  saved_ITES <- vector()
  for(iteration in 1:iterations_used){
    saved_ITES <- c(saved_ITES, paste("C_Results_Weighted_Analysis/ITE ", cross_long, iteration, ".RData", sep = ""))
  }

  n_ITES_PI <- drawsPL_used * loopsize_used


  for(iteration in 1:iterations_used){
    load.Rdata(saved_ITES[iteration], "ITE_iteration")

    if(!exists("ITES")){
      n_ITES_PI <- dim(ITE_iteration$ITE$syBP)[1] * loopsize_used
      ITES <- matrix(NA, iterations_used*n_ITES_PI, 3)
      individuals <- rep(NA, iterations_used*n_ITES_PI)
    }

    rowstofill <- ((iteration - 1)* n_ITES_PI + 1):(iteration * n_ITES_PI)
    ITES[rowstofill, 1] <- as.vector(ITE_iteration$ITE$syBP)
    ITES[rowstofill, 2] <- as.vector(ITE_iteration$ITE$BMI)
    ITES[rowstofill, 3] <- as.vector(ITE_iteration$ITE$waist)

    individuals[rowstofill] <- as.vector(ITE_iteration$individuals)
   }
  
  library(stats)
  quantilesfu <- function(dat){
    res <- quantile(dat, c(0.1, 0.9)) #Declare quantiles of interest
    return(res)
  }
  quantiles_oi <- apply(ITES, 2, quantilesfu)
  
  #Select individuals from relevant parts of data
  syBP_top_10p <- individuals[which(ITES[,1]>quantiles_oi[2,1])]
  syBP_mid_80p <- individuals[which(ITES[,1]>quantiles_oi[1,1] & ITES[,1]<quantiles_oi[2,1])]
  syBP_bot_10p <- individuals[which(ITES[,1]<quantiles_oi[1,1])]
  
  BMI_top_10p <- individuals[which(ITES[,2]>quantiles_oi[2,2])]
  BMI_mid_80p <- individuals[which(ITES[,2]>quantiles_oi[1,2] & ITES[,2]<quantiles_oi[2,2])]
  BMI_bot_10p <- individuals[which(ITES[,2]<quantiles_oi[1,2])]
  
  waist_top_10p <- individuals[which(ITES[,3]>quantiles_oi[2,3])]
  waist_mid_80p <- individuals[which(ITES[,3]>quantiles_oi[1,3] & ITES[,3]<quantiles_oi[2,3])]
  waist_bot_10p <- individuals[which(ITES[,3]<quantiles_oi[1,3])]
  
  resultslist[["ITES"]] <- list("syBPtop10" = syBP_top_10p,
                                "syBPmid80" = syBP_mid_80p,
                                "syBPbot10" = syBP_bot_10p,
                                "BMItop10" = BMI_top_10p,
                                "BMImid80" = BMI_mid_80p,
                                "BMIbot10" = BMI_bot_10p,
                                "waisttop10" = waist_top_10p,
                                "waistmid80" = waist_mid_80p,
                                "waistbot10" = waist_bot_10p
                                )
  
  return(resultslist)
  
}