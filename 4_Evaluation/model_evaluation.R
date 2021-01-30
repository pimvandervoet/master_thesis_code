#' evalpost
#' 
#' @description evaluation function BCF posterior results
#' 
#' @return evaluation_results - list with desired output 
#' 
#' @param bcf_post ... posterior-result from BCF_function
#' @param evaluation_methods ... desired output, default c("ATE", "Credibility interval", "CATE per moderator")
evalpost <- function(bcf_post = NULL, evaluation_methods = c("ATE", "Credibility interval", "CATEs", "plots ATE", "plots CATE")){
  if(!is.null(bcf_post)){
    
    #Create empty list to store results
    evaluation_results <- list()
    
    #First obtain ATE for each outcome
    if("ATE" %in% evaluation_methods){
      ATE_syBP <- mean(colMeans(bcf_post$`posterior_results syBP`$tau))
      ATE_BMI <- mean(colMeans(bcf_post$`posterior_results BMI`$tau))
      ATE_waist <- mean(colMeans(bcf_post$`posterior_results waist`$tau))
      evaluation_results[["ATEs"]] <- list("ATEs" = cbind(ATE_syBP, ATE_BMI, ATE_waist))
    }
    
    #Obtain Credibility intervals for ATE and Credibility intervals plus numbers for CATES
    if("Credibility interval" %in% evaluation_methods){
      library(bayestestR)
      library(see)

      if("ATE" %in% evaluation_methods){
      #ATE credibility intervals
        ATE_syBP_pd <- rowMeans(bcf_post$`posterior_results syBP`$tau) 
        ATE_syBP_CI <- ci(ATE_syBP_pd, method = "HDI")
        ATE_syBP_CI_plot <-  plot(ATE_syBP_CI, ATE_syBP_pd, show_zero = TRUE)
        syBP_ltz <- length(ATE_syBP_pd[ATE_syBP_pd>0])/length(ATE_syBP_pd)
        evaluation_results[["ATE CIs"]][["syBP"]] <- list("ATE CI syBP" = ATE_syBP_CI, "ATE CI plot" = ATE_syBP_CI_plot, "LTZ syBP" = syBP_ltz)
        
        ATE_BMI_pd <- rowMeans(bcf_post$`posterior_results BMI`$tau) 
        ATE_BMI_CI <- ci(ATE_BMI_pd, method = "HDI")
        ATE_BMI_CI_plot <-  plot(ATE_BMI_CI, ATE_BMI_pd, show_zero = TRUE)
        BMI_ltz <- length(ATE_BMI_pd[ATE_BMI_pd>0])/length(ATE_BMI_pd)
        evaluation_results[["ATE CIs"]][["BMI"]] <- list("ATE CI BMI" = ATE_BMI_CI , "ATE BMI plot" = ATE_BMI_CI_plot, "LTZ BMI" = BMI_ltz)
        
        ATE_waist_pd <- rowMeans(bcf_post$`posterior_results waist`$tau) 
        ATE_waist_CI <- ci(ATE_waist_pd, method = "HDI")
        ATE_waist_CI_plot <-  plot(ATE_waist_CI, ATE_waist_pd, show_zero = TRUE)
        waist_ltz <- length(ATE_waist_pd[ATE_waist_pd>0])/length(ATE_waist_pd)
        evaluation_results[["ATE CIs"]][["waist"]]  <- list("ATE CI waist" = ATE_waist_CI, "ATE waist plot" = ATE_waist_CI_plot, "LTZ waist" = waist_ltz)
      
      }
      
      if("CATEs" %in% evaluation_methods){
        #Sex####
        CATE_syBP_pd_female <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)]) 
        CATE_syBP_female_CI <- ci(CATE_syBP_pd_female, method = "HDI")
        CATE_syBP_female_CI_plot <-  plot(CATE_syBP_female_CI, CATE_syBP_pd_female, show_zero = TRUE)
        CATE_syBP_female_te <- mean(CATE_syBP_pd_female)
        CATE_syBP_female_ltz <- length(CATE_syBP_pd_female[CATE_syBP_pd_female>0])/length(CATE_syBP_pd_female)
        
        
        CATE_syBP_pd_male <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)]) 
        CATE_syBP_male_CI <- ci(CATE_syBP_pd_male, method = "HDI")
        CATE_syBP_male_CI_plot <-  plot(CATE_syBP_male_CI, CATE_syBP_pd_male, show_zero = TRUE)
        CATE_syBP_male_te <- mean(CATE_syBP_pd_male)
        CATE_syBP_male_ltz <- length(CATE_syBP_pd_male[CATE_syBP_pd_male>0])/length(CATE_syBP_pd_male)
        
        
        evaluation_results[["CATEs"]][["sex"]][["syBP"]]  <- list("CATE syBP female" = CATE_syBP_female_te,
                                                                  "CATE syBP female plot" = ATE_waist_CI_plot,
                                                                  "CATE syBP female ltz" = CATE_syBP_female_ltz,
                                                                  "CATE syBP male" = CATE_syBP_male_te,
                                                                  "CATE syBP male plot" = CATE_syBP_female_CI_plot,
                                                                   "CATE syBP male ltz" = CATE_syBP_male_ltz
                                                                )
        
        CATE_BMI_pd_female <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)]) 
        CATE_BMI_female_CI <- ci(CATE_BMI_pd_female, method = "HDI")
        CATE_BMI_female_CI_plot <-  plot(CATE_BMI_female_CI, CATE_BMI_pd_female, show_zero = TRUE)
        CATE_BMI_female_te <- mean(CATE_BMI_pd_female)
        CATE_BMI_female_ltz <- length(CATE_BMI_pd_female[CATE_BMI_pd_female>0])/length(CATE_BMI_pd_female)
        
        
        CATE_BMI_pd_male <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)]) 
        CATE_BMI_male_CI <- ci(CATE_BMI_pd_male, method = "HDI")
        CATE_BMI_male_CI_plot <-  plot(CATE_BMI_male_CI, CATE_BMI_pd_male, show_zero = TRUE)
        CATE_BMI_male_te <- mean(CATE_BMI_pd_male)
        CATE_BMI_male_ltz <- length(CATE_BMI_pd_male[CATE_BMI_pd_male>0])/length(CATE_BMI_pd_male)
        
        
        evaluation_results[["CATEs"]][["sex"]][["BMI"]]  <- list("CATE BMI female" = CATE_BMI_female_te,
                                                                  "CATE BMI female plot" = ATE_waist_CI_plot,
                                                                 "CATE BMI female ltz" = CATE_BMI_female_ltz,
                                                                  "CATE BMI male" = CATE_BMI_male_te,
                                                                  "CATE BMI male plot" = CATE_BMI_female_CI_plot,
                                                                 "CATE BMI male ltz" = CATE_BMI_male_ltz)
        
        CATE_waist_pd_female <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)]) 
        CATE_waist_female_CI <- ci(CATE_waist_pd_female, method = "HDI")
        CATE_waist_female_CI_plot <-  plot(CATE_waist_female_CI, CATE_waist_pd_female, show_zero = TRUE)
        CATE_waist_female_te <- mean(CATE_waist_pd_female)
        CATE_waist_female_ltz <- length(CATE_waist_pd_female[CATE_waist_pd_female>0])/length(CATE_waist_pd_female)
        
        
        CATE_waist_pd_male <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)]) 
        CATE_waist_male_CI <- ci(CATE_waist_pd_male, method = "HDI")
        CATE_waist_male_CI_plot <-  plot(CATE_waist_male_CI, CATE_waist_pd_male, show_zero = TRUE)
        CATE_waist_male_te <- mean(CATE_waist_pd_male)
        CATE_waist_male_ltz <- length(CATE_waist_pd_male[CATE_waist_pd_male>0])/length(CATE_waist_pd_male)
        
        
        evaluation_results[["CATEs"]][["sex"]][["waist"]]  <- list("CATE waist female" = CATE_waist_female_te,
                                                                  "CATE waist female plot" = ATE_waist_CI_plot,
                                                                  "CATE waist female ltz" = CATE_waist_female_ltz,
                                                                  "CATE waist male" = CATE_waist_male_te,
                                                                  "CATE waist male plot" = CATE_waist_female_CI_plot,
                                                                  "CATE waist male ltz" = CATE_waist_male_ltz)

        
        #Wealth bin ####
        
        for(bin in 1:10){
          CATE_syBP_pd_bin <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)]) 
          CATE_syBP_bin_CI <- ci(CATE_syBP_pd_bin, method = "HDI")
          CATE_syBP_bin_CI_plot <-  plot(CATE_syBP_bin_CI, CATE_syBP_pd_bin, show_zero = TRUE)
          CATE_syBP_bin_te <- mean(CATE_syBP_pd_bin)
          CATE_syBP_bin_ltz <- length(CATE_syBP_pd_bin[CATE_syBP_pd_bin>0])/length(CATE_syBP_pd_bin)
          
          
          listname <- paste("CATE syBP bin ", bin)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", bin)
          
          
          evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname]]  <- CATE_syBP_bin_te
          evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname2]] <- CATE_syBP_bin_CI_plot
          evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname3]] <- CATE_syBP_bin_ltz
          
          
          CATE_BMI_pd_bin <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)]) 
          CATE_BMI_bin_CI <- ci(CATE_BMI_pd_bin, method = "HDI")
          CATE_BMI_bin_CI_plot <-  plot(CATE_BMI_bin_CI, CATE_BMI_pd_bin, show_zero = TRUE)
          CATE_BMI_bin_te <- mean(CATE_BMI_pd_bin)
          CATE_BMI_bin_ltz <- length(CATE_BMI_pd_bin[CATE_BMI_pd_bin>0])/length(CATE_BMI_pd_bin)
          
          
          listname <- paste("CATE BMI bin ", bin)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", bin)
          
          
          evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname]]  <- CATE_BMI_bin_te
          evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname2]] <- CATE_BMI_bin_CI_plot
          evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname3]] <- CATE_BMI_bin_ltz
          
          
          CATE_waist_pd_bin <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)]) 
          CATE_waist_bin_CI <- ci(CATE_waist_pd_bin, method = "HDI")
          CATE_waist_bin_CI_plot <-  plot(CATE_waist_bin_CI, CATE_waist_pd_bin, show_zero = TRUE)
          CATE_waist_bin_te <- mean(CATE_waist_pd_bin)
          CATE_waist_bin_ltz <- length(CATE_waist_pd_bin[CATE_waist_pd_bin>0])/length(CATE_waist_pd_bin)
          
          
          listname <- paste("CATE waist bin ", bin)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", bin)
          
          
          evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname]]  <- CATE_waist_bin_te
          evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname2]] <- CATE_waist_bin_CI_plot
          evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname3]] <- CATE_waist_bin_ltz
          
        }
        
        #Ages####
        for(ages in c(50,60,70,80,90)){
          CATE_syBP_pd_age <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "age"] >= ages & bcf_post$effect_moderators[, "age"] < (ages + 10)) ]) 
          CATE_syBP_age_CI <- ci(CATE_syBP_pd_age, method = "HDI")
          CATE_syBP_age_CI_plot <-  plot(CATE_syBP_age_CI, CATE_syBP_pd_age, show_zero = TRUE)
          CATE_syBP_age_te <- mean(CATE_syBP_pd_age)
          CATE_syBP_age_ltz <- length(CATE_syBP_pd_age[CATE_syBP_pd_age>0])/length(CATE_syBP_pd_age)
          
          
          listname <- paste("CATE syBP age ", ages)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", ages)
          
          evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname]]  <- CATE_syBP_age_te
          evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname2]] <- CATE_syBP_age_CI_plot
          evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname3]] <- CATE_syBP_age_ltz
          
          
          CATE_BMI_pd_age <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "age"] >= ages & bcf_post$effect_moderators[, "age"] < (ages + 10)) ]) 
          CATE_BMI_age_CI <- ci(CATE_BMI_pd_age, method = "HDI")
          CATE_BMI_age_CI_plot <-  plot(CATE_BMI_age_CI, CATE_BMI_pd_age, show_zero = TRUE)
          CATE_BMI_age_te <- mean(CATE_BMI_pd_age)
          CATE_BMI_age_ltz <- length(CATE_BMI_pd_age[CATE_BMI_pd_age>0])/length(CATE_BMI_pd_age)
          
          
          listname <- paste("CATE BMI age ", ages)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", ages)
          
          
          evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname]]  <- CATE_BMI_age_te
          evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname2]] <- CATE_BMI_age_CI_plot
          evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname3]] <- CATE_BMI_age_ltz
          
          CATE_waist_pd_age <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "age"] >= ages & bcf_post$effect_moderators[, "age"] < (ages + 10)) ]) 
          CATE_waist_age_CI <- ci(CATE_waist_pd_age, method = "HDI")
          CATE_waist_age_CI_plot <-  plot(CATE_waist_age_CI, CATE_waist_pd_age, show_zero = TRUE)
          CATE_waist_age_te <- mean(CATE_waist_pd_age)
          CATE_waist_age_ltz <- length(CATE_waist_pd_age[CATE_waist_pd_age>0])/length(CATE_waist_pd_age)
          
          
          listname <- paste("CATE waist age ", ages)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", ages)
          
          
          evaluation_results[["CATEs"]][["age"]][["waist"]][[listname]]  <- CATE_waist_age_te
          evaluation_results[["CATEs"]][["age"]][["waist"]][[listname2]] <- CATE_waist_age_CI_plot
          evaluation_results[["CATEs"]][["age"]][["waist"]][[listname3]] <- CATE_waist_age_ltz
        }
        
        #Race####
        for(race_ind in c("race.1.white.caucasian", "race.2.black.african.american", "race.3.other")){
          CATE_syBP_pd_race <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)]) 
          CATE_syBP_race_CI <- ci(CATE_syBP_pd_race, method = "HDI")
          CATE_syBP_race_CI_plot <-  plot(CATE_syBP_race_CI, CATE_syBP_pd_race, show_zero = TRUE)
          CATE_syBP_race_te <- mean(CATE_syBP_pd_race)
          CATE_syBP_race_ltz <- length(CATE_syBP_pd_race[CATE_syBP_pd_race>0])/length(CATE_syBP_pd_race)
          
          
          listname <- paste("CATE syBP race ", race_ind)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", race_ind)
          
          evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname]]  <- CATE_syBP_race_te
          evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname2]] <- CATE_syBP_race_CI_plot
          evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname3]] <- CATE_syBP_race_ltz
          
          
          CATE_BMI_pd_race <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)]) 
          CATE_BMI_race_CI <- ci(CATE_BMI_pd_race, method = "HDI")
          CATE_BMI_race_CI_plot <-  plot(CATE_BMI_race_CI, CATE_BMI_pd_race, show_zero = TRUE)
          CATE_BMI_race_te <- mean(CATE_BMI_pd_race)
          CATE_BMI_race_ltz <- length(CATE_BMI_pd_race[CATE_BMI_pd_race>0])/length(CATE_BMI_pd_race)
          
          
          listname <- paste("CATE BMI race ", race_ind)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", race_ind)
          
          
          evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname]]  <- CATE_BMI_race_te
          evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname2]] <- CATE_BMI_race_CI_plot
          evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname3]] <- CATE_BMI_race_ltz
          
          
          CATE_waist_pd_race <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)]) 
          CATE_waist_race_CI <- ci(CATE_waist_pd_race, method = "HDI")
          CATE_waist_race_CI_plot <-  plot(CATE_waist_race_CI, CATE_waist_pd_race, show_zero = TRUE)
          CATE_waist_race_te <- mean(CATE_waist_pd_race)
          CATE_waist_race_ltz <- length(CATE_waist_pd_race[CATE_waist_pd_race>0])/length(CATE_waist_pd_race)
          
          
          listname <- paste("CATE waist race ", race_ind)
          listname2 <- paste(listname, " plot")
          listname3 <- paste("ltz", race_ind)
          
          
          evaluation_results[["CATEs"]][["race"]][["waist"]][[listname]]  <- CATE_waist_race_te
          evaluation_results[["CATEs"]][["race"]][["waist"]][[listname2]] <- CATE_waist_race_CI_plot
          evaluation_results[["CATEs"]][["race"]][["waist"]][[listname3]] <- CATE_waist_race_ltz
          
        }
        
        #Propensity score####
        for(ps in c(0.20, 0.40, 0.60)){
          CATE_syBP_pd_ps <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)]) 
          CATE_syBP_ps_CI <- ci(CATE_syBP_pd_ps, method = "HDI")
          CATE_syBP_ps_CI_plot <-  plot(CATE_syBP_ps_CI, CATE_syBP_pd_ps, show_zero = TRUE)
          CATE_syBP_ps_te <- mean(CATE_syBP_pd_ps)
          
          CATE_syBP_pd_ps_up <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)]) 
          CATE_syBP_ps_CI_up <- ci(CATE_syBP_pd_ps_up, method = "HDI")
          CATE_syBP_ps_CI_plot_up <-  plot(CATE_syBP_ps_CI_up, CATE_syBP_pd_ps_up, show_zero = TRUE)
          CATE_syBP_ps_te_up <- mean(CATE_syBP_pd_ps_up)
          
          listname <- paste("CATE syBP ps under", ps)
          listname2 <- paste(listname, " plot")
          listname_up <- paste("CATE syBP ps above", ps)
          listname2_up <- paste(listname_up, " plot")
          
          evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname]]  <- CATE_syBP_ps_te
          evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname2]] <- CATE_syBP_ps_CI_plot
          evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname_up]]  <- CATE_syBP_ps_te_up
          evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname2_up]] <- CATE_syBP_ps_CI_plot_up
          

          CATE_BMI_pd_ps <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)]) 
          CATE_BMI_ps_CI <- ci(CATE_BMI_pd_ps, method = "HDI")
          CATE_BMI_ps_CI_plot <-  plot(CATE_BMI_ps_CI, CATE_BMI_pd_ps, show_zero = TRUE)
          CATE_BMI_ps_te <- mean(CATE_BMI_pd_ps)
          
          CATE_BMI_pd_ps_up <- rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)]) 
          CATE_BMI_ps_CI_up <- ci(CATE_BMI_pd_ps_up, method = "HDI")
          CATE_BMI_ps_CI_plot_up <-  plot(CATE_BMI_ps_CI_up, CATE_BMI_pd_ps_up, show_zero = TRUE)
          CATE_BMI_ps_te_up <- mean(CATE_BMI_pd_ps_up)
          
          listname <- paste("CATE BMI ps under", ps)
          listname2 <- paste(listname, " plot")
          listname_up <- paste("CATE BMI ps above", ps)
          listname2_up <- paste(listname_up, " plot")
          
          evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname]]  <- CATE_BMI_ps_te
          evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname2]] <- CATE_BMI_ps_CI_plot
          evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname_up]]  <- CATE_BMI_ps_te_up
          evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname2_up]] <- CATE_BMI_ps_CI_plot_up
          
          CATE_waist_pd_ps <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)]) 
          CATE_waist_ps_CI <- ci(CATE_waist_pd_ps, method = "HDI")
          CATE_waist_ps_CI_plot <-  plot(CATE_waist_ps_CI, CATE_waist_pd_ps, show_zero = TRUE)
          CATE_waist_ps_te <- mean(CATE_waist_pd_ps)
          
          CATE_waist_pd_ps_up <- rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)]) 
          CATE_waist_ps_CI_up <- ci(CATE_waist_pd_ps_up, method = "HDI")
          CATE_waist_ps_CI_plot_up <-  plot(CATE_waist_ps_CI_up, CATE_waist_pd_ps_up, show_zero = TRUE)
          CATE_waist_ps_te_up <- mean(CATE_waist_pd_ps_up)
          
          listname <- paste("CATE waist ps under", ps)
          listname2 <- paste(listname, " plot")
          listname_up <- paste("CATE waist ps above", ps)
          listname2_up <- paste(listname_up, " plot")
          
          evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname]]  <- CATE_waist_ps_te
          evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname2]] <- CATE_waist_ps_CI_plot
          evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname_up]]  <- CATE_waist_ps_te_up
          evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname2_up]] <- CATE_waist_ps_CI_plot_up
        }
      }
      
      return(evaluation_results)
    }
      
  }else{
    print("No input provided for posterior results")
  }
}