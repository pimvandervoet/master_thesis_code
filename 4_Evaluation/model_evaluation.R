#' evalpost
#'
#' @description evaluation function BCF posterior results
#'
#' @return evaluation_results - list with desired output
#'
#' @param bcf_post ... posterior-result from BCF_function
#' @param evaluation_methods ... desired output, default c("ATE", "Credibility interval", "CATE per moderator")
evalpost <-
  function(bcf_post = NULL,
           evaluation_methods = c("ATE", "Credibility interval", "CATEs", "plots ATE", "plots CATE")) {
    if (!is.null(bcf_post)) {
      #Create empty list to store results
      evaluation_results <- list()
      
      #First obtain ATE for each outcome
      if ("ATE" %in% evaluation_methods) {
        ATE_syBP <- mean(colMeans(bcf_post$`posterior_results syBP`$tau))
        ATE_BMI <-
          mean(colMeans(bcf_post$`posterior_results BMI`$tau))
        ATE_waist <-
          mean(colMeans(bcf_post$`posterior_results waist`$tau))
        evaluation_results[["ATEs"]] <-
          list("ATEs" = cbind(ATE_syBP, ATE_BMI, ATE_waist))
      }
      
      #Obtain Credibility intervals for ATE and Credibility intervals plus numbers for CATES
      if ("Credibility interval" %in% evaluation_methods) {
        library(bayestestR)
        library(see)
        
        if ("ATE" %in% evaluation_methods) {
          #ATE credibility intervals
          ATE_syBP_pd <-
            rowMeans(bcf_post$`posterior_results syBP`$tau)
          ATE_syBP_CI <- ci(ATE_syBP_pd, method = "HDI")
          ATE_syBP_CI_plot <-
            plot(ATE_syBP_CI, ATE_syBP_pd, show_zero = TRUE)
          syBP_ltz <-
            length(ATE_syBP_pd[ATE_syBP_pd > 0]) / length(ATE_syBP_pd)
          evaluation_results[["ATE CIs"]][["syBP"]] <-
            list(
              "ATE CI syBP" = ATE_syBP_CI,
              "ATE CI plot" = ATE_syBP_CI_plot,
              "LTZ syBP" = syBP_ltz
            )
          
          ATE_BMI_pd <-
            rowMeans(bcf_post$`posterior_results BMI`$tau)
          ATE_BMI_CI <- ci(ATE_BMI_pd, method = "HDI")
          ATE_BMI_CI_plot <-
            plot(ATE_BMI_CI, ATE_BMI_pd, show_zero = TRUE)
          BMI_ltz <-
            length(ATE_BMI_pd[ATE_BMI_pd > 0]) / length(ATE_BMI_pd)
          evaluation_results[["ATE CIs"]][["BMI"]] <-
            list(
              "ATE CI BMI" = ATE_BMI_CI ,
              "ATE BMI plot" = ATE_BMI_CI_plot,
              "LTZ BMI" = BMI_ltz
            )
          
          ATE_waist_pd <-
            rowMeans(bcf_post$`posterior_results waist`$tau)
          ATE_waist_CI <- ci(ATE_waist_pd, method = "HDI")
          ATE_waist_CI_plot <-
            plot(ATE_waist_CI, ATE_waist_pd, show_zero = TRUE)
          waist_ltz <-
            length(ATE_waist_pd[ATE_waist_pd > 0]) / length(ATE_waist_pd)
          evaluation_results[["ATE CIs"]][["waist"]]  <-
            list(
              "ATE CI waist" = ATE_waist_CI,
              "ATE waist plot" = ATE_waist_CI_plot,
              "LTZ waist" = waist_ltz
            )
          
        }
        
        if ("CATEs" %in% evaluation_methods) {
          #Sex####
          CATE_syBP_pd_female <-
            rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)])
          CATE_syBP_female_CI <-
            ci(CATE_syBP_pd_female, method = "HDI")
          CATE_syBP_female_CI_plot <-
            plot(CATE_syBP_female_CI, CATE_syBP_pd_female, show_zero = TRUE)
          CATE_syBP_female_te <- mean(CATE_syBP_pd_female)
          CATE_syBP_female_ltz <-
            length(CATE_syBP_pd_female[CATE_syBP_pd_female > 0]) / length(CATE_syBP_pd_female)
          
          
          CATE_syBP_pd_male <-
            rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)])
          CATE_syBP_male_CI <- ci(CATE_syBP_pd_male, method = "HDI")
          CATE_syBP_male_CI_plot <-
            plot(CATE_syBP_male_CI, CATE_syBP_pd_male, show_zero = TRUE)
          CATE_syBP_male_te <- mean(CATE_syBP_pd_male)
          CATE_syBP_male_ltz <-
            length(CATE_syBP_pd_male[CATE_syBP_pd_male > 0]) / length(CATE_syBP_pd_male)
          
          
          evaluation_results[["CATEs"]][["sex"]][["syBP"]]  <-
            list(
              "CATE syBP female" = CATE_syBP_female_te,
              "CATE syBP female plot" = CATE_syBP_female_CI_plot,
              "CATE syBP female ltz" = CATE_syBP_female_ltz,
              "CATE syBP female CI" =  CATE_syBP_female_CI,
              "CATE syBP male" = CATE_syBP_male_te,
              "CATE syBP male plot" = CATE_syBP_male_CI_plot,
              "CATE syBP male ltz" = CATE_syBP_male_ltz,
              "CATE syBP male CI" =  CATE_syBP_male_CI
            )
          
          CATE_BMI_pd_female <-
            rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)])
          CATE_BMI_female_CI <- ci(CATE_BMI_pd_female, method = "HDI")
          CATE_BMI_female_CI_plot <-
            plot(CATE_BMI_female_CI, CATE_BMI_pd_female, show_zero = TRUE)
          CATE_BMI_female_te <- mean(CATE_BMI_pd_female)
          CATE_BMI_female_ltz <-
            length(CATE_BMI_pd_female[CATE_BMI_pd_female > 0]) / length(CATE_BMI_pd_female)
          
          
          CATE_BMI_pd_male <-
            rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)])
          CATE_BMI_male_CI <- ci(CATE_BMI_pd_male, method = "HDI")
          CATE_BMI_male_CI_plot <-
            plot(CATE_BMI_male_CI, CATE_BMI_pd_male, show_zero = TRUE)
          CATE_BMI_male_te <- mean(CATE_BMI_pd_male)
          CATE_BMI_male_ltz <-
            length(CATE_BMI_pd_male[CATE_BMI_pd_male > 0]) / length(CATE_BMI_pd_male)
          
          
          evaluation_results[["CATEs"]][["sex"]][["BMI"]]  <-
            list(
              "CATE BMI female" = CATE_BMI_female_te,
              "CATE BMI female plot" = CATE_BMI_female_CI_plot,
              "CATE BMI female ltz" = CATE_BMI_female_ltz,
              "CATE BMI female CI" =  CATE_BMI_female_CI,
              "CATE BMI male" = CATE_BMI_male_te,
              "CATE BMI male plot" = CATE_BMI_male_CI_plot,
              "CATE BMI male ltz" = CATE_BMI_male_ltz,
              "CATE BMI male CI" =  CATE_BMI_male_CI
            )
          
          
          CATE_waist_pd_female <-
            rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)])
          CATE_waist_female_CI <-
            ci(CATE_waist_pd_female, method = "HDI")
          CATE_waist_female_CI_plot <-
            plot(CATE_waist_female_CI,
                 CATE_waist_pd_female,
                 show_zero = TRUE)
          CATE_waist_female_te <- mean(CATE_waist_pd_female)
          CATE_waist_female_ltz <-
            length(CATE_waist_pd_female[CATE_waist_pd_female > 0]) / length(CATE_waist_pd_female)
          
          
          CATE_waist_pd_male <-
            rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)])
          CATE_waist_male_CI <- ci(CATE_waist_pd_male, method = "HDI")
          CATE_waist_male_CI_plot <-
            plot(CATE_waist_male_CI, CATE_waist_pd_male, show_zero = TRUE)
          CATE_waist_male_te <- mean(CATE_waist_pd_male)
          CATE_waist_male_ltz <-
            length(CATE_waist_pd_male[CATE_waist_pd_male > 0]) / length(CATE_waist_pd_male)
          
          
          evaluation_results[["CATEs"]][["sex"]][["waist"]]  <-
            list(
              "CATE waist female" = CATE_waist_female_te,
              "CATE waist female plot" = CATE_waist_female_CI_plot,
              "CATE waist female ltz" = CATE_waist_female_ltz,
              "CATE waist female CI" =  CATE_waist_female_CI,
              "CATE waist male" = CATE_waist_male_te,
              "CATE waist male plot" = CATE_waist_male_CI_plot,
              "CATE waist male ltz" = CATE_waist_male_ltz,
              "CATE waist male CI" =  CATE_waist_male_CI
            )
          
          
          #Wealth bin ####
          
          for (bin in 1:10) {
            CATE_syBP_pd_bin <-
              rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)])
            CATE_syBP_bin_CI <- ci(CATE_syBP_pd_bin, method = "HDI")
            CATE_syBP_bin_CI_plot <-
              plot(CATE_syBP_bin_CI, CATE_syBP_pd_bin, show_zero = TRUE)
            CATE_syBP_bin_te <- mean(CATE_syBP_pd_bin)
            CATE_syBP_bin_ltz <-
              length(CATE_syBP_pd_bin[CATE_syBP_pd_bin > 0]) / length(CATE_syBP_pd_bin)
            
            
            listname <- paste("CATE syBP bin ", bin)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", bin)
            listname4 <- paste("CI", bin)
            
            evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname]]  <-
              CATE_syBP_bin_te
            evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname2]] <-
              CATE_syBP_bin_CI_plot
            evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname3]] <-
              CATE_syBP_bin_ltz
            evaluation_results[["CATEs"]][["wealth"]][["syBP"]][[listname4]] <-
              CATE_syBP_bin_CI
            
            
            CATE_BMI_pd_bin <-
              rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)])
            CATE_BMI_bin_CI <- ci(CATE_BMI_pd_bin, method = "HDI")
            CATE_BMI_bin_CI_plot <-
              plot(CATE_BMI_bin_CI, CATE_BMI_pd_bin, show_zero = TRUE)
            CATE_BMI_bin_te <- mean(CATE_BMI_pd_bin)
            CATE_BMI_bin_ltz <-
              length(CATE_BMI_pd_bin[CATE_BMI_pd_bin > 0]) / length(CATE_BMI_pd_bin)
            
            
            listname <- paste("CATE BMI bin ", bin)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", bin)
            listname4 <- paste("CI", bin)
            
            evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname]]  <-
              CATE_BMI_bin_te
            evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname2]] <-
              CATE_BMI_bin_CI_plot
            evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname3]] <-
              CATE_BMI_bin_ltz
            evaluation_results[["CATEs"]][["wealth"]][["BMI"]][[listname4]] <-
              CATE_BMI_bin_CI
            
            
            CATE_waist_pd_bin <-
              rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "wealth_bin"] == bin)])
            CATE_waist_bin_CI <- ci(CATE_waist_pd_bin, method = "HDI")
            CATE_waist_bin_CI_plot <-
              plot(CATE_waist_bin_CI, CATE_waist_pd_bin, show_zero = TRUE)
            CATE_waist_bin_te <- mean(CATE_waist_pd_bin)
            CATE_waist_bin_ltz <-
              length(CATE_waist_pd_bin[CATE_waist_pd_bin > 0]) / length(CATE_waist_pd_bin)
            
            
            listname <- paste("CATE waist bin ", bin)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", bin)
            listname4 <- paste("CI", bin)
            
            evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname]]  <-
              CATE_waist_bin_te
            evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname2]] <-
              CATE_waist_bin_CI_plot
            evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname3]] <-
              CATE_waist_bin_ltz
            evaluation_results[["CATEs"]][["wealth"]][["waist"]][[listname4]] <-
              CATE_waist_bin_CI
            
          }
          
          #Ages####
          for (ages in c(50, 60, 70, 80, 90)) {
            CATE_syBP_pd_age <-
              rowMeans(bcf_post$`posterior_results syBP`$tau[, which(
                bcf_post$effect_moderators[, "age"] >= ages &
                  bcf_post$effect_moderators[, "age"] < (ages + 10)
              )])
            CATE_syBP_age_CI <- ci(CATE_syBP_pd_age, method = "HDI")
            CATE_syBP_age_CI_plot <-
              plot(CATE_syBP_age_CI, CATE_syBP_pd_age, show_zero = TRUE)
            CATE_syBP_age_te <- mean(CATE_syBP_pd_age)
            CATE_syBP_age_ltz <-
              length(CATE_syBP_pd_age[CATE_syBP_pd_age > 0]) / length(CATE_syBP_pd_age)
            
            
            listname <- paste("CATE syBP age ", ages)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", ages)
            listname4 <- paste("CI", ages)
            
            evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname]]  <-
              CATE_syBP_age_te
            evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname2]] <-
              CATE_syBP_age_CI_plot
            evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname3]] <-
              CATE_syBP_age_ltz
            evaluation_results[["CATEs"]][["age"]][["syBP"]][[listname4]] <-
              CATE_syBP_age_CI
            
            
            
            CATE_BMI_pd_age <-
              rowMeans(bcf_post$`posterior_results BMI`$tau[, which(
                bcf_post$effect_moderators[, "age"] >= ages &
                  bcf_post$effect_moderators[, "age"] < (ages + 10)
              )])
            CATE_BMI_age_CI <- ci(CATE_BMI_pd_age, method = "HDI")
            CATE_BMI_age_CI_plot <-
              plot(CATE_BMI_age_CI, CATE_BMI_pd_age, show_zero = TRUE)
            CATE_BMI_age_te <- mean(CATE_BMI_pd_age)
            CATE_BMI_age_ltz <-
              length(CATE_BMI_pd_age[CATE_BMI_pd_age > 0]) / length(CATE_BMI_pd_age)
            
            
            listname <- paste("CATE BMI age ", ages)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", ages)
            listname4 <- paste("CI", ages)
            
            
            
            evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname]]  <-
              CATE_BMI_age_te
            evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname2]] <-
              CATE_BMI_age_CI_plot
            evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname3]] <-
              CATE_BMI_age_ltz
            evaluation_results[["CATEs"]][["age"]][["BMI"]][[listname4]] <-
              CATE_BMI_age_CI
            
            
            CATE_waist_pd_age <-
              rowMeans(bcf_post$`posterior_results waist`$tau[, which(
                bcf_post$effect_moderators[, "age"] >= ages &
                  bcf_post$effect_moderators[, "age"] < (ages + 10)
              )])
            CATE_waist_age_CI <- ci(CATE_waist_pd_age, method = "HDI")
            CATE_waist_age_CI_plot <-
              plot(CATE_waist_age_CI, CATE_waist_pd_age, show_zero = TRUE)
            CATE_waist_age_te <- mean(CATE_waist_pd_age)
            CATE_waist_age_ltz <-
              length(CATE_waist_pd_age[CATE_waist_pd_age > 0]) / length(CATE_waist_pd_age)
            
            
            listname <- paste("CATE waist age ", ages)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", ages)
            listname4 <- paste("CI", ages)
            
            
            
            evaluation_results[["CATEs"]][["age"]][["waist"]][[listname]]  <-
              CATE_waist_age_te
            evaluation_results[["CATEs"]][["age"]][["waist"]][[listname2]] <-
              CATE_waist_age_CI_plot
            evaluation_results[["CATEs"]][["age"]][["waist"]][[listname3]] <-
              CATE_waist_age_ltz
            evaluation_results[["CATEs"]][["age"]][["waist"]][[listname4]] <-
              CATE_waist_age_CI
            
          }
          
          #Race####
          for (race_ind in c(
            "race.1.white.caucasian",
            "race.2.black.african.american",
            "race.3.other"
          )) {
            CATE_syBP_pd_race <-
              rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)])
            CATE_syBP_race_CI <- ci(CATE_syBP_pd_race, method = "HDI")
            CATE_syBP_race_CI_plot <-
              plot(CATE_syBP_race_CI, CATE_syBP_pd_race, show_zero = TRUE)
            CATE_syBP_race_te <- mean(CATE_syBP_pd_race)
            CATE_syBP_race_ltz <-
              length(CATE_syBP_pd_race[CATE_syBP_pd_race > 0]) / length(CATE_syBP_pd_race)
            
            
            listname <- paste("CATE syBP race ", race_ind)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", race_ind)
            listname4 <- paste("CI", race_ind)
            
            
            evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname]]  <-
              CATE_syBP_race_te
            evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname2]] <-
              CATE_syBP_race_CI_plot
            evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname3]] <-
              CATE_syBP_race_ltz
            evaluation_results[["CATEs"]][["race"]][["syBP"]][[listname4]] <-
              CATE_syBP_race_CI
            
            
            
            CATE_BMI_pd_race <-
              rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)])
            CATE_BMI_race_CI <- ci(CATE_BMI_pd_race, method = "HDI")
            CATE_BMI_race_CI_plot <-
              plot(CATE_BMI_race_CI, CATE_BMI_pd_race, show_zero = TRUE)
            CATE_BMI_race_te <- mean(CATE_BMI_pd_race)
            CATE_BMI_race_ltz <-
              length(CATE_BMI_pd_race[CATE_BMI_pd_race > 0]) / length(CATE_BMI_pd_race)
            
            
            listname <- paste("CATE BMI race ", race_ind)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", race_ind)
            listname4 <- paste("CI", race_ind)
            
            
            evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname]]  <-
              CATE_BMI_race_te
            evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname2]] <-
              CATE_BMI_race_CI_plot
            evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname3]] <-
              CATE_BMI_race_ltz
            evaluation_results[["CATEs"]][["race"]][["BMI"]][[listname4]] <-
              CATE_BMI_race_CI
            
            
            
            CATE_waist_pd_race <-
              rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, race_ind] == 1)])
            CATE_waist_race_CI <-
              ci(CATE_waist_pd_race, method = "HDI")
            CATE_waist_race_CI_plot <-
              plot(CATE_waist_race_CI, CATE_waist_pd_race, show_zero = TRUE)
            CATE_waist_race_te <- mean(CATE_waist_pd_race)
            CATE_waist_race_ltz <-
              length(CATE_waist_pd_race[CATE_waist_pd_race > 0]) / length(CATE_waist_pd_race)
            
            
            listname <- paste("CATE waist race ", race_ind)
            listname2 <- paste(listname, " plot")
            listname3 <- paste("ltz", race_ind)
            listname4 <- paste("CI", race_ind)
            
            
            evaluation_results[["CATEs"]][["race"]][["waist"]][[listname]]  <-
              CATE_waist_race_te
            evaluation_results[["CATEs"]][["race"]][["waist"]][[listname2]] <-
              CATE_waist_race_CI_plot
            evaluation_results[["CATEs"]][["race"]][["waist"]][[listname3]] <-
              CATE_waist_race_ltz
            evaluation_results[["CATEs"]][["race"]][["waist"]][[listname4]] <-
              CATE_waist_race_CI
            
            
          }
          
          #Propensity score####
          for (ps in c(0.20, 0.40, 0.60)) {
            CATE_syBP_pd_ps <-
              rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)])
            CATE_syBP_ps_CI <- ci(CATE_syBP_pd_ps, method = "HDI")
            CATE_syBP_ps_CI_plot <-
              plot(CATE_syBP_ps_CI, CATE_syBP_pd_ps, show_zero = TRUE)
            CATE_syBP_ps_te <- mean(CATE_syBP_pd_ps)
            
            CATE_syBP_pd_ps_up <-
              rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)])
            CATE_syBP_ps_CI_up <-
              ci(CATE_syBP_pd_ps_up, method = "HDI")
            CATE_syBP_ps_CI_plot_up <-
              plot(CATE_syBP_ps_CI_up, CATE_syBP_pd_ps_up, show_zero = TRUE)
            CATE_syBP_ps_te_up <- mean(CATE_syBP_pd_ps_up)
            
            listname <- paste("CATE syBP ps under", ps)
            listname2 <- paste(listname, " plot")
            listname_up <- paste("CATE syBP ps above", ps)
            listname2_up <- paste(listname_up, " plot")
            
            evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname]]  <-
              CATE_syBP_ps_te
            evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname2]] <-
              CATE_syBP_ps_CI_plot
            evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname_up]]  <-
              CATE_syBP_ps_te_up
            evaluation_results[["CATEs"]][["ps"]][["syBP"]][[listname2_up]] <-
              CATE_syBP_ps_CI_plot_up
            
            
            CATE_BMI_pd_ps <-
              rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)])
            CATE_BMI_ps_CI <- ci(CATE_BMI_pd_ps, method = "HDI")
            CATE_BMI_ps_CI_plot <-
              plot(CATE_BMI_ps_CI, CATE_BMI_pd_ps, show_zero = TRUE)
            CATE_BMI_ps_te <- mean(CATE_BMI_pd_ps)
            
            CATE_BMI_pd_ps_up <-
              rowMeans(bcf_post$`posterior_results BMI`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)])
            CATE_BMI_ps_CI_up <- ci(CATE_BMI_pd_ps_up, method = "HDI")
            CATE_BMI_ps_CI_plot_up <-
              plot(CATE_BMI_ps_CI_up, CATE_BMI_pd_ps_up, show_zero = TRUE)
            CATE_BMI_ps_te_up <- mean(CATE_BMI_pd_ps_up)
            
            listname <- paste("CATE BMI ps under", ps)
            listname2 <- paste(listname, " plot")
            listname_up <- paste("CATE BMI ps above", ps)
            listname2_up <- paste(listname_up, " plot")
            
            evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname]]  <-
              CATE_BMI_ps_te
            evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname2]] <-
              CATE_BMI_ps_CI_plot
            evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname_up]]  <-
              CATE_BMI_ps_te_up
            evaluation_results[["CATEs"]][["ps"]][["BMI"]][[listname2_up]] <-
              CATE_BMI_ps_CI_plot_up
            
            CATE_waist_pd_ps <-
              rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] < ps)])
            CATE_waist_ps_CI <- ci(CATE_waist_pd_ps, method = "HDI")
            CATE_waist_ps_CI_plot <-
              plot(CATE_waist_ps_CI, CATE_waist_pd_ps, show_zero = TRUE)
            CATE_waist_ps_te <- mean(CATE_waist_pd_ps)
            
            CATE_waist_pd_ps_up <-
              rowMeans(bcf_post$`posterior_results waist`$tau[, which(bcf_post$effect_moderators[, "ps_estimates"] >= ps)])
            CATE_waist_ps_CI_up <-
              ci(CATE_waist_pd_ps_up, method = "HDI")
            CATE_waist_ps_CI_plot_up <-
              plot(CATE_waist_ps_CI_up,
                   CATE_waist_pd_ps_up,
                   show_zero = TRUE)
            CATE_waist_ps_te_up <- mean(CATE_waist_pd_ps_up)
            
            listname <- paste("CATE waist ps under", ps)
            listname2 <- paste(listname, " plot")
            listname_up <- paste("CATE waist ps above", ps)
            listname2_up <- paste(listname_up, " plot")
            
            evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname]]  <-
              CATE_waist_ps_te
            evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname2]] <-
              CATE_waist_ps_CI_plot
            evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname_up]]  <-
              CATE_waist_ps_te_up
            evaluation_results[["CATEs"]][["ps"]][["waist"]][[listname2_up]] <-
              CATE_waist_ps_CI_plot_up
          }
        }
        
        return(evaluation_results)
      }
      
    } else{
      print("No input provided for posterior results")
    }
  }


#' evalpost_weighted
#'
#' @description evaluation function BCF posterior results for weighted analysis
#'
#' @return evaluation_results - list with desired output
#'
#' @param posterior_draws ... posterior draws - results from outcome of interest with dimension: sample size * total posterior draws
#' @param individuals ... matrix with indices of individuals dimension: sample size * loopsize
#' @param moderators ... matrix with moderators
#' @param ps_estimates ... matrix with ps estimates
evalpost_weighted <-
  function(posterior_draws = NULL,
           individuals = NULL,
           moderators = NULL,
           ps_estimates = NULL) {
    if (!is.null(posterior_draws)) {
      if (!is.null(individuals)) {
        if (!is.null(moderators)) {
          if (!is.null(ps_estimates)) {
            evaluation_results <- list()
            
            library(bayestestR)
            library(see)
            
            #First obtain ATE for the outcome
            ATE_PE <- mean(colMeans(posterior_draws))
            evaluation_results[["ATEs"]] <- list("ATE" = ATE_PE)
            
            #ATE credibility intervals
            ATE_pd <-
              colMeans(posterior_draws)
            ATE_CI <- ci(ATE_pd, method = "HDI")
            ATE_CI_plot <-
              plot(ATE_CI, ATE_pd, show_zero = TRUE)
            pltz <-
              length(ATE_pd[ATE_pd > 0]) / length(ATE_pd)
            evaluation_results[["ATE CIs"]] <-
              list("ATE CI" = ATE_CI,
                   "ATE CI plot" = ATE_CI_plot,
                   "LTZ" = pltz)
            
            #Go through all conditions, and loop through the loopsize
            
            #Basic variables for loop
            drawsPL <- dim(posterior_draws)[2] / dim(individuals)[2]
            loopsize <- dim(individuals)[2]
            
            #Obtain indices of each subgroup of interest in the original sample
            female_indices_mod <- which(moderators[, "sex"] == 0)
            male_indices_mod <- which(moderators[, "sex"] == 1)
            
            white_indices_mod <-
              which(moderators[, "race"] == "1.white/caucasian")
            black_indices_mod <-
              which(moderators[, "race"] == "2.black/african american")
            other_indices_mod <-
              which(moderators[, "race"] == "3.other")
            
            indices_mod_50 <-
              which(moderators[, "age"] >= 50 & moderators[, "age"] < 60)
            indices_mod_60 <-
              which(moderators[, "age"] >= 60 & moderators[, "age"] < 70)
            indices_mod_70 <-
              which(moderators[, "age"] >= 70 & moderators[, "age"] < 80)
            indices_mod_80 <-
              which(moderators[, "age"] >= 80 & moderators[, "age"] < 90)
            indices_mod_90 <-
              which(moderators[, "age"] >= 90 & moderators[, "age"] < 100)
            
            w1_indices_mod <-  which(moderators[, "wealth_bin"] == 1)
            w2_indices_mod <-  which(moderators[, "wealth_bin"] == 2)
            w3_indices_mod <-  which(moderators[, "wealth_bin"] == 3)
            w4_indices_mod <-  which(moderators[, "wealth_bin"] == 4)
            w5_indices_mod <-  which(moderators[, "wealth_bin"] == 5)
            w6_indices_mod <-  which(moderators[, "wealth_bin"] == 6)
            w7_indices_mod <-  which(moderators[, "wealth_bin"] == 7)
            w8_indices_mod <-  which(moderators[, "wealth_bin"] == 8)
            w9_indices_mod <-  which(moderators[, "wealth_bin"] == 9)
            w10_indices_mod <-
              which(moderators[, "wealth_bin"] == 10)
            
            pd_low_indices_mod <- which(ps_estimates < .20)
            pd_med_indices_mod <-
              which(ps_estimates >= .20 & ps_estimates < 0.40)
            pd_high_indices_mod <- which(ps_estimates >= 0.40)
            
            ##SEX and RACE #####
            
            #Storage vectors
            CATE_pd_female <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_male <-   rep(NA, dim(posterior_draws)[2])
            
            CATE_pd_white <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_black <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_other <- rep(NA, dim(posterior_draws)[2])
            
            CATE_pd_50 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_60 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_70 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_80 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_90 <- rep(NA, dim(posterior_draws)[2])
            
            CATE_pd_w1 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w2 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w3 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w4 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w5 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w6 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w7 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w8 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w9 <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_w10 <- rep(NA, dim(posterior_draws)[2])
            
            CATE_pd_lowps <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_medps <- rep(NA, dim(posterior_draws)[2])
            CATE_pd_highps <- rep(NA, dim(posterior_draws)[2])
            
            #Get CATEs
            for (PRS in 1:loopsize) {
              #Set basic variables in loop
              fill_start <- (drawsPL * (PRS - 1)) + 1
              fill_end <- drawsPL * PRS
              to_fill <- fill_start:fill_end
              
              
              #Obtain indices of the subgroups of interest from the PRS
              females_PRS <-
                which(individuals[, PRS] %in% female_indices_mod)
              males_PRS <-
                which(individuals[, PRS] %in% male_indices_mod)
              
              whites_PRS  <-
                which(individuals[, PRS] %in% white_indices_mod)
              black_PRS <-
                which(individuals[, PRS] %in% black_indices_mod)
              other_PRS <-
                which(individuals[, PRS] %in% other_indices_mod)
              
              age50_PRS <-
                which(individuals[, PRS] %in% indices_mod_50)
              age60_PRS <-
                which(individuals[, PRS] %in% indices_mod_60)
              age70_PRS <-
                which(individuals[, PRS] %in% indices_mod_70)
              age80_PRS <-
                which(individuals[, PRS] %in% indices_mod_80)
              age90_PRS <-
                which(individuals[, PRS] %in% indices_mod_90)
              
              w1_PRS <- which(individuals[, PRS] %in% w1_indices_mod)
              w2_PRS <- which(individuals[, PRS] %in% w2_indices_mod)
              w3_PRS <- which(individuals[, PRS] %in% w3_indices_mod)
              w4_PRS <- which(individuals[, PRS] %in% w4_indices_mod)
              w5_PRS <- which(individuals[, PRS] %in% w5_indices_mod)
              w6_PRS <- which(individuals[, PRS] %in% w6_indices_mod)
              w7_PRS <- which(individuals[, PRS] %in% w7_indices_mod)
              w8_PRS <- which(individuals[, PRS] %in% w8_indices_mod)
              w9_PRS <- which(individuals[, PRS] %in% w9_indices_mod)
              w10_PRS <-
                which(individuals[, PRS] %in% w10_indices_mod)
              
              lowps_PRS <-
                which(individuals[, PRS] %in% pd_low_indices_mod)
              medps_PRS <-
                which(individuals[, PRS] %in% pd_med_indices_mod)
              highps_PRS <-
                which(individuals[, PRS] %in% pd_high_indices_mod)
              
              
              #Fill posterior density vectors by estimating CATEs for each draw
              CATE_pd_female[to_fill] <-
                colMeans(posterior_draws[females_PRS , to_fill])
              CATE_pd_male[to_fill] <-
                colMeans(posterior_draws[males_PRS , to_fill])
              
              CATE_pd_white[to_fill] <-
                colMeans(posterior_draws[whites_PRS , to_fill])
              CATE_pd_black[to_fill] <-
                colMeans(posterior_draws[black_PRS , to_fill])
              CATE_pd_other[to_fill] <-
                colMeans(posterior_draws[other_PRS , to_fill])
              
              CATE_pd_50[to_fill] <-
                colMeans(posterior_draws[age50_PRS , to_fill])
              CATE_pd_60[to_fill] <-
                colMeans(posterior_draws[age60_PRS , to_fill])
              CATE_pd_70[to_fill] <-
                colMeans(posterior_draws[age70_PRS , to_fill])
              CATE_pd_80[to_fill] <-
                colMeans(posterior_draws[age80_PRS , to_fill])
              CATE_pd_90[to_fill] <-
                colMeans(posterior_draws[age90_PRS , to_fill])
              
              CATE_pd_w1[to_fill] <-
                colMeans(posterior_draws[w1_PRS , to_fill])
              CATE_pd_w2[to_fill] <-
                colMeans(posterior_draws[w2_PRS , to_fill])
              CATE_pd_w3[to_fill] <-
                colMeans(posterior_draws[w3_PRS , to_fill])
              CATE_pd_w4[to_fill] <-
                colMeans(posterior_draws[w4_PRS , to_fill])
              CATE_pd_w5[to_fill] <-
                colMeans(posterior_draws[w5_PRS , to_fill])
              CATE_pd_w6[to_fill] <-
                colMeans(posterior_draws[w6_PRS , to_fill])
              CATE_pd_w7[to_fill] <-
                colMeans(posterior_draws[w7_PRS , to_fill])
              CATE_pd_w8[to_fill] <-
                colMeans(posterior_draws[w8_PRS , to_fill])
              CATE_pd_w9[to_fill] <-
                colMeans(posterior_draws[w9_PRS , to_fill])
              CATE_pd_w10[to_fill] <-
                colMeans(posterior_draws[w10_PRS , to_fill])
              
              CATE_pd_lowps[to_fill] <-
                colMeans(posterior_draws[lowps_PRS , to_fill])
              CATE_pd_medps[to_fill] <-
                colMeans(posterior_draws[medps_PRS , to_fill])
              CATE_pd_highps[to_fill] <-
                colMeans(posterior_draws[highps_PRS , to_fill])
            }
            
            #Get CATE posterior characteristics
            CATE_female_CI <- ci(CATE_pd_female, method = "HDI")
            CATE_female_CI_plot <-
              plot(CATE_female_CI, CATE_pd_female, show_zero = TRUE)
            CATE_female_te <- mean(CATE_pd_female)
            CATE_female_ltz <-
              length(CATE_pd_female[CATE_pd_female > 0]) / length(CATE_pd_female)
            
            CATE_male_CI <- ci(CATE_pd_male, method = "HDI")
            CATE_male_CI_plot <-
              plot(CATE_male_CI, CATE_pd_male, show_zero = TRUE)
            CATE_male_te <- mean(CATE_pd_male)
            CATE_male_ltz <-
              length(CATE_pd_male[CATE_pd_male > 0]) / length(CATE_pd_male)
            
            
            evaluation_results[["CATEs"]][["sex"]]  <-
              list(
                "CATE  female" = CATE_female_te,
                "CATE female plot" = CATE_female_CI_plot,
                "CATE female ltz" = CATE_female_ltz,
                "CATE female CI" =  CATE_female_CI,
                "CATE male" = CATE_male_te,
                "CATE male plot" = CATE_male_CI_plot,
                "CATE male ltz" = CATE_male_ltz,
                "CATE male CI" =  CATE_male_CI
              )
            ###RACE ####
            for (race_ind in c(
              "race.1.white.caucasian",
              "race.2.black.african.american",
              "race.3.other"
            )) {
              if (race_ind == "race.1.white.caucasian") {
                CATE_pd_race <- CATE_pd_white
              } else if (race_ind == "race.2.black.african.american") {
                CATE_pd_race <- CATE_pd_black
                
              } else if (race_ind == "race.3.other") {
                CATE_pd_race <- CATE_pd_other
                
              }
              
              CATE_race_CI <- ci(CATE_pd_race, method = "HDI")
              CATE_race_CI_plot <-
                plot(CATE_race_CI, CATE_pd_race, show_zero = TRUE)
              CATE_race_te <- mean(CATE_pd_race)
              CATE_race_ltz <-
                length(CATE_pd_race[CATE_pd_race > 0]) / length(CATE_pd_race)
              
              
              listname <- paste("CATE race ", race_ind)
              listname2 <- paste(listname, " plot")
              listname3 <- paste("ltz", race_ind)
              listname4 <- paste("CI", race_ind)
              
              
              evaluation_results[["CATEs"]][["race"]][[listname]]  <-
                CATE_race_te
              evaluation_results[["CATEs"]][["race"]][[listname2]] <-
                CATE_race_CI_plot
              evaluation_results[["CATEs"]][["race"]][[listname3]] <-
                CATE_race_ltz
              evaluation_results[["CATEs"]][["race"]][[listname4]] <-
                CATE_race_CI
              
            }
            
            ####Age####
            for (age in c(50, 60, 70, 80, 90)) {
              if (age == 50) {
                CATE_pd_age <- CATE_pd_50
              } else if (age == 60) {
                CATE_pd_age <- CATE_pd_60
                
              } else if (age == 70) {
                CATE_pd_age <- CATE_pd_70
                
              } else if (age == 80) {
                CATE_pd_age <- CATE_pd_80
              } else if (age == 90) {
                CATE_pd_age <- CATE_pd_90
              }
              
              CATE_age_CI <- ci(CATE_pd_age, method = "HDI")
              CATE_age_CI_plot <-
                plot(CATE_age_CI, CATE_pd_age, show_zero = TRUE)
              CATE_age_te <- mean(CATE_pd_age)
              CATE_age_ltz <-
                length(CATE_pd_age[CATE_pd_age > 0]) / length(CATE_pd_age)
              
              listname <- paste("CATE age ", age)
              listname2 <- paste(listname, " plot")
              listname3 <- paste("ltz", age)
              listname4 <- paste("CI", age)
              
              evaluation_results[["CATEs"]][["age"]][[listname]]  <-
                CATE_age_te
              evaluation_results[["CATEs"]][["age"]][[listname2]] <-
                CATE_age_CI_plot
              evaluation_results[["CATEs"]][["age"]][[listname3]] <-
                CATE_age_ltz
              evaluation_results[["CATEs"]][["age"]][[listname4]] <-
                CATE_age_CI
              
            }
            
            
            
            ####Wealth####
            for (bin in 1:10) {
              CATE_pd_bin <-
                list(
                  CATE_pd_w1,
                  CATE_pd_w2,
                  CATE_pd_w3,
                  CATE_pd_w4,
                  CATE_pd_w5,
                  CATE_pd_w6,
                  CATE_pd_w7,
                  CATE_pd_w8,
                  CATE_pd_w9,
                  CATE_pd_w10
                )[[bin]]
              
              CATE_bin_CI <- ci(CATE_pd_bin, method = "HDI")
              CATE_bin_CI_plot <-
                plot(CATE_bin_CI, CATE_pd_bin, show_zero = TRUE)
              CATE_bin_te <- mean(CATE_pd_bin)
              CATE_bin_ltz <-
                length(CATE_pd_bin[CATE_pd_bin > 0]) / length(CATE_pd_bin)
              
              
              listname <- paste("CATE bin ", bin)
              listname2 <- paste(listname, " plot")
              listname3 <- paste("ltz", bin)
              listname4 <- paste("CI", bin)
              
              
              evaluation_results[["CATEs"]][["wealth"]][[listname]]  <-
                CATE_bin_te
              evaluation_results[["CATEs"]][["wealth"]][[listname2]] <-
                CATE_bin_CI_plot
              evaluation_results[["CATEs"]][["wealth"]][[listname3]] <-
                CATE_bin_ltz
              evaluation_results[["CATEs"]][["wealth"]][[listname4]] <-
                CATE_bin_CI
              
              
            }
            
            ####PS_estimates####
            for (ps_est in c("low", "med", "high")) {
              if (ps_est == "low") {
                CATE_pd_ps <- CATE_pd_lowps
              } else if (ps_est == "med") {
                CATE_pd_ps <- CATE_pd_medps
                
              } else if (ps_est == "high") {
                CATE_pd_ps <- CATE_pd_highps
                
              }
              CATE_ps_CI <- ci(CATE_pd_ps, method = "HDI")
              CATE_ps_CI_plot <-
                plot(CATE_ps_CI, CATE_pd_ps, show_zero = TRUE)
              CATE_ps_te <- mean(CATE_pd_ps)
              CATE_ps_ltz <-
                length(CATE_pd_ps[CATE_pd_ps > 0]) / length(CATE_pd_ps)
              
              
              listname <- paste("CATE ps ", ps_est)
              listname2 <- paste(listname, " plot")
              listname3 <- paste("ltz", ps_est)
              listname4 <- paste("CI", ps_est)
              
              
              evaluation_results[["CATEs"]][["ps_est"]][[listname]]  <-
                CATE_ps_te
              evaluation_results[["CATEs"]][["ps_est"]][[listname2]] <-
                CATE_ps_CI_plot
              evaluation_results[["CATEs"]][["ps_est"]][[listname3]] <-
                CATE_ps_ltz
              evaluation_results[["CATEs"]][["ps_est"]][[listname4]] <-
                CATE_ps_CI
              
            }
            
            #Return output
            return(evaluation_results)
          } else{
            print("Please provide propensity score estimates")
          }
        } else{
          print("Please provide matrix with moderator values for all individuals")
        }
        
      } else{
        print("Please provide matrix with indices of individuals")
      }
    } else{
      print("Please provide posterior draws")
    }
    
  }
