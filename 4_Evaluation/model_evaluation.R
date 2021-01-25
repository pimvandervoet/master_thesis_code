#' evalpost
#' 
#' @description evaluation function BCF posterior results
#' 
#' @return evaluation_results - list with desired output 
#' 
#' @param bcf_post ... posterior-result from BCF_function
#' @param evaluation_methods ... desired output, default c("ATE", "Credibility interval", "CATE per moderator")
evalpost <- function(bcf_post, evaluation_methods = c("ATE", "Credibility interval", "CATEs", "plots ATE", "plots CATE")){
  if(is.na(bcf_post)){
    
    #Create empty list to store results
    evaluation_results <- list()
    
    #First obtain ATE for each outcome
    if("ATE" %in% evaluation_methods){
      ATE_syBP <- mean(colMeans(bcf_post$`posterior_results syBP`$tau))
      ATE_BMI <- mean(colMeans(bcf_post$`posterior_results BMI`$tau))
      ATE_waist <- mean(colMeans(bcf_post$`posterior_results waist`$tau))
      evaluation_results[["ATEs"]] <- list("ATEs" = cbind(ATE_syBP, ATE_BMI, ATE_waist))
    }
    
    if("Credibility interval" %in% evaluation_methods){
      library(bayestestR)
      library(see)

      if("ATE" %in% evaluation_methods){
      #ATE credibility intervals
        ATE_syBP_pd <- rowMeans(bcf_post$`posterior_results syBP`$tau) 
        ATE_syBP_CI <- ci(ATE_syBP_pd, method = "HDI")
        ATE_syBP_CI_plot <-  plot(ATE_syBP_CI, ATE_syBP_pd, show_zero = TRUE)
        evaluation_results[["ATE CIs"]][["syBP"]] <- list("ATE CI syBP" = ATE_syBP_CI, "ATE CI plot" = ATE_syBP_CI_plot)
        
        ATE_BMI_pd <- rowMeans(bcf_post$`posterior_results BMI`$tau) 
        ATE_BMI_CI <- ci(ATE_BMI_pd, method = "HDI")
        ATE_BMI_CI_plot <-  plot(ATE_BMI_CI, ATE_BMI_pd, show_zero = TRUE)
        evaluation_results[["ATE CIs"]][["BMI"]] <- list("ATE CI BMI" = ATE_BMI_CI , "ATE BMI plot" = ATE_BMI_CI_plot)
        
        ATE_waist_pd <- rowMeans(bcf_post$`posterior_results waist`$tau) 
        ATE_waist_CI <- ci(ATE_waist_pd, method = "HDI")
        ATE_waist_CI_plot <-  plot(ATE_waist_CI, ATE_waist_pd, show_zero = TRUE)
        evaluation_results[["ATE CIs"]][["waist"]]  <- list("ATE CI waist" = ATE_waist_CI, "ATE waist plot" = ATE_waist_CI_plot)
      
      }
      
      if("CATEs" %in% evaluation_methods){
        #Sex
        ATE_syBP_pd_female <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 0)]) 
        ATE_syBP_female_CI <- ci(ATE_syBP_pd_female, method = "HDI")
        ATE_syBP_female_CI_plot <-  plot(ATE_syBP_female_CI, ATE_syBP_pd_female, show_zero = TRUE)
        
        ATE_syBP_pd_male <- rowMeans(bcf_post$`posterior_results syBP`$tau[, which(bcf_post$effect_moderators[, "sex"] == 1)]) 
        ATE_syBP_male_CI <- ci(ATE_syBP_pd_male, method = "HDI")
        ATE_syBP_male_CI_plot <-  plot(ATE_syBP_male_CI, ATE_syBP_pd_male, show_zero = TRUE)
        
        #Wealth bin 
        
        ATE_stack_syBP_black <-  data.frame(stack = c(t(bcf_post$'posterior results'$waist$tau[, which(bcf_post$effect_moderators[,5] == 1)])))
        ATE_syBP_black_CI <- ci(ATE_stack_syBP_black, method = "HDI")
        ATE_syBP_black_CI_plot <-  plot(ATE_syBP_black_CI, ATE_stack_syBP_black, show_zero = TRUE)
      }
    }
      
  }else{
    print("No input provided for posterior results")
  }
}