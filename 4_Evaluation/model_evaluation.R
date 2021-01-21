#' evalpost
#' 
#' @description evaluation function BCF posterior results
#' 
#' @return evaluation_results - list with desired output 
#' 
#' @param bcf_post ... posterior-result from BCF_function
#' @param evaluation_methods ... desired output, default c("ATE", "Credibility interval", "CATE per moderator")
evalpost <- function(bcf_post, evaluation_methods = c("ATE", "Credbility interval", "CATEs", "plots ATE", "plots CATE")){
  if(is.na(bcf_post)){
    
    #Create empty list to store results
    evaluation_results <- list()
    
    #First obtain ATE for each outcome
    if("ATE" %in% evaluation_methods){
      ATE_syBP <- mean(colMeans(bcf_post$'posterior results'$syBP$tau))
      ATE_BMI <- mean(colMeans(bcf_post$'posterior results'$BMI$tau))
      ATE_waist <- mean(colMeans(bcf_post$'posterior results'$waist$tau))
      evaluation_results <- list(evaluation_results, "ATEs" = cbind(ATE_syBP, ATE_BMI, ATE_waist))
    }
    
    if("Credibility interval" %in% evaluation_methods){
      library(bayestestR)
      library(see)

      if("ATE" %in% evaluation_methods){
      #ATE credibility interval
        ATE_stack_syBP <- data.frame(stack = c(t(bcf_post$'posterior results'$syBP$tau)))
        ATE_syBP_CI <- ci(ATE_stack_syBP, method = "HDI")
        # ATE_syBP_CI$plot <-  ATE_stack_syBP %>% 
        #      estimate_density(extend=TRUE) %>% 
        #      ggplot(aes(x=x, y=y)) +
        #      #labs(y= "Posterior density", x = "Posterior effect on syBP") +
        #      geom_area(fill="orange") +
        #      theme_classic() +
        #      # HDI in blue
        #      geom_vline(xintercept=ATE_syBP_CI$CI_low, color="royalblue", size=2) +
        #      geom_vline(xintercept=ATE_syBP_CI$CI_high, color="royalblue", size=2) 
       ATE_syBP_CI_plot <-  plot(ATE_syBP_CI, ATE_stack_syBP, show_zero = TRUE)
        
        
        evaluation_results <- list(evaluation_results,  "ATE CI syBP" = ATE_syBP_CI, "ATE CI plot" = ATE_syBP_CI_plot)
        
        ATE_stack_BMI <- data.frame(stack = c(t(bcf_post$'posterior results'$BMI$tau)))
        ATE_BMI_CI <- ci(ATE_stack_BMI, method = "HDI")
        # ATE_BMI_CI$plot <-  ATE_stack_BMI %>% 
        #   estimate_density(extend=TRUE) %>% 
        #   ggplot(aes(x=x, y=y)) +
        #   geom_area(fill="orange") +
        #   theme_classic() +
        #   # HDI in blue
        #   geom_vline(xintercept=ATE_BMI_CI$CI_low, color="royalblue", size=2) +
        #   geom_vline(xintercept=ATE_BMI_CI$CI_high, color="royalblue", size=2) 
        ATE_BMI_CI_plot <-  plot(ATE_BMI_CI, ATE_stack_BMI, show_zero = TRUE)
        
        evaluation_results <- list(evaluation_results,  "ATE CI BMI" = ATE_BMI_CI , "ATE BMI plot" = ATE_BMI_CI_plot)
        
        
        ATE_stack_waist <- data.frame(stack = c(t(bcf_post$'posterior results'$waist$tau)))
        ATE_waist_CI <- ci(ATE_stack_waist, method = "HDI")
        # ATE_waist_CI$plot <-  ATE_stack_waist %>% 
        #   estimate_density(extend=TRUE) %>% 
        #   ggplot(aes(x=x, y=y)) +
        #   geom_area(fill="orange") +
        #   theme_classic() +
        #   # HDI in blue
        #   geom_vline(xintercept=ATE_waist_CI$CI_low, color="royalblue", size=2) +
        #   geom_vline(xintercept=ATE_waist_CI$CI_high, color="royalblue", size=2) 
        ATE_waist_CI_plot <-  plot(ATE_waist_CI, ATE_stack_waist, show_zero = TRUE)
        
        evaluation_results <- list(evaluation_results, "ATE CI waist" = ATE_waist_CI, "ATE waist plot" = ATE_waist_CI_plot)
      
      }
      
      if("CATEs" %in% evaluation_methods){
        ATE_stack_syBP_white <- data.frame(stack = c(t(bcf_post$'posterior results'$waist$tau[, which(bcf_post$effect_moderators[,4] == 1)])))
        ATE_syBP_white_CI <- ci(ATE_stack_syBP_white, method = "HDI")
        # ATE_BMI_CI$plot <-  ATE_stack_BMI %>% 
        #   estimate_density(extend=TRUE) %>% 
        #   ggplot(aes(x=x, y=y)) +
        #   geom_area(fill="orange") +
        #   theme_classic() +
        #   # HDI in blue
        #   geom_vline(xintercept=ATE_BMI_CI$CI_low, color="royalblue", size=2) +
        #   geom_vline(xintercept=ATE_BMI_CI$CI_high, color="royalblue", size=2) 
        ATE_syBP_white_CI_plot <-  plot(ATE_syBP_white_CI, ATE_stack_syBP_white, show_zero = TRUE)
        
        ATE_stack_syBP_black <-  data.frame(stack = c(t(bcf_post$'posterior results'$waist$tau[, which(bcf_post$effect_moderators[,5] == 1)])))
        ATE_syBP_black_CI <- ci(ATE_stack_syBP_black, method = "HDI")
        # ATE_BMI_CI$plot <-  ATE_stack_BMI %>% 
        #   estimate_density(extend=TRUE) %>% 
        #   ggplot(aes(x=x, y=y)) +
        #   geom_area(fill="orange") +
        #   theme_classic() +
        #   # HDI in blue
        #   geom_vline(xintercept=ATE_BMI_CI$CI_low, color="royalblue", size=2) +
        #   geom_vline(xintercept=ATE_BMI_CI$CI_high, color="royalblue", size=2) 
        ATE_syBP_black_CI_plot <-  plot(ATE_syBP_black_CI, ATE_stack_syBP_black, show_zero = TRUE)
      }
    }
      
  }else{
    print("No input provided for posterior results")
  }
}