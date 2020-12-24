#' BCF_function
#'
#' @description  This function is used to estimate treatment effects, using the BCF method proposed by Hahn, Murray, Carvalho 2020
#'
#' Output: @return results_for_posterior_inference .... list containing posterior draws and the moderating variables to do posterior inferencer
#' Input:  @param cvars ... control variables used in the model
#' @param outcome ... (health) outcome of interest 
#' @param treatment ....
#' @param mvars .... moderator variables used in the model
#' @param ps_estimates ... propensity score estimates providecxd by user
#' @param errordistribution ... error distribution used in the model, default is a Normal (0,1) distribution
#' @param sensitivity_parmaeters .... to be thought out
#' @param ps_estimate_inclusion ... setting for where to include propensity scores, default = "both", can be "control" or "moderatore" or "none" if the estimates shouldn't be includedd both in mu and tau 
#'
#' 
BCF_estimation <- function(outcome, cvars, mvars, treatment,  ps_estimates, errordistribution = "normal", sensitivity_parameters = "null", ps_estimate_inclusion = "both"){
  library(bcf)
  library(dbarts)
  #First transform control and moderator variables to matrix from dataframe, drops columsn that are constant or factors levels without instances
  cvars_matrix <- makeModelMatrixFromDataFrame(cvars, drop = TRUE) 
  mvars_matrix <- makeModelMatrixFromDataFrame(mvars, drop = TRUE)
  
  #Obtain model estimates using the bcf function of Hahn, Carvlho, Murray 2020
  bcf_estimates <- bcf(
    y = outcome,
    z = treatment,
    x_control = cvars_matrix,
    x_moderate = mvars_matrix,
    pihat = ps_estimates,
    nburn = 2000L,
    nsim = 4000L,
    nthin = 3L,
    update_interval = 200L,
    #Prior specification and nr of trees etc. according to specification of Hahn, Murray, Carvalho 2020
    ntree_control = 200L,
    sd_control = 2 * sd(outcome),
    base_control = 0.95,
    power_control = 2,
    ntree_moderate = 50,
    sd_moderate = sd(outcome),
    base_moderate = 0.25,
    power_moderate = 3,
    nu = 3,
    lambda = NULL,
    sigq = 0.9,
    sighat = NULL,
    include_pi = ps_estimate_inclusion,
    use_muscale = TRUE,
    use_tauscale = TRUE
  )
  
  #Return both posterior results and features of all individuals to do posterior inference
  results_for_posterior_inference <- list("posterior_draws" = bcf_estimates, "effect_moderators" = mvars)
  return(results_for_posterior_inference)
  
  }