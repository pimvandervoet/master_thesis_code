###Data loading and preperation####
#Set working directory
setwd("C:/Users/pimva/Documents/Studie/Thesis/Programming")
set.seed(301297)

#Read in data from stata file (note that this is an extraction of variables from the HRS)
source("0_Data_loading/data_loader.R")
HRS_data_unprepared <- data_loader(filename = "0_Data_loading/merged_data.dta")

#Basic data treatment ####
source("1_Data_preperation/data_preperation.R")
#Rename all variables
renamed_HRS_data <- variable_rename(HRS_data_unprepared)
remove(HRS_data_unprepared)

#Format all variables BUT treatment variables
formatted_data <- data_formatter(renamed_HRS_data)
remove(renamed_HRS_data)

#Make treatment a binary variable
racism_added <- define_racial_discrimination(formatted_data)
remove(formatted_data)

#Add other variables
analysis_dataset <- define_variables(racism_added)
remove(racism_added)

#Create weighted samples by two methods -- do later                        

#Oversample by weight
#Construct pseudo representative samples - (sample probability by weight) -> isn't that the same though in the end...

#Do missing data anlysis
#To do still...

#Create seperations of data and select which variables are used as control/moderator variables: Model 1 used to explain levels, Model 2 to explain changes between 2012 and 2016
controlvariables_1 <-
  c(
    "sampleWeight",
    "sex",
    "race",
    "wealth_bin",
    "age",
    "moEducation",
    "education",
    "mStat11",
    "mStat13",
    "prevRetStat",
    "workingNow",
    "married",
    "drinker13",
    "drinker11",   
    "nDDrink_pw",
    "nGDrink_pw",
    "nDDrink",
    'nGDrink',
    "nDrinkPerWeek",
    "nDrinkPerWeek_pw",
    "smoker13",
    "smoker11",
    "nSmokepw",
    "everSmoke",
    "smokenow",
    "nSmokenow",
    "nSmokemos",
    "vigAct",
    "modAct",
    "milAct"
  )

moderatorvariables_1 <- c("sex", "race", "wealth_bin", "age")
outcomevariables_1 <- c("syBP_mean", "BMI", "waist")
treatmentvariables_1 <- c("expRDAll")

controlvariables_2 <- c(controlvariables_1, "quit_smoking", "started_smoking", "quit_drinking", "started_drinking", "divorced_or_seperated", "widowed", "recently_married_or_partnered", "quit_working")
moderatorvariables_2 <- moderatorvariables_1
outcomevariables_2 <- c("d_syBP_mean", "d_BMI", "d_waist")
treatmentvariables_2 <- treatmentvariables_1


#Make data sets and remove observations with NA in outcomes or moderators to prevent severe bias
source("1_Data_preperation/simple_knn_imputation.R")
#Basic analysis
data_seperated_1 <- data_seperator(analysis_dataset, controlvariables_1, moderatorvariables_1, treatmentvariables_1, outcomevariables_1)
data_seperated_1 <- remove_NA_outcomes(data_seperated_1)
data_seperated_1 <- remove_obs_missing_mods(data_seperated_1)
data_seperated_1 <- remove_big_missing_control(data_seperated_1)
data_seperated_1 <- remove_noninformative_missing_control(data_seperated_1)
data_seperated_1 <- remove_unknown_activity(data_seperated_1)
data_seperated_1 <- impute_from_other_wave(data_seperated_1)
data_seperated_1 <- remove_unknown_smoke_drink_13(data_seperated_1)
data_seperated_1 <- knn_impute_moEducation(data_seperated_1)
data_seperated_1 <- mode_impute_education(data_seperated_1)

#Difference between waves analysis
data_seperated_2 <- data_seperator(analysis_dataset, controlvariables_2, moderatorvariables_2, treatmentvariables_2, outcomevariables_2)
data_seperated_2 <- remove_NA_outcomes(data_seperated_2)
data_seperated_2 <- remove_obs_missing_mods(data_seperated_2)
data_seperated_2 <- remove_big_missing_control(data_seperated_2)
data_seperated_2 <- remove_noninformative_missing_control(data_seperated_2)
data_seperated_2 <- remove_unknown_activity(data_seperated_2)
data_seperated_2 <- impute_from_other_wave(data_seperated_2)
data_seperated_2 <- remove_unknown_smoke_drink_13(data_seperated_2)
data_seperated_2 <- knn_impute_moEducation(data_seperated_2)
data_seperated_2 <- mode_impute_education(data_seperated_2)
data_seperated_2 <- impute_missings_additional_vars(data_seperated_2)

#Basic insights####

source("1_Data_preperation/preliminary_data_analysis.R")

#Gather summary statistics
sumstats_1 <- summary_statistics(data_seperated_1, main_split = "treatment", further_splits = "race")
sumstats_2 <- summary_statistics(data_seperated_2, main_split = "treatment", further_splits = "race")

#Create some plots to give insight in how the health outcomes are divided over moderator variables and experiences of racial discrimintion
treatment_plots <- treatment_analysis(data_seperated_1)

#Deal with missing data in control variables by considering whether high correlation with treatment - high correlation implicates that there may be bias induced by removing variable


#Do simple resample to make sure that 'representative summary statistics can be obtained
source("1_Data_preperation/resampling.R")
# repsample_sumstats <- simple_resampler(analysis_dataset)
# repsample_households <- nrow(repsample_sumstats)
# seperated_repsample <- data_seperator(repsample_sumstats, controlvariables_2, moderatorvariables_2, treatmentvariables_2, outcomevariables_2)
# sumstats_repsample <- summary_statistics(seperated_repsample)


#Provide initial insights on the data (missings, distributions etc.) 

#FOR NOW - oversimplified KNN imputation of data to test model. - this knn makes some factors malformed...
#source("1_Data_preperation/simple_knn_imputation.R")
#testdataset <- simple_knn(data_seperated_1)
#testdataset$moderators <- testdataset$controls[, moderatorvariables_1]
testdataset$

#Prepare the data by treating missings, constructing subsets, creating new variables

#Provide insights again, but now with the "cleaned data" 

######Propensity score estimates#####

#Provide 'simple' propensity score estimates - without oversampling treated
source("2_Propensity_estimation/propensity_score_estimation.R")
simple_ps_est <- ps_estimator(testdataset$controls,
                              testdataset$treatment,
                              samples = 1000,
                              technique = "BART",
                              take_means_draws = TRUE,
                              k_fold_cv = 10,
                              repeats = 1)

#Provide 'improved' propensity score estimates - with oversampling treated by SMOTE
testdataset$treatment <- mutate(testdataset$treatment, expRDAll = as_factor(expRDAll))
testdataset_ps <- as.data.frame(cbind(testdataset$controls, testdataset$treatment))
resampled_ps_est <- ps_estimator(ps_resample[, !names(ps_resample) %in% c("expRDAll")],
                                 ps_resample[, names(ps_resample) %in% c("expRDAll")],
                                 samples = 1000,
                                 technique = "BART",
                                 take_means_draws = TRUE,
                                 k_fold_cv = 10,
                                 repeats = 1) 

#Provide insights on the estimated propensity scores

#####Estimate the posterior treatment function with BCF - basics######
source("3_Model_estimation/make_model_matrix_1.R")
model_dataset <- make_model_matrix_1(testdataset, simple_ps_est) 

#Data needs to NOT include any missing data
source("3_Model_estimation/BCF_function.R")
bcf_test <- BCF_estimation(model_dataset$outcomes, model_dataset$controls, model_dataset$moderators, model_dataset$treatment, model_dataset$ps_estimates)

#Evaluation of results

#Estimate posterior treatment function including sensitivity analysis