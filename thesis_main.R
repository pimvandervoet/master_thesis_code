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
    "expODAll",
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


#Do simple resample to make sure that 'representative summary statistics can be obtained -- this is no longer valid!
# source("1_Data_preperation/resampling.R")
#  repsample_sumstats <- simple_resampler(analysis_dataset)
#  repsample_households <- nrow(repsample_sumstats)
#  seperated_repsample <- data_seperator(repsample_sumstats, controlvariables_2, moderatorvariables_2, treatmentvariables_1, outcomevariables_2)
#  sumstats_repsample <- summary_statistics(seperated_repsample)

#Obtain weighted summary statistics#####
library(radiant.data)
weightvector_1 <- data_seperated_1$controls$sampleWeight
treated_1 <- which(data_seperated_1$treatment == 1)
non_treated_1 <- which(data_seperated_1$treatment == 0)
weighted_means_1 <-  c("sex tr" = weighted.mean(data_seperated_1$moderators$sex[treated_1], weightvector_1[treated_1 ]),
                     "age tr" = weighted.mean(data_seperated_1$moderators$age[treated_1], weightvector_1[treated_1 ]),
                     "syBP tr" = weighted.mean(data_seperated_1$outcomes$syBP_mean[treated_1 ], weightvector_1[treated_1 ]),
                     "BMI tr"  = weighted.mean(data_seperated_1$outcomes$BMI[treated_1 ], weightvector_1[treated_1 ]),
                     "waist tr" = weighted.mean(data_seperated_1$outcomes$waist[treated_1 ], weightvector_1[treated_1 ]),
                     "sex nt" = weighted.mean(data_seperated_1$moderators$sex[non_treated_1 ], weightvector_1[non_treated_1 ]),
                     "age nt" = weighted.mean(data_seperated_1$moderators$age[non_treated_1 ], weightvector_1[non_treated_1 ]),
                     "syBP nt" = weighted.mean(data_seperated_1$outcomes$syBP_mean[non_treated_1 ], weightvector_1[non_treated_1 ]),
                     "BMI nt"  = weighted.mean(data_seperated_1$outcomes$BMI[non_treated_1 ], weightvector_1[non_treated_1 ]),
                     "waist nt" = weighted.mean(data_seperated_1$outcomes$waist[non_treated_1 ], weightvector_1[non_treated_1 ]),
                     "sex all" = weighted.mean(data_seperated_1$moderators$sex, weightvector_1),
                     "age all" = weighted.mean(data_seperated_1$moderators$age, weightvector_1),
                     "syBP all" = weighted.mean(data_seperated_1$outcomes$syBP_mean, weightvector_1),
                     "BMI all"  = weighted.mean(data_seperated_1$outcomes$BMI, weightvector_1),
                     "waist all" = weighted.mean(data_seperated_1$outcomes$waist, weightvector_1)
)
weighted_sd_1 <-  c("sex tr" = weighted.sd(data_seperated_1$moderators$sex[treated_1 ], weightvector_1[treated_1 ]),
                       "age tr" = weighted.sd(data_seperated_1$moderators$age[treated_1 ], weightvector_1[treated_1 ]),
                       "syBP tr" = weighted.sd(data_seperated_1$outcomes$syBP_mean[treated_1 ], weightvector_1[treated_1 ]),
                       "BMI tr"  = weighted.sd(data_seperated_1$outcomes$BMI[treated_1 ], weightvector_1[treated_1 ]),
                       "waist tr" = weighted.sd(data_seperated_1$outcomes$waist[treated_1 ], weightvector_1[treated_1 ]),
                       "sex nt" = weighted.sd(data_seperated_1$moderators$sex[non_treated_1 ], weightvector_1[non_treated_1 ]),
                       "age nt" = weighted.sd(data_seperated_1$moderators$age[non_treated_1 ], weightvector_1[non_treated_1 ]),
                       "syBP nt" = weighted.sd(data_seperated_1$outcomes$syBP_mean[non_treated_1 ], weightvector_1[non_treated_1 ]),
                       "BMI nt"  = weighted.sd(data_seperated_1$outcomes$BMI[non_treated_1 ], weightvector_1[non_treated_1 ]),
                       "waist nt" = weighted.sd(data_seperated_1$outcomes$waist[non_treated_1 ], weightvector_1[non_treated_1 ]),
                    "sex all" = weighted.sd(data_seperated_1$moderators$sex, weightvector_1),
                    "age all" = weighted.sd(data_seperated_1$moderators$age, weightvector_1),
                    "syBP all" = weighted.sd(data_seperated_1$outcomes$syBP_mean, weightvector_1),
                    "BMI all"  = weighted.sd(data_seperated_1$outcomes$BMI, weightvector_1),
                    "waist all" = weighted.sd(data_seperated_1$outcomes$waist, weightvector_1)
)


counts_1 <- c("white_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "1.white/caucasian"), treated_1)]),
              "black_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "2.black/african american"), treated_1)]),
              "other_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "3.other"), treated_1)]),
              "wb1_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "1"), treated_1)]),
              "wb2_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "2"), treated_1)]),
              "wb3_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "3"), treated_1)]),
              "wb4_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "4"), treated_1)]),
              "wb5_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "5"), treated_1)]),
              "wb6_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "6"), treated_1)]),
              "wb7_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "7"), treated_1)]),
              "wb8_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "8"), treated_1)]),
              "wb9_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "9"), treated_1)]),
              "wb10_tr" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "10"), treated_1)]),
              "white_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "1.white/caucasian"), non_treated_1)]),
              "black_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "2.black/african american"), non_treated_1)]),
              "other_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$race == "3.other"), non_treated_1)]),
              "wb1_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "1"), non_treated_1)]),
              "wb2_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "2"), non_treated_1)]),
              "wb3_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "3"), non_treated_1)]),
              "wb4_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "4"), non_treated_1)]),
              "wb5_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "5"), non_treated_1)]),
              "wb6_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "6"), non_treated_1)]),
              "wb7_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "7"), non_treated_1)]),
              "wb8_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "8"), non_treated_1)]),
              "wb9_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "9"), non_treated_1)]),
              "wb10_nt" = sum(weightvector_1[intersect(which(data_seperated_1$moderators$wealth_bin == "10"), non_treated_1)]),
              "white_all" = sum(weightvector_1[which(data_seperated_1$moderators$race == "1.white/caucasian")]),
              "black_all" = sum(weightvector_1[which(data_seperated_1$moderators$race == "2.black/african american")]),
              "other_all" = sum(weightvector_1[which(data_seperated_1$moderators$race == "3.other")]),
              "wb1_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "1")]),
              "wb2_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "2")]),
              "wb3_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "3")]),
              "wb4_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "4")]),
              "wb5_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "5")]),
              "wb6_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "6")]),
              "wb7_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "7")]),
              "wb8_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "8")]),
              "wb9_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "9")]),
              "wb10_all" = sum(weightvector_1[which(data_seperated_1$moderators$wealth_bin == "10")])
              )



weightvector_2 <- data_seperated_2$controls$sampleWeight
treated_2 <- which(data_seperated_2$treatment == 1)
non_treated_2 <- which(data_seperated_2$treatment == 0)
weighted_means_2 <-  c("sex tr" = weighted.mean(data_seperated_2$moderators$sex[treated_2], weightvector_2[treated_2 ]),
                       "age tr" = weighted.mean(data_seperated_2$moderators$age[treated_2], weightvector_2[treated_2 ]),
                       "syBP tr" = weighted.mean(data_seperated_2$outcomes$d_syBP_mean[treated_2 ], weightvector_2[treated_2 ]),
                       "BMI tr"  = weighted.mean(data_seperated_2$outcomes$d_BMI[treated_2 ], weightvector_2[treated_2 ]),
                       "waist tr" = weighted.mean(data_seperated_2$outcomes$d_waist[treated_2 ], weightvector_2[treated_2 ]),
                       "sex nt" = weighted.mean(data_seperated_2$moderators$sex[non_treated_2 ], weightvector_2[non_treated_2 ]),
                       "age nt" = weighted.mean(data_seperated_2$moderators$age[non_treated_2 ], weightvector_2[non_treated_2 ]),
                       "syBP nt" = weighted.mean(data_seperated_2$outcomes$d_syBP_mean[non_treated_2 ], weightvector_2[non_treated_2 ]),
                       "BMI nt"  = weighted.mean(data_seperated_2$outcomes$d_BMI[non_treated_2 ], weightvector_2[non_treated_2 ]),
                       "waist nt" = weighted.mean(data_seperated_2$outcomes$d_waist[non_treated_2 ], weightvector_2[non_treated_2 ]),
                       "sex all" = weighted.mean(data_seperated_2$moderators$sex, weightvector_2),
                       "age all" = weighted.mean(data_seperated_2$moderators$age, weightvector_2),
                       "syBP all" = weighted.mean(data_seperated_2$outcomes$d_syBP_mean, weightvector_2),
                       "BMI all"  = weighted.mean(data_seperated_2$outcomes$d_BMI, weightvector_2),
                       "waist all" = weighted.mean(data_seperated_2$outcomes$d_waist, weightvector_2)
)
weighted_sd_2 <-  c("sex tr" = weighted.sd(data_seperated_2$moderators$sex[treated_2 ], weightvector_2[treated_2 ]),
                    "age tr" = weighted.sd(data_seperated_2$moderators$age[treated_2 ], weightvector_2[treated_2 ]),
                    "syBP tr" = weighted.sd(data_seperated_2$outcomes$d_syBP_mean[treated_2 ], weightvector_2[treated_2 ]),
                    "BMI tr"  = weighted.sd(data_seperated_2$outcomes$d_BMI[treated_2 ], weightvector_2[treated_2 ]),
                    "waist tr" = weighted.sd(data_seperated_2$outcomes$d_waist[treated_2 ], weightvector_2[treated_2 ]),
                    "sex nt" = weighted.sd(data_seperated_2$moderators$sex[non_treated_2 ], weightvector_2[non_treated_2 ]),
                    "age nt" = weighted.sd(data_seperated_2$moderators$age[non_treated_2 ], weightvector_2[non_treated_2 ]),
                    "syBP nt" = weighted.sd(data_seperated_2$outcomes$d_syBP_mean[non_treated_2 ], weightvector_2[non_treated_2 ]),
                    "BMI nt"  = weighted.sd(data_seperated_2$outcomes$d_BMI[non_treated_2 ], weightvector_2[non_treated_2 ]),
                    "waist nt" = weighted.sd(data_seperated_2$outcomes$d_waist[non_treated_2 ], weightvector_2[non_treated_2 ]),
                    "sex all" = weighted.sd(data_seperated_2$moderators$sex, weightvector_2),
                    "age all" = weighted.sd(data_seperated_2$moderators$age, weightvector_2),
                    "syBP all" = weighted.sd(data_seperated_2$outcomes$d_syBP_mean, weightvector_2),
                    "BMI all"  = weighted.sd(data_seperated_2$outcomes$d_BMI, weightvector_2),
                    "waist all" = weighted.sd(data_seperated_2$outcomes$d_waist, weightvector_2)
)


counts_2 <- c("white_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "1.white/caucasian"), treated_2)]),
              "black_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "2.black/african american"), treated_2)]),
              "other_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "3.other"), treated_2)]),
              "wb1_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "1"), treated_2)]),
              "wb2_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "2"), treated_2)]),
              "wb3_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "3"), treated_2)]),
              "wb4_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "4"), treated_2)]),
              "wb5_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "5"), treated_2)]),
              "wb6_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "6"), treated_2)]),
              "wb7_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "7"), treated_2)]),
              "wb8_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "8"), treated_2)]),
              "wb9_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "9"), treated_2)]),
              "wb10_tr" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "10"), treated_2)]),
              "white_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "1.white/caucasian"), non_treated_2)]),
              "black_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "2.black/african american"), non_treated_2)]),
              "other_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$race == "3.other"), non_treated_2)]),
              "wb1_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "1"), non_treated_2)]),
              "wb2_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "2"), non_treated_2)]),
              "wb3_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "3"), non_treated_2)]),
              "wb4_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "4"), non_treated_2)]),
              "wb5_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "5"), non_treated_2)]),
              "wb6_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "6"), non_treated_2)]),
              "wb7_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "7"), non_treated_2)]),
              "wb8_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "8"), non_treated_2)]),
              "wb9_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "9"), non_treated_2)]),
              "wb10_nt" = sum(weightvector_2[intersect(which(data_seperated_2$moderators$wealth_bin == "10"), non_treated_2)]),
              "white_all" = sum(weightvector_2[which(data_seperated_2$moderators$race == "1.white/caucasian")]),
              "black_all" = sum(weightvector_2[which(data_seperated_2$moderators$race == "2.black/african american")]),
              "other_all" = sum(weightvector_2[which(data_seperated_2$moderators$race == "3.other")]),
              "wb1_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "1")]),
              "wb2_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "2")]),
              "wb3_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "3")]),
              "wb4_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "4")]),
              "wb5_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "5")]),
              "wb6_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "6")]),
              "wb7_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "7")]),
              "wb8_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "8")]),
              "wb9_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "9")]),
              "wb10_all" = sum(weightvector_2[which(data_seperated_2$moderators$wealth_bin == "10")])
)


#Provide initial insights on the data (missings, distributions etc.) 

#FOR NOW - oversimplified KNN imputation of data to test model. - this knn makes some factors malformed...
#source("1_Data_preperation/simple_knn_imputation.R")
#testdataset <- simple_knn(data_seperated_1)

#Prepare the data by treating missings, constructing subsets, creating new variables

#Provide insights again, but now with the "cleaned data" 

######Propensity score estimates#####

#Perform K-fold cross validation to do parameter tuning
#source("2_Propensity_estimation/cv_ps_est.R")
#AUC_matrix <- cv_ps_est(data_seperated_1, k_fold = 10, k_par = c(2), m_par = c(400))



#Provide 'simple' propensity score estimates - without oversampling treated
source("2_Propensity_estimation/propensity_score_estimation.R")
library(ROCR)

#Convergence is checked inside ps_estimator function manuallly
simple_ps_est <- ps_estimator(data_seperated_1$controls[,!names(data_seperated_1$controls) %in% c("sampleWeight")],
                              data_seperated_1$treatment,
                              samples = 1000,
                              technique = "BARTMACHINE",
                              take_means_draws = FALSE,
                              k_fold_cv = 1,
                              repeats = 1,
                              k_parameter = 3,
                              m_parameter = 50
                              )

pred_simple <- prediction(simple_ps_est, data_seperated_1$treatment)
perf_simple <- performance(pred_simple,"tpr","fpr")
auc_plot_1 <- plot(perf_simple,colorize=TRUE)
auc_ROCR_simple_1 <- performance(pred_simple, measure = "auc")
auc_ROCR_simple_1 <- auc_ROCR_simple_1@y.values[[1]]

simple_ps_est_2 <- ps_estimator(data_seperated_2$controls[,!names(data_seperated_2$controls) %in% c("sampleWeight")],
                               data_seperated_2$treatment,
                               samples = 1000,
                               technique = "BARTMACHINE",
                               take_means_draws = TRUE,
                               k_fold_cv = 1,
                               repeats = 1,
                               k_parameter = 3,
                               m_parameter = 50)

pred_simple_2 <- prediction(simple_ps_est_2, data_seperated_2$treatment)
perf_simple_2 <- performance(pred_simple_2,"tpr","fpr")
auc_plot_2 <- plot(perf_simple_2,colorize=TRUE)
auc_ROCR_simple_2 <- performance(pred_simple_2, measure = "auc")
auc_ROCR_simple_2 <- auc_ROCR_simple_2@y.values[[1]]

source("3_Model_estimation/make_model_matrix_1.R")
model_dataset <- make_model_matrix_1(data_seperated_1, simple_ps_est) 
model_dataset_2 <- make_model_matrix_1(data_seperated_2, simple_ps_est_2) 

#library(imbalance)
# os_input_1 <- as.data.frame(cbind(model_dataset$controls, model_dataset$treatment))
# oversample_SMOTE_1 <- oversample(os_input_1[,!names(os_input_1) %in% c("sampleWeight")], 0.8, "SMOTE", filtering = FALSE, classAttr =  "expRDAll")
# treatment_tib <- tibble(oversample_SMOTE_1$expRDAll)
# names(treatment_tib) <- c("expRDAll")
# 
# os_input_2 <- as.data.frame(cbind(model_dataset_2$controls, model_dataset_2$treatment))
# oversample_SMOTE_2 <- oversample(os_input_2[,!names(os_input_2) %in% c("sampleWeight")], 0.8, "SMOTE", filtering = FALSE, classAttr =  "expRDAll")
# treatment_tib_2 <- tibble(oversample_SMOTE_2$expRDAll)
# names(treatment_tib_2) <- c("expRDAll")

#Obtain SMOTE pihat

# oversampled_ps_est <- ps_estimator(oversample_SMOTE_1[, !names(oversample_SMOTE_1) %in% c("expRDAll")],
#                               treatment_tib,
#                               samples = 1000,
#                               technique = "BARTMACHINE",
#                               take_means_draws = TRUE,
#                               k_fold_cv = 1,
#                               repeats = 1)
# 
# oversampled_ps_est_2 <- ps_estimator(oversample_SMOTE_2[, !names(oversample_SMOTE_2) %in% c("expRDAll")],
#                                  treatment_tib_2,
#                                  samples = 1000,
#                                  technique = "BARTMACHINE",
#                                  take_means_draws = TRUE,
#                                  k_fold_cv = 1,
#                                  repeats = 1,
#                                  k_parameter = 3,
#                                  m_parameter = 200)
#  
# # model_dataset_smote_1 <- model_dataset
# # model_dataset_smote_1$ps_estimates <- oversampled_ps_est[1:dim(model_dataset_smote_1$treatment)[1], ]
#  
# #Prep data to go into model
#  model_dataset_smote_2 <- model_dataset
#  model_dataset_smote_2$ps_estimates <- oversampled_ps_est_2[1:dim(model_dataset_smote_2$treatment)[1], ]
# 
#  #Plot performance
#  pred_smote_2 <- prediction(model_dataset_smote_2$ps_estimates, model_dataset_smote_2$treatment)
#  perf_smote_2 <- performance(pred_smote_2,"tpr","fpr")
#  plot(perf_smote_2,colorize=TRUE)
#  auc_ROCR_smote_2 <- performance(pred_smote_2, measure = "auc")
#  auc_ROCR_smote_2 <- auc_ROCR_smote_2@y.values[[1]]
 
 
#Obtain posterior results#####
source("3_Model_estimation/BCF_function.R")

#Cross-sectional model with 'simple'estimates for pi hat
bcf_test <- BCF_estimation(model_dataset$outcomes, model_dataset$controls, model_dataset$moderators, model_dataset$treatment, model_dataset$ps_estimates, no_draws = 10000, burnin = 1000)
attributes(bcf_test$effect_moderators)$dimnames[[2]][[7]] <- "ps_estimates"
colnames(bcf_test$effect_moderators) <- attributes(bcf_test$effect_moderators)$dimnames[[2]]

library(coda)
raterfy_syBP <- coda::raftery.diag(coda::as.mcmc(bcf_test$`posterior_results syBP`$tau))
raterfy_BMI <- coda::raftery.diag(coda::as.mcmc(bcf_test$`posterior_results BMI`$tau))
raterfy_waist <- coda::raftery.diag(coda::as.mcmc(bcf_test$`posterior_results waist`$tau))


#Difference model with 'simple' estimates for pi hat
bcf_test2 <- BCF_estimation(model_dataset_2$outcomes, model_dataset_2$controls, model_dataset_2$moderators, model_dataset_2$treatment, model_dataset_2$ps_estimates, no_draws = 10000, burnin = 1000)
attributes(bcf_test2$effect_moderators)$dimnames[[2]][[7]] <- "ps_estimates"
colnames(bcf_test2$effect_moderators) <- attributes(bcf_test2$effect_moderators)$dimnames[[2]]

library(coda)
raterfy_syBP2 <- coda::raftery.diag(coda::as.mcmc(bcf_test2$`posterior_results syBP`$tau))
raterfy_BMI2 <- coda::raftery.diag(coda::as.mcmc(bcf_test2$`posterior_results BMI`$tau))
raterfy_waist2 <- coda::raftery.diag(coda::as.mcmc(bcf_test2$`posterior_results waist`$tau))

#Cross-sectional model with SMOTE pi hat estimates
# bcf_test_smote <- BCF_estimation(model_dataset_smote_1$outcomes, model_dataset_smote_1$controls, model_dataset_smote_1$moderators, model_dataset_smote_1$treatment, model_dataset_smote_1$ps_estimates)
# 
# 
# #Difference model with SMOTE pi hat estimates
# bcf_test_smote2 <- BCF_estimation(model_dataset_smote_2$outcomes, model_dataset_smote_2$controls, model_dataset_smote_2$moderators, model_dataset_smote_2$treatment, model_dataset_smote_2$ps_estimates)


#####Evaluation of results#####
source("4_Evaluation/model_evaluation.R")
bcf_base_results <- evalpost(bcf_test, evaluation_methods = c("ATE", "Credibility interval", "CATEs"))
bcf_long_results <- evalpost(bcf_test2, evaluation_methods = c("ATE", "Credibility interval", "CATEs"))
                             
#Posterior exploration cross-sectional sample
library(rpart)
library(rattle)

#Systolic Blood pressure
exploreset_sybp <- cbind(colMeans(bcf_test$`posterior_results syBP`$tau), bcf_test$effect_moderators)
colnames(exploreset_sybp)[1] <- "tau"
exploreset_sybp <- as.data.frame(exploreset_sybp)
model <- rpart(tau ~., data = exploreset_sybp)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#BMI
exploreset_BMI <- cbind(colMeans(bcf_test$`posterior_results BMI`$tau), bcf_test$effect_moderators)
colnames(exploreset_BMI)[1] <- "tau"
exploreset_BMI <- as.data.frame(exploreset_BMI)
model <- rpart(tau ~., data = exploreset_BMI)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#Waist
exploreset_waist <- cbind(colMeans(bcf_test$`posterior_results waist`$tau), bcf_test$effect_moderators)
colnames(exploreset_waist)[1] <- "tau"
exploreset_waist <- as.data.frame(exploreset_waist)
model <- rpart(tau ~., data = exploreset_waist)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#Posterior exploration longitudinal sample

#Systolic Blood pressure
exploreset_sybp <- cbind(colMeans(bcf_test2$`posterior_results syBP`$tau), bcf_test2$effect_moderators)
colnames(exploreset_sybp)[1] <- "tau"
exploreset_sybp <- as.data.frame(exploreset_sybp)
model <- rpart(tau ~., data = exploreset_sybp)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#BMI
exploreset_BMI <- cbind(colMeans(bcf_test2$`posterior_results BMI`$tau), bcf_test2$effect_moderators)
colnames(exploreset_BMI)[1] <- "tau"
exploreset_BMI <- as.data.frame(exploreset_BMI)
model <- rpart(tau ~., data = exploreset_BMI)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#Waist
exploreset_waist <- cbind(colMeans(bcf_test2$`posterior_results waist`$tau), bcf_test2$effect_moderators)
colnames(exploreset_waist)[1] <- "tau"
exploreset_waist <- as.data.frame(exploreset_waist)
model <- rpart(tau ~., data = exploreset_waist)
fancyRpartPlot(model, "", "", palettes = "RdBu")

#Save results
save.image("unweighted_analysis_results.RData")

#### Weighted analysis #### ---- BEFORE RUNNING NEED TO RESTART R AND SET NO OF PARRALEL LOOPS IN PS_ESTIMATOR #####
#Code below is runned in chuncks of 100 (with the exception of the first run which contained 200 iterations) first for the cross sectional dataset and then for the longitudinal dataset
#Seeds were manually changed each iterations. Starting with both seeds at 30121997 for the first run, then 13022021 to 31022021 for the other 18. 

  library(dplyr)
  wanalysis_data_seperated <- data_seperated_1
  rm(list=setdiff(ls(), c("wanalysis_data_seperated", lsf.str())))
  #Scale sample weights to sum of weights
  totalweights <- sum(wanalysis_data_seperated$controls$sampleWeight)
  wanalysis_data_seperated$controls <- mutate(wanalysis_data_seperated$controls, sampleWeight = sampleWeight/totalweights)
  
  #To save in loop
  set.seed(13022021)
  loopsize <- 100
  drawsPL <- 1000
  
  library(foreach)
  library(parallel)
  library(doParallel)
  library(doRNG)
  #Actual loop
  
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  doRNG::registerDoRNG(13022021)
  init <- Sys.time()
  w_an <- foreach(PRS_count=1:loopsize, .packages = c("dplyr", "forcats", "haven", "bartMachine", "bcf"),  .combine=c, .multicombine=TRUE,
                  .init=list()) %dopar% {
    #Generate PRS
    PRS <- list()
    individuals_shuffle <- sample(nrow(wanalysis_data_seperated$controls), size = nrow(wanalysis_data_seperated$controls), replace = TRUE, prob = wanalysis_data_seperated$sampleWeight )
    PRS$controls <- wanalysis_data_seperated$controls[individuals_shuffle, ]
    PRS$moderators <- wanalysis_data_seperated$moderators[individuals_shuffle, ]
    PRS$outcomes <- wanalysis_data_seperated$outcomes[individuals_shuffle, ]
    PRS$treatment <- wanalysis_data_seperated$treatment[individuals_shuffle, ]
     
    #Estimate propensity scores with pre-tuned parameters - fix nr of cores used before running!!
    loop_ps_est <- ps_estimator(PRS$controls[,!names(PRS$controls) %in% c("sampleWeight")],
                                 PRS$treatment,
                                 samples = 500, 
                                 technique = "BARTMACHINE",
                                 take_means_draws = TRUE,
                                 k_fold_cv = 1,
                                 repeats = 1,
                                 k_parameter = 3,
                                 m_parameter = 50
     )
  
    loop_model_dataset <- make_model_matrix_1(PRS, loop_ps_est)
  
  
    #Estimate BCF model - make sure to adapt nr of draws before doing this in BCF function
    bcf_loop <- BCF_estimation(loop_model_dataset$outcomes, loop_model_dataset$controls, loop_model_dataset$moderators, loop_model_dataset$treatment, loop_model_dataset$ps_estimates, no_draws = drawsPL , burnin = 200 )
  
    #Save relevant results, throw away all other data
    return(list("PRS_results_syBP" <- t(bcf_loop$`posterior_results syBP`$tau),
    "PRS_results_BMI" <- t(bcf_loop$`posterior_results BMI`$tau),
    "PRS_results_waist" <- t(bcf_loop$`posterior_results waist`$tau), 
    individuals_shuffle, 
    loop_ps_est,
    PRS_count))
    
    remove(c(individuals_shuffle, PRS, loop_ps_est, loop_model_dataset, bcf_loop))
    
    
  }
  
  Sys.time() - init
  stopCluster(cl)
  
  save(w_an, file = "run_2_cross_sample.RData")

remove(w_an)
#Proceed only if gathered all runs

#Analyze CATEs and ITEs#####
source("4_Evaluation/weighted_model_evaluation.R")

#Obtain CATE and ITE posterior draws
w_an_eval_prep(loopsize, drawsPL, iterations_used = 10, "cross")
w_an_eval_prep(loopsize, drawsPL, iterations_used = 10, "long")

#Obtain point estimates, CI's, ITE individual lists, plots and PD's
cw_results <- w_an_eval(loopsize, drawsPL, iterations_used = 10, cross_long = "cross")
lw_results <- w_an_eval(loopsize, drawsPL, iterations_used = 10, cross_long = "long")

#Obtain summary statistics from quantiles in the ITES
sumstats_quantiles_cr <- list()
wealthbins_cr <- matrix(NA, 9, 10)
library(miceadds)

#Obtain moderators
load.Rdata("C_results_Weighted_Analysis/moderators_cross.RData", "moderators_cr")
#Loop through quantiles and fill list
for(qsel in 1:9){
  sumstats_quantiles_cr[[qsel]] <- summary(moderators_cr[cw_results$ITES[[qsel]],])
  wealthbins_cr[qsel,] <- table(moderators_cr$wealth_bin[cw_results$ITES[[qsel]]])/sum(table(moderators_cr$wealth_bin[cw_results$ITES[[qsel]]]))
  }

sumstats_quantiles_lo <- list()
wealthbins_lo <- matrix(NA, 9, 10)

load.Rdata("C_results_Weighted_Analysis/moderators_long.RData", "moderators_lo")
#Loop through quantiles and fill list
for(qsel in 1:9){
  sumstats_quantiles_lo[[qsel]] <- summary(moderators_lo[lw_results$ITES[[qsel]],]) 
  wealthbins_lo[qsel,] <- table(moderators_lo$wealth_bin[lw_results$ITES[[qsel]]])/sum(table(moderators_lo$wealth_bin[lw_results$ITES[[qsel]]]))
  
}


