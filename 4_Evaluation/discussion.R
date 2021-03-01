setwd("C:/Users/pimva/Documents/Studie/Thesis/Programming")
set.seed(301297)

#Read in data from stata file (note that this is an extraction of variables from the HRS)
source("0_Data_loading/data_loader.R")
HRS_data_unprepared <-
  data_loader(filename = "0_Data_loading/merged_data_discussion.dta")

source("1_Data_preperation/data_preperation.R")
renamed_HRS_data <- variable_rename(HRS_data_unprepared)
remove(HRS_data_unprepared)

renamed_HRS_data <- renamed_HRS_data %>%
  rename(
    gripDom = pi815,
    lefthandfirst = pi816,
    righthandfirst = pi851,
    lefthandsecond = pi852,
    righthandsecond = pi853,
    timeseendoctor = pn147,
    numbertimesseendoctor20x = pn148,
    numbertimeseendoctor5x = pn149,
    soughtadvicedcotor = pn150,
    soughtadvicedoctor50x = pn151
  )


#Format all variables BUT treatment variables
formatted_data <- data_formatter(renamed_HRS_data)
remove(renamed_HRS_data)

#Make treatment a binary variable
racism_added <- define_racial_discrimination(formatted_data)
remove(formatted_data)

#Add other variables
analysis_dataset <- define_variables(racism_added)
remove(racism_added)

#Make the variables for grip strength and health care usage
analysis_dataset <- discussion_variables(analysis_dataset)

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

controlvariables_2 <-
  c(
    controlvariables_1,
    "quit_smoking",
    "started_smoking",
    "quit_drinking",
    "started_drinking",
    "divorced_or_seperated",
    "widowed",
    "recently_married_or_partnered",
    "quit_working"
  )
moderatorvariables_2 <- moderatorvariables_1
outcomevariables_2 <- c("d_syBP_mean", "d_BMI", "d_waist")
treatmentvariables_2 <- treatmentvariables_1

controlvariables_discussion <-
  c(controlvariables_2, "timeseendoctor")
outcomevariables_discussion <- c(outcomevariables_1, "gripstrength")
moderatorvariables_discussion <- moderatorvariables_1
treatmentvariables_discussion <- treatmentvariables_1

source("1_Data_preperation/simple_knn_imputation.R")
#Basic analysis
data_seperated_d <-
  data_seperator(
    analysis_dataset,
    controlvariables_discussion,
    moderatorvariables_discussion,
    treatmentvariables_discussion,
    outcomevariables_discussion
  )
data_seperated_d <- remove_NA_outcomes(data_seperated_d)
data_seperated_d <- remove_obs_missing_mods(data_seperated_d)
data_seperated_d <- remove_big_missing_control(data_seperated_d)
data_seperated_d <-
  remove_noninformative_missing_control(data_seperated_d)
data_seperated_d <- remove_unknown_activity(data_seperated_d)
data_seperated_d <- impute_from_other_wave(data_seperated_d)
data_seperated_d <- remove_unknown_smoke_drink_13(data_seperated_d)
data_seperated_d <- knn_impute_moEducation(data_seperated_d)
data_seperated_d <- mode_impute_education(data_seperated_d)


#Basic insights####

source("1_Data_preperation/preliminary_data_analysis.R")


#Obtain weighted summary statistics#####
library(radiant.data)
weightvector_1 <- data_seperated_d$controls$sampleWeight
treated_1 <- which(data_seperated_d$treatment == 1)
non_treated_1 <- which(data_seperated_d$treatment == 0)
weighted_means_1 <-
  c(
    "sex tr" = weighted.mean(data_seperated_d$moderators$sex[treated_1], weightvector_1[treated_1]),
    "age tr" = weighted.mean(data_seperated_d$moderators$age[treated_1], weightvector_1[treated_1]),
    "syBP tr" = weighted.mean(data_seperated_d$outcomes$syBP_mean[treated_1], weightvector_1[treated_1]),
    "BMI tr"  = weighted.mean(data_seperated_d$outcomes$BMI[treated_1], weightvector_1[treated_1]),
    "waist tr" = weighted.mean(data_seperated_d$outcomes$waist[treated_1], weightvector_1[treated_1]),
    "grip tr" = weighted.mean(data_seperated_d$outcomes$gripstrength[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "doctor tr" = weighted.mean(data_seperated_d$controls$timeseendoctor[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "sex nt" = weighted.mean(data_seperated_d$moderators$sex[non_treated_1], weightvector_1[non_treated_1]),
    "age nt" = weighted.mean(data_seperated_d$moderators$age[non_treated_1], weightvector_1[non_treated_1]),
    "syBP nt" = weighted.mean(data_seperated_d$outcomes$syBP_mean[non_treated_1], weightvector_1[non_treated_1]),
    "BMI nt"  = weighted.mean(data_seperated_d$outcomes$BMI[non_treated_1], weightvector_1[non_treated_1]),
    "waist nt" = weighted.mean(data_seperated_d$outcomes$waist[non_treated_1], weightvector_1[non_treated_1]),
    "grip nt" = weighted.mean(data_seperated_d$outcomes$gripstrength[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "doctor nt" = weighted.mean(data_seperated_d$controls$timeseendoctor[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "grip white" = weighted.mean(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "grip black" = weighted.mean(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "grip other" = weighted.mean(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "grip male" = weighted.mean(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "grip female" = weighted.mean(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE),
    "doctor white" = weighted.mean(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "doctor black" = weighted.mean(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "doctor other" = weighted.mean(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "doctor male" = weighted.mean(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "doctor female" = weighted.mean(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE)
  )
weighted_sd_1 <-
  c(
    "sex tr" = weighted.sd(data_seperated_d$moderators$sex[treated_1], weightvector_1[treated_1]),
    "age tr" = weighted.sd(data_seperated_d$moderators$age[treated_1], weightvector_1[treated_1]),
    "syBP tr" = weighted.sd(data_seperated_d$outcomes$syBP_mean[treated_1], weightvector_1[treated_1]),
    "BMI tr"  = weighted.sd(data_seperated_d$outcomes$BMI[treated_1], weightvector_1[treated_1]),
    "waist tr" = weighted.sd(data_seperated_d$outcomes$waist[treated_1], weightvector_1[treated_1]),
    "grip tr" = weighted.sd(data_seperated_d$outcomes$gripstrength[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "doctor tr" = weighted.sd(data_seperated_d$controls$timeseendoctor[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "sex nt" = weighted.sd(data_seperated_d$moderators$sex[non_treated_1], weightvector_1[non_treated_1]),
    "age nt" = weighted.sd(data_seperated_d$moderators$age[non_treated_1], weightvector_1[non_treated_1]),
    "syBP nt" = weighted.sd(data_seperated_d$outcomes$syBP_mean[non_treated_1], weightvector_1[non_treated_1]),
    "BMI nt"  = weighted.sd(data_seperated_d$outcomes$BMI[non_treated_1], weightvector_1[non_treated_1]),
    "waist nt" = weighted.sd(data_seperated_d$outcomes$waist[non_treated_1], weightvector_1[non_treated_1]),
    "grip nt" = weighted.sd(data_seperated_d$outcomes$gripstrength[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "doctor nt" = weighted.sd(data_seperated_d$controls$timeseendoctor[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "grip white" = weighted.sd(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "grip black" = weighted.sd(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "grip other" = weighted.sd(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "grip male" = weighted.sd(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "grip female" = weighted.sd(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE),
    "doctor white" = weighted.sd(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "doctor black" = weighted.sd(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "doctor other" = weighted.sd(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "doctor male" = weighted.sd(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "doctor female" = weighted.sd(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE)
    
  )

library(gamboostLSS)
weighted_median_1 <- 
  
  c(
    "sex tr" = weighted.median(data_seperated_d$moderators$sex[treated_1], weightvector_1[treated_1]),
    "age tr" = weighted.median(data_seperated_d$moderators$age[treated_1], weightvector_1[treated_1]),
    "syBP tr" = weighted.median(data_seperated_d$outcomes$syBP_mean[treated_1], weightvector_1[treated_1]),
    "BMI tr"  = weighted.median(data_seperated_d$outcomes$BMI[treated_1], weightvector_1[treated_1]),
    "waist tr" = weighted.median(data_seperated_d$outcomes$waist[treated_1], weightvector_1[treated_1]),
    "grip tr" = weighted.median(data_seperated_d$outcomes$gripstrength[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "doctor tr" = weighted.median(data_seperated_d$controls$timeseendoctor[treated_1], weightvector_1[treated_1], na.rm = TRUE),
    "sex nt" = weighted.median(data_seperated_d$moderators$sex[non_treated_1], weightvector_1[non_treated_1]),
    "age nt" = weighted.median(data_seperated_d$moderators$age[non_treated_1], weightvector_1[non_treated_1]),
    "syBP nt" = weighted.median(data_seperated_d$outcomes$syBP_mean[non_treated_1], weightvector_1[non_treated_1]),
    "BMI nt"  = weighted.median(data_seperated_d$outcomes$BMI[non_treated_1], weightvector_1[non_treated_1]),
    "waist nt" = weighted.median(data_seperated_d$outcomes$waist[non_treated_1], weightvector_1[non_treated_1]),
    "grip nt" = weighted.median(data_seperated_d$outcomes$gripstrength[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "doctor nt" = weighted.median(data_seperated_d$controls$timeseendoctor[non_treated_1], weightvector_1[non_treated_1], na.rm = TRUE),
    "grip white" = weighted.median(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "grip black" = weighted.median(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "grip other" = weighted.median(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "grip male" = weighted.median(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "grip female" = weighted.median(data_seperated_d$outcomes$gripstrength[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE),
    "doctor white" = weighted.median(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "1.white/caucasian")], weightvector_1[which(data_seperated_d$moderators$race == "1.white/caucasian")], na.rm = TRUE),
    "doctor black" = weighted.median(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "2.black/african american")], weightvector_1[which(data_seperated_d$moderators$race == "2.black/african american")], na.rm = TRUE),
    "doctor other" = weighted.median(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$race == "3.other")], weightvector_1[which(data_seperated_d$moderators$race == "3.other")], na.rm = TRUE),
    "doctor male" = weighted.median(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 1)], weightvector_1[which(data_seperated_d$moderators$sex == 1)], na.rm = TRUE),
    "doctor female" = weighted.median(data_seperated_d$controls$timeseendoctor[which(data_seperated_d$moderators$sex == 0)], weightvector_1[which(data_seperated_d$moderators$sex == 0)], na.rm = TRUE)
    
  )
