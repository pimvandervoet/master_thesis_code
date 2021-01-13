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
outcomevariables_2 <- outcomevariables_1
treatmentvariables_2 <- treatmentvariables_1

data_seperated_1 <- data_seperator(analysis_dataset, controlvariables_1, moderatorvariables_1, treatmentvariables_1, outcomevariables_1)
data_seperated_2 <- data_seperator(analysis_dataset, controlvariables_2, moderatorvariables_2, treatmentvariables_2, outcomevariables_2)



#Basic insights####

#Provide initial insights on the data (missings, distributions etc.) 

#Prepare the data by treating missings, constructing subsets, creating new variables

#Provide insights again, but now with the "cleaned data" 

#Estimate the propensity scores and provide analysis of these estimates (we need to normalize somewhere - also need to oversample propensity scores!)

#Provide insights on the estimated propensity scores

#Estimate the posterior treatment function with BCF - basics

#Evaluation of results

#Estimate posterior treatment function including sensitivity analysis