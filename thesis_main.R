#Set working directory
setwd("C:/Users/pimva/Documents/Studie/Thesis/Programming")
set.seed(301297)

#Read in data from stata file (note that this is an extraction of variables from the HRS)
source("0_Data_loading/data_loader.R")
HRS_data_unprepared <- data_loader(filename = "A_Data_sources/merged_data.dta")

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


#Create seperations of data and select which variables are used as control/moderator variables
controlvariables <- c("")
moderatorvariables <- c("")
outcomevariables <- c("")
treatmentvariables <- c("")

data_seperated <- data_seperator(formatted_data, controlvariables, moderatorvariables, treatmentvariables, outcomevariables)

#Basic insights####



#Provide initial insights on the data (missings, distributions etc.) 

#Prepare the data by treating missings, constructing subsets, creating new variables

#Provide insights again, but now with the "cleaned data" 

#Estimate the propensity scores and provide analysis of these estimates (we need to normalize somewhere - also need to oversample propensity scores!)

#Provide insights on the estimated propensity scores

#Estimate the posterior treatment function with BCF - basics

#Evaluation of results

#Estimate posterior treatment function including sensitivity analysis