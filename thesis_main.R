#Set working directory
setwd("C:/Users/pimva/Documents/Studie/Thesis/Git/masterthesis")
set.seed(301297)

#Read in data from stata file (note that this is an extraction of variables from the HRS)
source("Data_preperation/data_loader.R")
HRS_data_unprepared <- data_loader(filename = "Data/merged_data.dta")

#Provide initial insights on the data (missings, distributions etc.)

#Prepare the data by treating missings, constructing subsets, creating new variables

#Provide insights again, but now with the "cleaned data" 

#Estimate the propensity scores and provide analysis of these estimates

#Provide insights on the estimated propensity scores

#Estimate the posterior treatment function with BCF - basics

#Evaluation of results

#Estimate posterior treatment function including sensitivity analysis