#' data_seperator
#'
#'
#' @description   This function retreieves the seperate matrices form the data that need to be used for further analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return data_list ... List with data in the rigt matrices 
#' Input: @param raw_data .... Raw data as obtained from data_loader function
#' @param control_variables ... column numbers of control variables
#' @param moderator_variables ... column numbers of moderator variables
#' @param treatment_variables ... column numbers of treatment variables
#' @param outcomes .... column numbers of outcome variables
data_seperator <- function(raw_data = unseperated_data, control_columns = NA, moderator_columns = NA, treatment_columns = NA, outcome_columns = NA){
  
  #Make seperate dataframes
  if(!is.null(control_columns)){
    control_variables <- raw_data[, control_columns]
  }else{
    control_variables <- NA
  }
  
  if(!is.null(moderator_columns)){
    moderator_variables <- raw_data[, moderator_columns]
  }else{
    moderator_variables <- NA
  }
  
  if(!is.null(treatment_columns)){
    treatment <- raw_data[, treatment_columns]
  }else{
    treatment <- NA
  }
  
  if(!is.null(outcome_columns)){
    outcomes <- raw_data[, outcome_columns]
  }else{
    outcomes <- NA
  }
  
  #Return list 
  data_list <- list("controls" = control_variables, "moderators" = moderator_variables, "treatment" = treatment, "outcomes" = outcomes)
  return(data_list)
  #Seperate treatment, outcomes and control variables - do not seperate moderating variables out yet
  
  #Make sure certain variables are factors (level variables)
  
  #Make sure that the multiple choice variables (q29 and q30 become one variable)
  
  #Make sure it in the right data format(dataframe?)
}

#' variable_rename
#' 
#' @description This function gives interpretable names to the data pulled from the HRS study
#' Output:
#' @return renamed_data
#' 
#' Input:
#' @param non_named_data.... list of dataframe of non-formatted data to be formatted
#'
variable_rename <- function(non_named_data = NA){
  library(dplyr)
  
    renamed_data <- non_named_data %>%
    rename(
      syBPM1 = pi859, #Outcome variables
      syBPM2 = pi864,
      syBPM3 = pi869,
      wLbs = pi841,
      hInc = pi834,
      waist = pi907,
      everSmoke = pc116, #Control and moderator variables
      smokenow = pc117,
      nSmokenow = pc118,
      nSmokemos = pc123,
      nDrink = pc129,
      jobStat.A1 = pj005m1,
      jobStat.A2 = pj005m2,
      jobStat.A3 = pj005m3,
      jobStat.A4 = pj005m4,
      jobStat.A5 = pj005m5,
      vigAct = pc223,
      modAct = pc224,
      milAct = pc225,
      mStat12 = r12mstat,
      mStat13 = r13mstat,
      smoker12 = r12smoken,
      smoker13 = r13smoken,
      drinker12 = s12drink,
      drinker13 = s13drink,
      prevRetStat = pz134,
      race = raracem,
      sex = px060_r,
      education = raeduc,
      moEducation = rameduc,
      age = pa019,
      married = pa026,
      wealthImputed = px092,
      wealthNotImputed = pz266,
      q29.A1 = plb029a, #Treatment
      q29.A2 = plb029b,
      q29.A3 = plb029c,
      q29.A4 = plb029d,
      q29.A5 = plb029e,
      q29.A6 = plb029f,
      rDisc1 = plb030m1,
      rDisc2 = plb030m2,
      rDisc3 = plb030m3,
      rDisc4 = plb030m4,
      rDisc5 = plb030m5,
      rDisc6 = plb030m6,
      rDisc7 = plb030m7,
      rDisc8 = plb030m8,
      rDisc9 = plb030m9,
      rDisc10 = plb030m10
    )
    
    return(renamed_data)
}


#' data_formatter
#' 
#' @description This function formats variables for analysis. Custom-made for master thesis analysis. Only formats control, moderators and outcomes, not treatment
#' 
#' Output:
#' @return formatted_data
#' 
#' Input:
#' @param non_formatted_data .... list of dataframe of non-formatted data to be formatted
#'
data_formatter <- function(non_formatted_data = NA) {
  library(tidyverse)
  
  #Check if input is provided
  if (!is.null(non_formatted_data)) {
    #Create new datamatrix to store formatted data in
    formatted_data <- non_formatted_data
    
    #Outcome variables are all already double format
    #Control variables
    
    #Binary variables - Change answers to dummy by setting NO = 0, YES = 1, DK, NA AND RF  = NA
    for (i in c("everSmoke", "smokenow", "married")) {
      #Select the variable that needs to be changed
      variable <- formatted_data[[i]]
      
      for (observation in 1:dim(formatted_data)[1]) {
        if (!is.na(variable[observation])) {
          if (variable[observation] == 5) {
            variable[observation] <- 0
            
          } else if (variable[observation] == 8 |
                     variable[observation] == 9) {
            variable[observation] <- NA
          }
          
        }
      }
      
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
    }
    
    #Binary variables that have been cocded not in 1 and 0's
    for (i in c("sex")) {
      #Select the variable that needs to be changed
      variable <- formatted_data[[i]]
      
      for (observation in 1:dim(formatted_data)[1]) {
        if (!is.na(variable[observation])) {
          if (variable[observation] == 2) {
            variable[observation] <- 0
            
          }
          
        }
      }
      
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
    }
    
    
    #Numerical/Double variables
    for (i in c("nSmokenow", "nSmokemos", "nDrink")) {
      #Select the variable that needs to be changed
      variable <- formatted_data[[i]]
      
      for (observation in 1:dim(formatted_data)[1]) {
        if (!is.na(variable[observation])) {
          if (variable[observation] == 98 |
              variable[observation] == 99) {
            variable[observation] <- NA
          }
          
        }
      }
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
    }
    
    #Factors - simple
    for (i in c(
      "mStat12",
      "mStat13",
      "race",
      "jobStat.A1",
      "jobStat.A2",
      "jobStat.A3",
      "jobStat.A4",
      "jobStat.A5"
    )) {
      #Select the variable that needs to be changed
      variable <- formatted_data[[i]]
      variable <- as_factor(variable)
      
      if (i %in% c("jobStat.A1",
                   "jobStat.A2",
                   "jobStat.A3",
                   "jobStat.A4",
                   "jobStat.A5")) {
        #Change factor levels
        variable <- recode_factor(
          variable,
          "1" = "1.working now",
          "2" = "2.unemployed and looking for work",
          "3" = "3.temporarily laid off",
          "4" = "4.disabled",
          "5" = "5.retired",
          "6" = "6.homemaker",
          "7" = "7.other",
          "8" = "8.on sick or leave",
          "98" = "98.DK",
          "99" = "99.RF"
        )
        #Set Don't knows and refusals to NA
        for (observation in 1:dim(formatted_data)[1]) {
          if (!is.na(variable[observation])) {
            if (variable[observation] == "98.DK" |
                variable[observation] == "99.RF") {
              variable[observation] <- NA
            }
          }
        }
      }
      
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
      
    }
    
    #Ordinal factors - activity
    for (i in c("vigAct", "modAct", "milAct"))
    {
      #Select the variable that needs to be changed
      variable <- formatted_data[[i]]
      variable <- as_factor(variable)
      
      #Change factor levels - low factor = high activity
      variable <- recode_factor(
        variable,
        "7" = "7.every day",
        "1" = "1.more than once a week",
        "2" = "2.once a week",
        "3" = "3.one to three times a month",
        "4" = "4.hardly ever or never",
        "8" = "8.DK",
        "9" = "9.RF",
        .ordered = TRUE
      )
      
      #Set Don't knows and refusals to NA
      for (observation in 1:dim(formatted_data)[1]) {
        if (!is.na(variable[observation])) {
          if (variable[observation] == "98.DK" |
              variable[observation] == "99.RF") {
            variable[observation] <- NA
          }
        }
      }
      #Put formatted variable in formatted data
      formatted_data[[i]] <- variable
    }
    
    #Ordinal factor - education
    for (i in c("education"))
    {
      #Select the variable that needs to be changed (education already has labels on it, ordered automatically - low factor = low education)
      variable <- formatted_data[[i]]
      variable <-
        as_factor(variable, levels = "default", ordered = TRUE)
      
      #Put formatted variable in formatted data
      formatted_data[[i]] <- variable
    }
    
    for (i in c("prevRetStat"))
    {
      #Select the variable that needs to be changed (education already has labels on it, ordered automatically - low factor = low education)
      variable <- formatted_data[[i]]
      variable <- as_factor(variable)
      
      #Recode levels, high factor is now retired
      variable <- recode_factor(
        variable,
        "1" = "1.fully retired",
        "3" = "3.partially retired",
        "5" = "5.not retired",
        .ordered = TRUE
      )
      #Put formatted variable in formatted data
      formatted_data[[i]] <- variable
    }
    
    
    #Return formatted data set
    return(formatted_data)
    
  } else{
    (print("No data has been provided"))
  }
}

#' define_racial_discrimination
#'
#'=
#' @description   This function creates variables that were not in the original data
#'
#' Dependency: data_loader.R
#'
#' Output: @return extended_data_df ... data list with variables for racial discrimination accorindg to user input
#' Input: @param data_cleaned .... cleaned data as obtained from general_data_preperation function
#'        @param method .... method used to define racial discrimination. Default is "Proposal" 
define_variables <- function(data = data_cleaned, method = "Proposal"){
  
  #Construct outcome variables of interest
  systolic_bp_mean <- rowMeans(data_cleaned[,c("pi859", "pi864", "pi869")])
  BMI <- data_cleaned[, "pi841"] / (data_cleaned[, "pi834"] ^ 2) * 703 #703 is correction factor for inches and pounds
  
  # 
  #
  
  #Major life events
  quit_smoking  
  quit_drinking 
  divorced 
  widowed
  
  
  #Construct 
  
  
}

#' define_racial_discrimination
#'
#'=
#' @description   This function puts the data in the right format to be used for analysis
#'
#' Dependency: data_loader.R
#'
#' Output: @return data_incl_racial_discrimination ... data list with variables for racial discrimination accorindg to user input
#' Input: @param data_formatted .... cleaned data as obtained from general_data_preperation function
#'        @param method .... method used to define racial discrimination. Default is "Proposal" 
define_racial_discrimination <-
  function(data_wo_rd = data_formatted,
           method = "Proposal") {
    #Check if user provided data
    if (!is.null(data_wo_rd)) {
      data_rd_inc <- data_wo_rd
      if (method == "Proposal") {
        #Check whether individual reports having had any experience of discrimination
        
        #FOrmat q29
        for (i in c("q29.A1",
                    "q29.A2",
                    "q29.A3",
                    "q29.A4",
                    "q29.A5",
                    "q29.A6")) {
          variable <- data_rd_inc[[i]]
          variable <- as_factor(variable)
          
          #Change factor levels
          variable <- recode_factor(
            variable,
            "1" = "1.almost everyday",
            "2" = "2.at least once a week",
            "3" = "3.a few times a month",
            "4" = "4.a few times a year",
            "5" = "5.less than once a year",
            "6" = "6.never",
            .ordered = TRUE
          )
          
          #Put formatted version of the variables in the dataframe
          data_rd_inc[[i]] <- variable
        }
        
        data_rd_inc <-
          mutate(data_rd_inc, expDisc = ifelse((
            is.na(q29.A1) &
              is.na(q29.A2) &
              is.na(q29.A3) &
              is.na(q29.A4) &
              is.na(q29.A5) &
              is.na(q29.A6)
          ) ,
          NA,
          ifelse((
            (q29.A1 == "6.never" & !is.na(q29.A1)) &
              (q29.A2 == "6.never" &
                 !is.na(q29.A2)) &
              (q29.A3 == "6.never" &
                 !is.na(q29.A3)) &
              (q29.A4 == "6.never" &
                 !is.na(q29.A4)) &
              (q29.A5 == "6.never" &
                 !is.na(q29.A5)) &
              (q29.A6 == "6.never" &
                 !is.na(q29.A6))
          )
          , 0, 1)
          ))
        
        
        #Check whether individual reports having had any experience of racial discrimination, given that the individual reports experiences of discrimination in general
        
        data_rd_inc$expRacialDisc <- NA
        
        #First format the answer on question 30 as factor such that we can work with it
        for (i in c(
          "rDisc1",
          "rDisc2",
          "rDisc3",
          "rDisc4",
          "rDisc5",
          "rDisc6",
          "rDisc7",
          "rDisc8",
          "rDisc9",
          "rDisc10"
        )) {
          variable <- data_rd_inc[[i]]
          variable <- as_factor(variable)
          
          #Change factor levels
          variable <- recode_factor(
            variable,
            "1" = "1.your ancestry or national origin",
            "2" = "2.your gender",
            "3" = "3.your race",
            "4" = "4.your age",
            "5" = "5.your religion",
            "6" = "6.your weight",
            "7" = "7.a physical disability",
            "8" = "8.an aspect of your physical appearance",
            "9" = "9.your sexual orientation",
            "10" = "10.your financial status",
            "11" = "11.other"
          )
          
          #Put formatted version of the variables in the dataframe
          data_rd_inc[[i]] <- variable
        }
        
        #Fill racial discrimnation variable. For every individual that answers that race AND/OR ancestry/national
        #origin is one of the reasons for experiencing the experiences of q29 set racial discrimination is 1, else 0
        #Only fill if rDisc1 is not NA as question needs to be filled in at all
        data_rd_inc <-
          mutate(data_rd_inc, expRacialDisc = ifelse(is.na(rDisc1), NA, ifelse((
            rDisc1 == "1.your ancestry or national origin" |
              rDisc1 == "3.your race" |
              (
                rDisc2 == "1.your ancestry or national origin" & !is.na(rDisc2)
              ) |
              (rDisc2 == "3.your race" & !is.na(rDisc2)) |
              (
                rDisc3 == "1.your ancestry or national origin" & !is.na(rDisc3)
              ) |
              (rDisc3 == "3.your race" & !is.na(rDisc3)) |
              (
                rDisc4 == "1.your ancestry or national origin" & !is.na(rDisc4)
              ) |
              (rDisc4 == "3.your race" & !is.na(rDisc4)) |
              (
                rDisc5 == "1.your ancestry or national origin" & !is.na(rDisc5)
              ) |
              (rDisc5 == "3.your race" & !is.na(rDisc5)) |
              (
                rDisc6 == "1.your ancestry or national origin" & !is.na(rDisc6)
              ) |
              (rDisc6 == "3.your race" & !is.na(rDisc6)) |
              (
                rDisc7 == "1.your ancestry or national origin" & !is.na(rDisc7)
              ) |
              (rDisc7 == "3.your race" & !is.na(rDisc7)) |
              (
                rDisc8 == "1.your ancestry or national origin" & !is.na(rDisc8)
              ) |
              (rDisc8 == "3.your race" & !is.na(rDisc8)) |
              (
                rDisc9 == "1.your ancestry or national origin" & !is.na(rDisc9)
              ) |
              (rDisc9 == "3.your race" & !is.na(rDisc9)) |
              (
                rDisc10 == "1.your ancestry or national origin" & !is.na(rDisc10)
              ) |
              (rDisc10 == "3.your race" & !is.na(rDisc10))
          )
          ,
          1,
          0
          )))
        
        #Find individuals that report experiences of racial discirmination overall (fill NA's of expRacialDisc with information from expDisc)
        data_rd_inc <-
          mutate(data_rd_inc, expRDAll = ifelse((
            expRacialDisc == 1 &
              !is.na(expRacialDisc) &
              expDisc == 1 &
              !is.na(expDisc)
          ) ,
          1,
          ifelse(!is.na(expDisc), 0, NA)
          ))
        
      }
      
      #Return appended matrix
      return(data_rd_inc)
    } else {
      print("Please input data")
      
    }
  }


#' racial_discrimination_analysis
#' 
#' @description Provides analysis on racial discrimination seperately
#' 
#' Output: @return analysis_racial_discrimination
#' Input: @param data dataframe with racial discrimination data
#' @param method .... method used to do this analysis
#' @param choices ... choices of what analysis to do 
racial_discrimination_analysis <- function(data = racial_discrimination_data, method = "Proposal", choices = "All" ){
  
}

#' general_data_analysis
#' 
#' General data anslysis that provides summary tables and other things if needed
#' 
#' Output: @return general_analysis
#' Input: @param input_data ... input data
#' @param choices ... choices of analysis to do c(means, median etc)
general_data_analysis <- function(input_data = data_to_analyse, choices = "All"){
  
}

#' missing_data_analysis
#' 
#' @description Analysis of missing data and possible imputation if user provided
#' 
#' Output: @return missing_data_analysis
#' Input: @param missing_data ... data with missings
#' @param imputation ... imputation method
missing_data_analysis <- function(missing_data = missing_data, imputation = "Multiple imputation"){
  
}