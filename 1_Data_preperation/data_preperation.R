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
      pw_syBPM1 = ni859, #Outcome variables 2014
      pw_syBPM2 = ni864,
      pw_syBPM3 = ni869,
      wLbs_pw = ni841,
      hInc_pw = ni834,
      waist_pw = ni907,
      everSmoke = pc116, #Control and moderator variables
      smokenow = pc117,
      nSmokenow = pc118,
      nSmokepw = nc118,
      nSmokemos = pc123,
      nDDrink = pc129,
      nDDrink_pw = nc129,
      nGDrink = pc130,
      nGDrink_pw = nc130,
      jobStat.A1 = pj005m1,
      jobStat.A2 = pj005m2,
      jobStat.A3 = pj005m3,
      jobStat.A4 = pj005m4,
      jobStat.A5 = pj005m5,
      vigAct = pc223,
      modAct = pc224,
      milAct = pc225,
      mStat11 = r11mstat,
      mStat13 = r13mstat,
      smoker11 = r11smoken,
      smoker13 = r13smoken,
      drinker11 = s11drink,
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
    
    #Outcome variables are all already double format - but need to set unavailble measurements to NA, 993 998 and 999 are not filled in answers, but we also take out unrealistic measurements to be sure 
    for (i in c("syBPM1", "syBPM2", "syBPM3", "pw_syBPM1", "pw_syBPM2", "pw_syBPM3", "wLbs", "wLbs_pw")){
      #Select variable that needs to be changed
      variable <- formatted_data[[i]]
      
      for (observation in 1:dim(formatted_data)[1]) {
        if(!is.na(variable[observation])){
          if(variable[observation] > 500){
            variable[observation] <- NA
          }
        }
      }
      
      
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
      
    }
    
    for (i in c("hInc", "hInc_pw", "waist", "waist_pw")){ #Out of Range measurements (denoted by 99-190) are currently denoted by NA
      #Select variable that needs to be changed
      variable <- formatted_data[[i]]
      
      for (observation in 1:dim(formatted_data)[1]) {
        if(!is.na(variable[observation])){
          if(variable[observation] > 95){
            variable[observation] <- NA
          }
        }
      }
      
      #Put it in the formatted_data
      formatted_data[[i]] <- variable
      
    }
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
    for (i in c("nSmokenow", "nSmokepw", "nSmokemos", "nDDrink", "nDDrink_pw")) {
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
      "mStat11",
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
#' Output: @return extended_data ... data list with variables for racial discrimination accorindg to user input
#' Input: @param data_cleaned .... cleaned data as obtained from general_data_preperation function
#' 
define_variables <-
  function(data_cleaned = data) {
    #Load in package
    library(dplyr)
    
    extended_data <- data_cleaned
    
    #First we need to do rowwise operations
    extended_data_R <- rowwise(extended_data, hhidpn)
    
    
    #Construct outcome variables of interest
    extended_data_R <-
      mutate(extended_data_R, syBP_mean = mean(c_across(starts_with("syBPM"))))
    extended_data_R <-
      mutate(extended_data_R, pw_syBP_mean = mean(c_across(starts_with("pw_SyBPM"))))
    extended_data_R <-
      mutate(extended_data_R, BMI = wLbs / (hInc ^ 2) * 703) #703 is correction factor for inches and pounds
    extended_data_R <- 
      mutate(extended_data_R, BMI_pw = wLbs_pw / (hInc_pw ^2) * 703) #703 is correction factor for inches and pounds
    
    #Construct delta outcomes
    extended_data_R <- 
      mutate(extended_data_R, d_syBP_mean = syBP_mean - pw_syBP_mean)
    extended_data_R <- 
      mutate(extended_data_R, d_BMI = BMI - BMI_pw)
    extended_data_R <- 
      mutate(extended_data_R, d_waist = waist - waist_pw)
    
    #Construct control variables of interest
    
    #number of glasses per drinking day in last 3 months is 0 when someone drinks 0 days in last 3 months
    extended_data_R <-
      mutate(extended_data_R, nGDrink = ifelse(nDDrink == 0 , 0, nGDrink))
    extended_data_R <-
      mutate(extended_data_R, nGDrink_pw = ifelse(nDDrink_pw == 0 , 0, nGDrink_pw))
    
    #Create interaction variable: glasses alchol drank per week last 3 months
    extended_data_R <-
      mutate(extended_data_R, nDrinkPerWeek = ifelse(!is.na(nGDrink) & !is.na(nDDrink), nGDrink * nDDrink, NA))
    extended_data_R <-
      mutate(extended_data_R, nDrinkPerWeek_pw = ifelse(!is.na(nGDrink_pw) & !is.na(nDDrink_pw), nGDrink_pw * nDDrink_pw, NA))
    
    #number of cigarettes is 0 when someone says he/she is not a smoker (instead of NA)
    extended_data_R <-
      mutate(extended_data_R, nSmokenow = ifelse(smokenow == 0, 0, nSmokenow))
    extended_data_R <-
      mutate(extended_data_R, nSmokemos = ifelse(everSmoke == 0, 0, nSmokemos))
    extended_data_R <-
      mutate(extended_data_R, nSmokepw = ifelse(everSmoke == 0, 0, nSmokepw))
    
    #simplification of retirement status, workingNow dummy if someone is working or not
    extended_data_R <-
      mutate(
        extended_data_R,
        workingNow = ifelse(
          jobStat.A1 == "1.working now" & !is.na(jobStat.A1) |
            jobStat.A2 == "1.working now" &
            !is.na(jobStat.A2) |
            jobStat.A3 == "1.working now" &
            !is.na(jobStat.A3) |
            jobStat.A4 == "1.working now" &
            !is.na(jobStat.A4) |
            jobStat.A5 == "1.working now" &
            !is.na(jobStat.A5)
          ,
          1,
          0
        )
      )
    
    #Impute wealth from previous wave if missing now and make bins of wealth based on quantiles
    extended_data_R <-
      mutate(extended_data_R,
             wealthCalc = ifelse(!is.na(wealthNotImputed), wealthNotImputed, wealthImputed))
    
    #Major life events
    extended_data_R <-
      mutate(extended_data_R, quit_smoking = ifelse((smoker11 == 1 &
                                                       smoker13 == 0), 1, 0))
    extended_data_R <-
      mutate(extended_data_R, started_smoking = ifelse((smoker13 == 1 &
                                                          smoker11 == 0), 1, 0))
    
    extended_data_R <-
      mutate(extended_data_R, quit_drinking = ifelse((drinker11 == 1 &
                                                        drinker13 == 0), 1, 0))
    extended_data_R <-
      mutate(extended_data_R, started_drinking = ifelse((drinker13 == 1 &
                                                           drinker11 == 0), 1, 0))
    
    extended_data_R <-
      mutate(extended_data_R, divorced_or_seperated = ifelse(
        (
          mStat11 == "1.married" |
            mStat11 == "2.married, spouse absent" | mStat11 == "3.partnered"
        )
        &
          (
            mStat13 == "4.seperated" |
              mStat13 == "5.divorced" | mStat13 == "6.seperated/divorced"
          ),
        1,
        0
      ))
    extended_data_R <-
      mutate(extended_data_R, widowed = ifelse(
        (
          mStat11 == "1.married" |
            mStat11 == "2.married, spouse absent" | mStat11 == "3.partnered"
        )
        &
          mStat13 == "7.widowed",
        1,
        0
      ))
    extended_data_R <-
      mutate(extended_data_R, recently_married_or_partnered = ifelse(
        (
          mStat13 == "1.married" |
            mStat13 == "2.married, spouse absent" | mStat13 == "3.partnered"
        )
        &
          (
            mStat11 == "4.seperated" |
              mStat11 == "5.divorced" |
              mStat11 == "6.seperated/divorced" |
              mStat11 == "7.widowed" | mStat11 == "8.never married"
          ),
        1,
        0
      ))
    
    extended_data_R <-
      mutate(extended_data_R, quit_working = ifelse(
        (
          prevRetStat == "3.partially retired" |
            prevRetStat == "5.not retired"
        ) & ((jobStat.A1 == "5.retired" & !is.na(jobStat.A1))
             |
               (jobStat.A2 == "5.retired" & !is.na(jobStat.A2)) |
               (jobStat.A3 == "5.retired" &
                  !is.na(jobStat.A3)) |
               (jobStat.A4 == "5.retired" &
                  !is.na(jobStat.A4)) |
               (jobStat.A5 == "5.retired" &
                  !is.na(jobStat.A1))
        ),
        1,
        0
      ))
    #Wealth bins
    
    #To make bins we need to go back to non-row-wise data
    extended_data <- ungroup(extended_data_R)
    extended_data <-
      mutate(extended_data, wealth_bin = ntile(wealthCalc, 10)) #make 10 wealth bins -- still need to make ordered factor from this
    extended_data$wealth_bin <-
      factor(extended_data$wealth_bin, order = TRUE)
    
    return(extended_data)
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