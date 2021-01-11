#data_loader####
#Reads in data from stata file
#Output: @output_data <- R data file
#Input: @filename - location of file in working directory
data_loader <- function(filename = "A_Data_sources/merged_data.dta"){
  
#Load packages recuired in this function
library(haven)

#Read in data from filename
output_data <- read_dta(
  filename,
  encoding = NULL,
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
)

return(output_data)
}