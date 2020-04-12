###############################
# 1_libraries_load.R
###############################

# Purpose
########################################################################
# Load necessary packages and options for rest of workflow


#**********************************************************************************************************************************


# Turn off scientific notation
options(scipen=999)


## First, load required packages (or install if they're not already) 
pkgs = c("magrittr", 
         "data.table", 
         "dplyr", 
         "FinancialMath", 
         "openxlsx",
         "ggplot2",
         "lubridate") 

for (pkg in pkgs){ 
  if (!require(pkg, character.only = T)){ 
    install.packages(pkg) 
    library(pkg) 
  } 
}