###############################
# 3_user_def_funcs_load.R
###############################

# Purpose
########################################################################
# Initialize user defined functions for workflow.


#**********************************************************************************************************************************


# First load packages and initialize necessary inputs and directory paths
source('code/1_libraries_load.R')
source('code/2_initial_inputs.R')


# Initialize user define functions
########################################################################

# Below are functions that help calculate average returns from S&P data over a period of time.
source('code/z_user_def_funcs/rows_extract.R')  # Function extracts consecutive rows, with specified starting point, from data. 
source('code/z_user_def_funcs/rand_rows_extract.R')  # Function extracts consecutive rows, at random, from data.
source('code/z_user_def_funcs/return_scenario_gen.R')  # Function extracts best/worst case monthly return scenarios over specified time period.
source('code/z_user_def_funcs/rand_return_scenario_gen.R')  # Function extract random n-month return scenario.

source('code/z_user_def_funcs/annuity_fv.R')  # Function projects future value of a typical retirement portfolio.
source('code/z_user_def_funcs/amort_sched.R')  # Function creates amortization/distribution schedule for retirement portfolio.
