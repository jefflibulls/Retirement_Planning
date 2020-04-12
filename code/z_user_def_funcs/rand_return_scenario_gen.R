###############################
# rand_return_scenario_gen.R
###############################

# Purpose
########################################################################
# This function takes historical market return data and returns
# average market return over a specified period of time, at random.

# Considerations
########################################################################
# For financial planning, it is normal to assume an average market 
# return. (5-7%) However, this assumption is derived over a long period
# of time. Given retirement planning offers a finite time horizon, it
# is good to simulate market returns over a relevant
# period of time based on market history. This function sets up
# simulation by randomly generating historical timelines and their
# market returns.
#
# Reference Info: https://towardsdatascience.com/monte-carlo-simulation-in-r-with-focus-on-financial-data-ad43e2a4aedf

# Parameters
########################################################################
# return_data -> Market return data, specifically monthly S&P data from yahoo.com
# n -> number of periods to review. Assumed to be 12 by default.

# Output
########################################################################
# Monthly Return scenario based on a random n-period timeline in the
# return data.


#**********************************************************************************************************************************


rand_return_scenario_gen <- function(return_data, n=12){
  
  
  # Calculate average returns for each record
  ##############################################################################
  
  # Creating a next day open column in order to compare to
  # actual open when calculating returns.
  # This is more accurate than comparing open vs close because
  # it accounts for after market movement.
  return_data$next_day_open <- c(return_data$Open[2:nrow(return_data)],
                                    return_data$Close[nrow(return_data)])
  
  return_data %<>% mutate(return=next_day_open/Open-1,
                             return_fctr=next_day_open/Open)
  
  
  # Extract random return scenario from data
  ###############################################################################
  
  # Pull out a historical n-month period at random
  rand_data <- rand_rows_extract(return_data, n)
  
  
  return(rand_data$return_fctr)
  
}
