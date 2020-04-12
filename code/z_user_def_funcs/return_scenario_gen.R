###############################
# return_scenario_gen.R
###############################

# Purpose
########################################################################
# This function takes historical market return data and returns
# optimistic/pessimistic market returns over a specified period of time.

# Considerations
########################################################################
# For financial planning, it is normal to assume an average market 
# return. (5-7%) However, this assumption is derived over a long period
# of time. Given retirement planning offers a finite time horizon, it
# is good to assess best/worst case market returns over a relevant
# period of time based on market history. This function accomplishes that.
#
# Reference Info: https://towardsdatascience.com/monte-carlo-simulation-in-r-with-focus-on-financial-data-ad43e2a4aedf

# Parameters
########################################################################
# return_data -> Market return data, specifically monthly S&P data from yahoo.com
# n -> number of periods to review. Assumed to be 12 by default.
# perc_rng -> A numerical vector specifying the percentiles to extract as worst/best case. If value not provided, absolute worst/best case provided.
#             Vector is the form of c(a, b).  a=lower percentile,  b=higher percentile

# Output
########################################################################
# Best/worst case monthly return scenarios based on the return data
# provided and the period length specified.


#**********************************************************************************************************************************


return_scenario_gen <- function(return_data, n=12, perc_rng=NULL){
  
  # Resetting column names for input data
  setnames(return_data, names(return_data))
  
  
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
  
  
  # calculate average returns over different time frames 
  ##############################################################################
  
  # Specify number of possible scenarios in data set.
  # Example: If there are 500 months (records) in the data set, and we want to assess
  # return scenarios for 12-month periods, then there should be 500-(12-1) possible scenarios.
  # First Scenario would be records 1:12. Last scenario would be records 489:500.
  scenario_cnt <- nrow(return_data) - (n-1)  
  
  
  # Once scenario count is determined, we iterate through all those scenarios.
  
  scenario_id <- numeric()
  avg_return_list <- numeric()
  
  for(i in c(1:scenario_cnt)){
    
    # Create vector of scenario ids for reference.
    scenario_id <- c(scenario_id, i)
    
    # Filter records from input data based on starting row and length specifications
    filtrd_data <- rows_extract(return_data, i, n)
    
    # Use the filtered returns to calculate average annual return 
    avg_return <- prod(filtrd_data$return_fctr)^(1/(nrow(filtrd_data)/12))
    
    # Create vector tracking average annul returns for each scenario
    avg_return_list <- c(avg_return_list, avg_return)
    
  }
  
  # Create data frame to track the average return of iterated scenarios
  avg_return_table <- cbind.data.frame(scenario_id, avg_return_list)
  names(avg_return_table) <- c('scenario_id', 'avg_return')
  

  # Extract Optimistic / Pessimistic average return scenarios
  ###############################################################################
  # Once we have average returns for all scenarios from above steps, we can
  # id the best/worst case scenarios.
  
  if(is.null(perc_rng)){
    
    # If perc_rng parameter is not provided, we'll extract scenarios with the
    # absolute highest and lowest average returns.
    
    # From avg_return_table, return row numbers of the max and min avg returns.
    max_index <- which(avg_return_table$avg_return==max(avg_return_table$avg_return))
    min_index <- which(avg_return_table$avg_return==min(avg_return_table$avg_return))
    
    # Note that given the way the previou loop was set up, the row numbers in
    # avg_return_table also correspond with starting rows in the input return data,
    # for each avg return scenario. Thus, we can use the corresponding row numbers
    # to extract return scenarios from input data.
    opti_return <- return_data$return_fctr[max_index : (max_index+n-1)]
    pess_return <- return_data$return_fctr[min_index : (min_index+n-1)]
    
  }else{
    
    # It's possible we may not want the absolute best/worst case return scenarios,
    # as those are often outliers unlikely to happen again. Maybe we're looking for
    # optimistic/conservative scenarios likely to happen again. In this situation,
    # we want to look at percentiles. Example, return the 90th percentile return scenario (optimistic)
    # and the 10th percentile return scenario. (pessimistic)
    
    # If perc_rng paramter IS provided, then first calculate the corresponding avg returns of the
    # specified percentiles, using avg_return_table. Then, find the return scenario that is closest
    # to the avg returns of the specified percentiles, return the corresponding row numbers
    # in avg_return_table.
    high_perc_index <- which.min(abs(avg_return_table$avg_return - as.numeric(quantile(avg_return_table$avg_return, perc_rng[2]))))
    low_perc_index <- which.min(abs(avg_return_table$avg_return - as.numeric(quantile(avg_return_table$avg_return, perc_rng[1]))))
    
    # Note that given the way the previou loop was set up, the row numbers in
    # avg_return_table also correspond with starting rows in the input return data,
    # for each avg return scenario. Thus, we can use the corresponding row numbers
    # to extract return scenarios from input data.
    opti_return <- return_data$return_fctr[high_perc_index : (high_perc_index+n-1)]
    pess_return <- return_data$return_fctr[low_perc_index : (low_perc_index+n-1)]
    
  }
  
  return(list(opti_return, pess_return))
  
}
