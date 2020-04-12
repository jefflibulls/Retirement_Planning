###############################
# annuity_fv.R
###############################

# Purpose
########################################################################
# This code calculates the projected future value of a typical 
# retirement portfolio, given specified assumptions.

# Considerations
########################################################################
# There are two versions of the function in this code.
# annuity_fv() - assumes a constant rate of return.
# annuity_fv_v2() - passes in a vector of monthly returns.

# Output
########################################################################
# Both versions of the function return an accumulation table of the
# retirement portfolio, which can be used to calculate projected
# future value of the portfolio.


#**********************************************************************************************************************************


# VERSION 1
########################################################################

# Parameters
########################################################################
# curr_dt -> current date
# curr_prin -> Principle amount of portfolio at current date
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# contrib_by_prd -> amount added to principle each period
# contrib_incr_rate -> % increase to contrib_by_prd each period
# intrst_rate_by_prd -> assumed interest rate by period (unit agnostic)
# Note:  Make sure the units match between prd_cnt, contrib_by_prd, contrib_incr_rate and intrst_rate_by_prd


annuity_fv <- function(curr_dt, curr_prin=0, prd_cnt, contrib_by_prd=0, contrib_incr_rate=0, intrst_rate_by_prd=0){
  
  # Specify the starting date of the projection. Future dates will be generated from here.
  curr_dt <- as.Date(curr_dt, '%m/%d/%Y')
  dt_list <- as.numeric()
  class(dt_list) <- 'Date'
  
  
  # Current principal growth projection
  ##############################################################################################
  # Given current portfolio principle and assumed interest rate, create a schedule of 
  # expected principle growth over the specified period of time.
  
  prin <- curr_prin * (1+intrst_rate_by_prd)^(0:(prd_cnt-1))
  
  
  # Periodical contributions growth projection
  ##############################################################################################
  # A normal retirement portfolio assumes periodical (for example: monthly) contributions.
  # This code section reflects the following:
  # 1) periodical contributions
  # 2) periodical contributions increases
  # 3) expected growth of each contribution
  
  contrib_table <- data.frame(matrix(vector(), nrow=prd_cnt)) # Empty dataframe to hold schedules of contributions
  
  # Loop through all periods within projection timeline
  for(i in (0:(prd_cnt-1))){
    
    curr_contrib <- contrib_by_prd * (1 + contrib_incr_rate)^i  # Estimate contribution amount of current period iteration
    
    # Create a schedule of expected growth of current contribution over specified period of time
    assign(paste0('contrib_',i), c(rep(0,i), curr_contrib * (1+intrst_rate_by_prd)^(0:(prd_cnt-1-i))))
    
    # Add current schedule to dataframe holding all contribution growth schedules
    contrib_table <- cbind.data.frame(contrib_table, get(paste0('contrib_',i)))
    names(contrib_table)[ncol(contrib_table)] <- paste0('contrib_',i)
    
    # Keep track of date of current period iteration, for reference.
    dt_var <- curr_dt %m+% months(i)
    dt_list <- c(dt_list, dt_var)
    
  }
  
  
  # Combine principle and contribution growth schedules
  #############################################################################################
  accum_table <- cbind.data.frame('date'=dt_list, 'init_prin'=prin, contrib_table)
  
  # Calculate totals of principle and contributions. This column will provide
  # total projected portfolio amount
  accum_table$tot <- rowSums(accum_table[,-1])
  
  
  return(accum_table)
  
}



# VERSION 2
########################################################################

# Parameters
########################################################################
# curr_dt -> current date
# curr_prin -> Principle amount at current date
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# contrib_by_prd -> amount added to principle each period
# contrib_incr_rate -> % increase to contrib_by_prd each period
# intrst_rate_vector -> a vector of equal length to prd_cnt reflecting different interest rates by period (unit agnostic)
# Note:  Make sure the units match between prd_cnt, contrib_by_prd, contrib_incr_rate and intrst_rate_vector


annuity_fv_v2 <- function(curr_dt, curr_prin, prd_cnt, contrib_by_prd, contrib_incr_rate, intrst_rate_vector){
  
  # Specify the starting date of the projection. Future dates will be generated from here.
  curr_dt <- as.Date(curr_dt, '%m/%d/%Y')
  dt_list <- as.numeric()
  class(dt_list) <- 'Date'
  
  # Calculate cumulative interest rates by period.
  intrst_rate_vector_filtrd <- c(1, head(intrst_rate_vector, -1))
  cum_intrst_rates <- cumprod(intrst_rate_vector_filtrd)
  
  
  # Current principal growth projection
  ##############################################################################################
  # Given current portfolio principle and specified interest rates, create a schedule of 
  # expected principle growth over the specified period of time.
  
  prin <- curr_prin * cum_intrst_rates
  
  
  # Periodical contributions growth projection
  ##############################################################################################
  # A normal retirement portfolio assumes periodical (for example: monthly) contributions.
  # This code section reflects the following:
  # 1) periodical contributions
  # 2) periodical contributions increases
  # 3) expected growth of each contribution
  
  contrib_table <- data.frame(matrix(vector(), nrow=prd_cnt))  # Empty dataframe to hold schedules of contributions
  
  for(i in (0:(prd_cnt-1))){
    
    curr_contrib <- contrib_by_prd * (1 + contrib_incr_rate)^i  # Estimate contribution amount of current period iteration
    
    # Create a schedule of expected growth of current contribution over specified period of time
    assign(paste0('contrib_',i), c(rep(0,i), curr_contrib * cum_intrst_rates[0:(prd_cnt-i)]))
    
    # Add current schedule to dataframe holding all contribution growth schedules
    contrib_table <- cbind.data.frame(contrib_table, get(paste0('contrib_',i)))
    names(contrib_table)[ncol(contrib_table)] <- paste0('contrib_',i)
    
    # Keep track of date of current period iteration, for reference.
    dt_var <- curr_dt %m+% months(i)
    dt_list <- c(dt_list, dt_var)
    
  }
  
  
  # Combine principle and contribution growth schedules
  #############################################################################################
  accum_table <- cbind.data.frame('date'=dt_list, 'init_prin'=prin, contrib_table)
  
  # Calculate totals of principle and contributions. This column will provide
  # total projected portfolio amount
  accum_table$tot <- rowSums(accum_table[,-1])
  
  
  return(accum_table)
  
}