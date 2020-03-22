# File name: annuity.fv.R
# Functions in this program estimates future value of annuity.

# annuity_fv()
##################
# inputs:
# curr_dt -> current date
# curr_prin -> Principle amount at current date
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# contrib_by_prd -> amount added to principle each period
# contrib_incr_rate -> % increase to contrib_by_prd each period
# intrst_rate_by_prd -> assumed interest rate by period (unit agnostic)
# Note:  Make sure the units match between prd_cnt, contrib_by_prd, contrib_incr_rate and intrst_rate_by_prd

# output:
# dataframe reflecting the accumulation schedule of annuity

annuity_fv <- function(curr_dt, curr_prin=0, prd_cnt, contrib_by_prd=0, contrib_incr_rate=0, intrst_rate_by_prd=0){
  
  curr_dt <- as.Date(curr_dt, '%m/%d/%Y')
  dt_list <- as.numeric()
  class(dt_list) <- 'Date'
  
  prin <- curr_prin * (1+intrst_rate_by_prd)^(0:(prd_cnt-1))
  
  contrib_table <- data.frame(matrix(vector(), nrow=prd_cnt))
  for(i in (0:(prd_cnt-1))){
    curr_contrib <- contrib_by_prd * (1 + contrib_incr_rate)^i
    assign(paste0('contrib_',i), c(rep(0,i), curr_contrib * (1+intrst_rate_by_prd)^(0:(prd_cnt-1-i))))
    contrib_table <- cbind.data.frame(contrib_table, get(paste0('contrib_',i)))
    names(contrib_table)[ncol(contrib_table)] <- paste0('contrib_',i)
    
    dt_var <- curr_dt %m+% months(i)
    dt_list <- c(dt_list, dt_var)
    
  }
  
  accum_table <- cbind.data.frame('date'=dt_list, 'init_prin'=prin, contrib_table)
  accum_table$tot <- rowSums(accum_table[,-1])
  
  return(accum_table)
  
}


# annuity_fv_v2()
##################
# inputs:
# curr_dt -> current date
# curr_prin -> Principle amount at current date
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# contrib_by_prd -> amount added to principle each period
# contrib_incr_rate -> % increase to contrib_by_prd each period
# intrst_rate_vector -> a vector of equal length to prd_cnt reflecting different interest rates by period (unit agnostic)
# Note:  Make sure the units match between prd_cnt, contrib_by_prd, contrib_incr_rate and intrst_rate_vector

# output:
# dataframe reflecting the accumulation schedule of annuity

annuity_fv_v2 <- function(curr_dt, curr_prin, prd_cnt, contrib_by_prd, contrib_incr_rate, intrst_rate_vector){
  
  curr_dt <- as.Date(curr_dt, '%m/%d/%Y')
  dt_list <- as.numeric()
  class(dt_list) <- 'Date'
  
  intrst_rate_vector_filtrd <- c(1, head(intrst_rate_vector, -1))
  cum_intrst_rates <- cumprod(intrst_rate_vector_filtrd)
  
  prin <- curr_prin * cum_intrst_rates
  
  contrib_table <- data.frame(matrix(vector(), nrow=prd_cnt))
  for(i in (0:(prd_cnt-1))){
    curr_contrib <- contrib_by_prd * (1 + contrib_incr_rate)^i
    assign(paste0('contrib_',i), c(rep(0,i), curr_contrib * cum_intrst_rates[0:(prd_cnt-i)]))
    contrib_table <- cbind.data.frame(contrib_table, get(paste0('contrib_',i)))
    names(contrib_table)[ncol(contrib_table)] <- paste0('contrib_',i)
    
    dt_var <- curr_dt %m+% months(i)
    dt_list <- c(dt_list, dt_var)
    
  }
  
  accum_table <- cbind.data.frame('date'=dt_list, 'init_prin'=prin, contrib_table)
  accum_table$tot <- rowSums(accum_table[,-1])
  
  return(accum_table)
  
}