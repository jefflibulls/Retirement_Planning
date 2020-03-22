# program tests fiscal projection scenarios
# based on different historical return scenarios.
#https://towardsdatascience.com/monte-carlo-simulation-in-r-with-focus-on-financial-data-ad43e2a4aedf


return_scenario_gen <- function(return_data, n=12, perc_rng=NULL){
  
  #source('fiscal_scenarios/rows_extract.R')
  
  #max_min_ind <- TRUE
  #perc_rng <- c(0.05, 0.95)
  
  
  setnames(return_data, names(return_data))
  
  # Creating a next day open column in order to compare to
  # actual open when calculating returns.
  # This is more accurate than comparing open vs close because
  # it accounts for after market movement.
  return_data$next_day_open <- c(return_data$Open[2:nrow(return_data)],
                                    return_data$Close[nrow(return_data)])
  
  return_data %<>% mutate(return=next_day_open/Open-1,
                             return_fctr=next_day_open/Open)
  
  
  # calculate average returns over different timeframes #######################
  
  scenario_cnt <- nrow(return_data) - (n-1)
  
  scenario_id <- numeric()
  avg_return_list <- numeric()
  
  for(i in c(1:scenario_cnt)){
    
    scenario_id <- c(scenario_id, i)
    
    filtrd_data <- rows_extract(return_data, i, n)
    
    avg_return <- prod(filtrd_data$return_fctr)^(1/(nrow(filtrd_data)/12))
    
    avg_return_list <- c(avg_return_list, avg_return)
    
  }
  
  avg_return_table <- cbind.data.frame(scenario_id, avg_return_list)
  names(avg_return_table) <- c('scenario_id', 'avg_return')
  
  #plot(avg_return_table$avg_return)
  
  
  if(is.null(perc_rng)){
    
    max_index <- which(avg_return_table$avg_return==max(avg_return_table$avg_return))
    min_index <- which(avg_return_table$avg_return==min(avg_return_table$avg_return))
    
    opti_return <- return_data$return_fctr[max_index : (max_index+n-1)]
    pess_return <- return_data$return_fctr[min_index : (min_index+n-1)]
    
  }else{
    
    high_perc_index <- which.min(abs(avg_return_table$avg_return - as.numeric(quantile(avg_return_table$avg_return, perc_rng[2]))))
    low_perc_index <- which.min(abs(avg_return_table$avg_return - as.numeric(quantile(avg_return_table$avg_return, perc_rng[1]))))
    
    opti_return <- return_data$return_fctr[high_perc_index : (high_perc_index+n-1)]
    pess_return <- return_data$return_fctr[low_perc_index : (low_perc_index+n-1)]
    
  }
  
  return(list(opti_return, pess_return))
  
}
