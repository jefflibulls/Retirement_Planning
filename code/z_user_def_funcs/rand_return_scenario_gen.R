
rand_return_scenario_gen <- function(return_data, n=12){
  
  #Source files
  #source("fiscal_scenarios/rand_rows_extract.R")
  
  # Creating a next day open column in order to compare to
  # actual open when calculating returns.
  # This is more accurate than comparing open vs close because
  # it accounts for after market movement.
  return_data$next_day_open <- c(return_data$Open[2:nrow(return_data)],
                                    return_data$Close[nrow(return_data)])
  
  return_data %<>% mutate(return=next_day_open/Open-1,
                             return_fctr=next_day_open/Open)
  
  
  # Random timeline generator ############################################
  
  # Pull out a historical n-month period at random
  rand_data <- rand_rows_extract(return_data, n)
  
  #plot(rand_data$Close)
  
  #prod(rand_data$return_fctr)^(1/(nrow(rand_data)/12))
  
  
  return(rand_data$return_fctr)
  
}
