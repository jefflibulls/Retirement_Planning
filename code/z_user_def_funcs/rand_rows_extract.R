###############################
# rand_rows_extract.R
###############################

# Purpose
########################################################################
# Extracts random consecutive rows from a data frame. This function
# is similar to rows_extract.R except the starting point of the
# extraction is random.

# Parameters
########################################################################
# data: a data frame
# n: a consecutive number of rows to extract from data

# Output
########################################################################
# returns a data frame containing a random n consecutive
# records from the input data frame.


#**********************************************************************************************************************************


rand_rows_extract <- function(data, n){
  
  col_nms <- names(data)  #Save column names of data
  c <- ncol(data)  #Determine number of columns in data
  
  # Pull out n consecutive periods at random
  
  # Randomly generate starting point. Constraint is that there must be > n rows in the data after the starting point. 
  rand_start <- round(runif(1, min=1, max=nrow(data)-(n-1))) 
  rand_end <- rand_start + (n-1)
  
  # lapply is not necessary here; but used for learning the function
  rand_result <- lapply(1:c, function(i){
    data[,i][rand_start:rand_end]
  })
  
  rand_result <- as.data.frame(rand_result)
  names(rand_result) <- col_nms  #Reassign column names to filtered data
  
  return(rand_result)
  
}

