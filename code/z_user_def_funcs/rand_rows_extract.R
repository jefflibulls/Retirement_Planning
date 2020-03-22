# Random timeline generator ############################################

# Parameters:
# data: a data frame
# n: a consecutive number of rows to extract from data
# returns a data frame containing a random n consecutive
# records from the input data frame.

rand_rows_extract <- function(data, n){
  
  col_nms <- names(data)
  c <- ncol(data)
  
  # Pull out a historical n consecutive periods at random
  rand_start <- round(runif(1, min=1, max=nrow(data)-(n-1)))
  rand_end <- rand_start + (n-1)
  
  rand_result <- lapply(1:c, function(i){
    data[,i][rand_start:rand_end]
  })
  
  rand_result <- as.data.frame(rand_result)
  names(rand_result) <- col_nms
  
  return(rand_result)
  
}

