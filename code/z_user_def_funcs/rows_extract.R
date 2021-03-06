# Timeline generator ############################################

# Parameters:
# data: a data frame
# init_row: first row from which to extract data
# n: a consecutive number of rows to extract from data
# returns a data frame containing n consecutive
# records from the input data frame.

rows_extract <- function(data, init_row, n){
  
  col_nms <- names(data)
  c <- ncol(data)
  
  # Pull out a historical n starting from period 'init_row'
  start <- init_row
  end <- init_row + (n-1)
  
  extract_result <- data[start:end,]
  
  extract_result <- as.data.frame(extract_result)
  names(extract_result) <- col_nms
  
  return(extract_result)
  
}

