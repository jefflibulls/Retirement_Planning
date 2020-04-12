###############################
# amort_sched.R
###############################

# Purpose
########################################################################
# This function produces the amortization/distribution schedule of
# a typical retirement portfolio, given specified assumptions.

# Considerations
########################################################################
# Given portfolio amount and withdraw rate, how long will the 
# portfolio last?

# Parameters
########################################################################
# curr_dt -> current date
# curr_prin -> Principle amount at current date
# withr_amt -> Initial amount withrawn from principle at beginning of period (current function assumes monthly unit)
# intrst_rate -> assumed interest rate by period (current function assumes monthly unit)
# inflation -> assumed inflation rate (impacts amount of withdraw) (assumed monthly unit)
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# Note:  Make sure the units match between withr_amt and intrst_rate and inflation andn prd_cnt

# Output
########################################################################
# Returns an amortization schedule table of the
# retirement portfolio.


#**********************************************************************************************************************************


amort_sched <- function(curr_dt, curr_prin, withdr_amt, intrst_rate, inflation, prd_cnt){
  
  # Idea here is to iterate and update the principle amount for n times specified by prd_cnt,
  # or until portfolio balance reaches 0.  So using a while loop here.
  
  
  # First set up initial variable levels for while loop
  ###############################################################################################
  
  #curr_dt <- as.Date(curr_dt, '%M/%d/%Y')
  dt_list <- curr_dt #starting the first element of date tracking list
  
  n <- 0 #starting period count
  prd_list <- n #starting the first element of period tracking list
  
  currP <- curr_prin #starting principal amount
  Pt <- curr_prin - withdr_amt # principal minus first withdrawal (assumes withdrawal at month start)
  withdraw_list <- withdr_amt #starting the first element of withdrawal amount tracking list

  
  # Loop through amortization iterations
  ###############################################################################################
  
  while(Pt>=0) {
    
    n <- n + 1  #update iteration number
    prd_list <- c(prd_list, n)  #continue adding to period tracking list
    
    H <- Pt * intrst_rate # this is the interest earned in current period
    withdraw <- withdr_amt * (1+inflation)^n  # withdrawal amount for current period
    C <- withdraw - H # this is your curr period distribution minus your interest earned, so it is the amount of principal you deduct for that month
    Q <- Pt - C # this is the new balance of your principal of your loan
    Pt <- Q # sets Pt equal to Q and goes back to step 1. The loop continues until the value Q (and hence Pt) goes to zero
    
    currP <- c(currP, Pt)  #continue adding to principle tracking list
    withdraw_list <- c(withdraw_list, withdraw)  #continue adding to withdrawal amount tracking list
    
    dt_var <- curr_dt %m+% months(n)
    dt_list <- c(dt_list, dt_var)  #continue adding to date tracking list

    
    # breaks out of loop after a specified amount of periods
    # This is necessary because if interest rate is too high here, 
    # principle may never go to zero and the loop becomes infinite.
    if(n > prd_cnt) break
    
  }
  
  
  # Create amortization schedule table 
  #############################################################################################
  withdraw_table <- cbind.data.frame('date'=dt_list, 'withdraw_amt'=withdraw_list, 'remain_prin'=currP)
  
  
  return(withdraw_table)
  
}
