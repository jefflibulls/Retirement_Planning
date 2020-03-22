# File name: amort_sched.R
# Functions in this program estimates amortization schedule of withdraws.

# amort_sched
##################
# inputs:
# curr_dt -> current date
# curr_prin -> Principle amount at current date
# withr_amt -> amount withrawn from principle at beginning of each period (current function assumes monthly unit)
# intrst_rate -> assumed interest rate by period (current function assumes monthly unit)
# inflation -> assumed inflation rate (impacts amount of withdraw) (assumed monthly unit)
# prd_cnt -> Number of periods to project into the future (unit agnostic)
# Note:  Make sure the units match between withr_amt and intrst_rate and inflation andn prd_cnt

# output:
# dataframe reflecting the amortization schedule 



amort_sched <- function(curr_dt, curr_prin, withdr_amt, intrst_rate, inflation, prd_cnt){
  
  #curr_dt <- as.Date(curr_dt, '%M/%d/%Y')
  dt_list <- curr_dt
  
  # Calculate Amortization for each period
  n <- 0 #starting period count
  currP <- curr_prin #starting principal amount
  Pt <- curr_prin - withdr_amt # current principal minus first withdrawal (assumes withdrawal at month start)
  withdraw_list <- withdr_amt
  prd_list <- n
  while(Pt>=0) {
    
    n <- n + 1
    prd_list <- c(prd_list, n)
    
    H <- Pt * intrst_rate # this is the current interest by period
    withdraw <- c * (1+inflation)^n
    C <- withdraw - H # this is your payment by period minus your interest by period, so it is the amount of principal you deduct for that month
    Q <- Pt - C # this is the new balance of your principal of your loan
    Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
    currP <- c(currP, Pt)
    withdraw_list <- c(withdraw_list, withdraw)
    
    dt_var <- curr_dt %m+% months(n)
    dt_list <- c(dt_list, dt_var)

    # breaks out of loop after a specified amount of periods
    if(n > prd_cnt) break
    
  }
  
  withdraw_table <- cbind.data.frame('date'=dt_list, 'withdraw_amt'=withdraw_list, 'remain_prin'=currP)
  
  return(withdraw_table)
  
}
