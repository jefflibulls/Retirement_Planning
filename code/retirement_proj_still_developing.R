# Relevant Info
#######################################################################################################################

#balance at retirement
retire_balance <- annuity_accum_table$tot[nrow(annuity_accum_table)]
retire_balance_opti <- annuity_accum_rng$tot_opti[nrow(annuity_accum_rng)]
retire_balance_pess <- annuity_accum_rng$tot_pess[nrow(annuity_accum_rng)]

retire_balance
retire_balance_opti
retire_balance_pess


#date of depletion

if(amort_withdraw_table$remain_prin[nrow(amort_withdraw_table)] <= 0){
  
  depletion_date <- amort_withdraw_table$date[nrow(amort_withdraw_table)]
  depletion_age <- round(length(seq(from=as.Date(birth_date, '%M/%d/%Y'), to=depletion_date, by='month')) / 12, 1)
  
}else{
  
  print(paste0('depletion age greater than ', life_expectancy, '.'))
  
}


if(amort_withdraw_table_opti$remain_prin[nrow(amort_withdraw_table_opti)] <= 0){
  
  depletion_date_opti <- amort_withdraw_table_opti$date[nrow(amort_withdraw_table_opti)]
  depletion_age_opti <- round(length(seq(from=as.Date(birth_date, '%M/%d/%Y'), to=depletion_date_opti, by='month')) / 12, 1)
  
}else{
  
  print(paste0('depletion age greater than ', life_expectancy, '.'))
  
}


if(amort_withdraw_table_pess$remain_prin[nrow(amort_withdraw_table_pess)] <= 0){
  
  depletion_date_pess <- amort_withdraw_table_pess$date[nrow(amort_withdraw_table_pess)]
  depletion_age_pess <- round(length(seq(from=as.Date(birth_date, '%M/%d/%Y'), to=depletion_date_pess, by='month')) / 12, 1)
  
}else{
  
  print(paste0('depletion age greater than ', life_expectancy, '.'))
  
}


depletion_age
depletion_age_opti
depletion_age_pess


#contribution and withdraw schedule

contrib_schedule <- cbind.data.frame('date'=annuity_accum_table$date,
                                     'contrib_amt'=diag(as.matrix(annuity_accum_table[,-c(1:2)]))
)

withdraw_schedule <- amort_withdraw_table_opti %>% select(date, withdraw_amt)


#confidence intervals


avg_return_list <- numeric()
retire_balance_list <- numeric()
depletion_age_list <- numeric()

k <- 100
for(i in c(1:k)){
  
  curr_dt <- init_date
  P=init_principle
  c=contribution_by_period
  c_annual_incr=annual_contribution_increase_perc
  c_mthly_incr=(1+c_annual_incr)^(1/12)-1
  n=contrib_period_count
  
  return_scenario <- rand_return_scenario_gen(sp_500_returns, n)
  annuity_accum_scenario <- annuity_fv_v2(curr_dt, P, n, c, c_mthly_incr, return_scenario)
  
  avg_return <- prod(return_scenario)^(1/(length(return_scenario)/12)) - 1
  retire_balance <- annuity_accum_scenario$tot[nrow(annuity_accum_scenario)]
  
  retire_dt <- annuity_accum_table$date[nrow(annuity_accum_scenario)]
  P=annuity_accum_scenario$tot[nrow(annuity_accum_scenario)]
  c=init_withdraw_target
  annual_rate=assumed_avg_return_post_retirement
  annual_inflation=annual_withdraw_increase_perc
  mthly_rate <- (1+annual_rate)^(1/12)-1
  mthly_inflation <- (1+annual_inflation)^(1/12)-1
  m <- withdraw_period_count
  
  amort_withdraw_scenario <- amort_sched(retire_dt, P, c, mthly_rate, mthly_inflation, m)
  
  
  if(amort_withdraw_scenario$remain_prin[nrow(amort_withdraw_scenario)] <= 0){
    
    depletion_date <- amort_withdraw_scenario$date[nrow(amort_withdraw_scenario)]
    depletion_age <- round(length(seq(from=as.Date(birth_date, '%M/%d/%Y'), to=depletion_date, by='month')) / 12, 1)
    
  }else{
    
    print(paste0('depletion age greater than ', life_expectancy, '.'))
    depletion_age <- life_expectancy
    
  }
  
  avg_return_list <- c(avg_return_list, avg_return)
  retire_balance_list <- c(retire_balance_list, retire_balance)
  depletion_age_list <- c(depletion_age_list, depletion_age)
  
}


simulated_scenarios <- cbind.data.frame('scenario'=c(1:k), 
                                        'avg_return'=avg_return_list,
                                        'retire_balance'=retire_balance_list,
                                        'depletion_age'=depletion_age_list)

plot(sort(simulated_scenarios$avg_return))
plot(sort(simulated_scenarios$retire_balance))
plot(sort(simulated_scenarios$depletion_age))

summary(simulated_scenarios$avg_return)
summary(simulated_scenarios$retire_balance)
summary(simulated_scenarios$depletion_age)

conf_lvl <- length(which(simulated_scenarios$depletion_age >= 80)) / nrow(simulated_scenarios)
conf_lvl
