

source('code/1_libraries_load.R')
source('code/2_user_def_funcs_load.R')
source('code/3_initial_inputs.R')

#Import s&p500 returns file
sp_500_returns <- fread(sp_500_filepath, header=TRUE)
setnames(sp_500_returns, names(sp_500_returns))



# Simulation Inputs Configurations
######################################################################################################################

# Expected contribution dollar amount by month
contribution_by_period <- (curr_salary * (contrib_401k_perc + company_401k_match_perc) + annual_ira_contribution) / 12

# Number of months before retirement age
age_in_months <- length(seq(from=as.Date(birth_date, '%M/%d/%Y'), to=as.Date(init_date, '%M/%d/%Y'), by='month'))
months_till_retirement <- target_retirement_age*12 - age_in_months
contrib_period_count <- months_till_retirement

# Expected initial withdraw rate at retirement in order to achieve targeted income level at current cost of living
target_retirement_income_fv <- target_retirement_income_lvl*((1+annual_withdraw_increase_perc)^(months_till_retirement/12))
init_withdraw_target <- target_retirement_income_fv / 12
withdraw_period_count <- life_expectancy*12 - target_retirement_age*12



# Retirement Nest Egg Accumulation Schedule Projections
######################################################################################################################

curr_dt <- init_date
P=init_principle
c=contribution_by_period
c_annual_incr=annual_contribution_increase_perc
c_mthly_incr=(1+c_annual_incr)^(1/12)-1
n=contrib_period_count

return_scenarios_rng <- return_scenario_gen(sp_500_returns, n, perc_rng=c(0.05, 0.95))

annual_rate=assumed_avg_return_pre_retirement
mthly_rate <- (1+annual_rate)^(1/12)-1
annuity_accum_table <- annuity_fv(curr_dt, P, n, c, c_mthly_incr, mthly_rate)

annuity_accum_table_opti <- annuity_fv_v2(curr_dt, P, n, c, c_mthly_incr, return_scenarios_rng[[1]])
annuity_accum_table_pess <- annuity_fv_v2(curr_dt, P, n, c, c_mthly_incr, return_scenarios_rng[[2]])

annuity_accum_rng <- cbind.data.frame('date'=annuity_accum_table$date, 
                                      'tot_opti'=annuity_accum_table_opti$tot, 'tot_pess'=annuity_accum_table_pess$tot)



# Retirement Nest Egg Withdraw Schedule Projections
######################################################################################################################

retire_dt <- annuity_accum_table$date[nrow(annuity_accum_table)]
P=annuity_accum_table$tot[nrow(annuity_accum_table)]
P_opti=annuity_accum_table_opti$tot[nrow(annuity_accum_table_opti)]
P_pess=annuity_accum_table_pess$tot[nrow(annuity_accum_table_pess)]
c=init_withdraw_target
annual_rate=assumed_avg_return_post_retirement
annual_inflation=annual_withdraw_increase_perc
mthly_rate <- (1+annual_rate)^(1/12)-1
mthly_inflation <- (1+annual_inflation)^(1/12)-1
m <- withdraw_period_count

amort_withdraw_table <- amort_sched(retire_dt, P, c, mthly_rate, mthly_inflation, m)

amort_withdraw_table_opti <- amort_sched(retire_dt, P_opti, c, mthly_rate, mthly_inflation, m)
amort_dispersion_table_opti <- amort_withdraw_table_opti %>% mutate(remain_prin_opti=remain_prin) %>% select(date, remain_prin_opti)

amort_withdraw_table_pess <- amort_sched(retire_dt, P_pess, c, mthly_rate, mthly_inflation, m)
amort_dispersion_table_pess <- amort_withdraw_table_pess %>% mutate(remain_prin_pess=remain_prin) %>% select(date, remain_prin_pess)

amort_dispersion_rng <- merge(amort_dispersion_table_opti, amort_dispersion_table_pess, by='date', all=TRUE)
amort_dispersion_rng %<>% mutate(remain_prin_pess=ifelse(is.na(remain_prin_pess),0,remain_prin_pess))



# Retirement Projection Plotting 
#######################################################################################################################

overall_proj_table <- merge(annuity_accum_table, amort_withdraw_table, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, annuity_accum_rng, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, amort_dispersion_rng, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, actual_table, by='date', all=TRUE)

cutoff_yr <- 2090
plot_data <- overall_proj_table %>% filter(year(overall_proj_table$date) <= cutoff_yr)

plot <- ggplot()
plot <- plot + geom_line(data=plot_data, aes(x=date, y=tot), size=1, colour='springgreen4') 
plot <- plot + geom_line(data=plot_data, aes(x=date, y=remain_prin), size=1, colour='red')
plot <- plot + geom_ribbon(data=plot_data, aes(x=date, ymin=tot_pess, ymax=tot_opti), fill='springgreen4', alpha=0.2)
plot <- plot + geom_ribbon(data=plot_data, aes(x=date, ymin=remain_prin_pess, ymax=remain_prin_opti), fill='red', alpha=0.2)
plot <- plot + geom_point(data=plot_data, aes(x=date, y=accum_amt), colour='navy')
#plot <- plot +  scale_x_date(date_breaks = '5 year', date_labels='%Y') + theme(axis.text.x = element_text(angle=45, hjust=1, size=7)) 
plot <- plot +  scale_x_date(date_labels='%b-%Y') + theme(axis.text.x = element_text(angle=45, hjust=1, size=7)) 
plot <- plot +  labs(title='Retirement Nestegg Projection', x="Date", y="Total Accumulated Amount($)") 

plot



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
