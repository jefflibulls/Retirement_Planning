###############################
# retirement_projection.R
###############################

# Purpose
########################################################################
# This code projects retirement portfolio accumulation and distribution.

# Considerations
########################################################################
# Take assumptions in 2_initial_inputs.R and:
# 1) create retirement portfolio accumulation table
# 2) create retirement distribution schedule
# 3) Plot out visualization for retirement scenario


#**********************************************************************************************************************************


# Load necessary source files
source('code/1_libraries_load.R')
source('code/2_initial_inputs.R')
source('code/3_user_def_funcs_load.R')


# Data import
######################################################################################################################

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

# Number of withdrawal periods in retirement
withdraw_period_count <- life_expectancy*12 - target_retirement_age*12


# Retirement Nest Egg Accumulation Schedule Projections
######################################################################################################################

curr_dt <- init_date  # current date
P=init_principle  # starting portfolio balance
c=contribution_by_period  # current contribution to portfolio
c_annual_incr=annual_contribution_increase_perc  # annual rate of contribution increase 
c_mthly_incr=(1+c_annual_incr)^(1/12)-1  # monthly rate of contribution increase
n=contrib_period_count  # Number of periods to project


# Based on sp_500_returns file, generate 5th and 95th percentile avg return scenarios over
# specified time period.
return_scenarios_rng <- return_scenario_gen(sp_500_returns, n, perc_rng=c(0.05, 0.95))

annual_rate=assumed_avg_return_pre_retirement  # Assumed annual rate of return
mthly_rate <- (1+annual_rate)^(1/12)-1  # Assumed monthly rate of return

# Generate portfolio accumulation table based on assumptions (assumed interest rate)
annuity_accum_table <- annuity_fv(curr_dt, P, n, c, c_mthly_incr, mthly_rate)  

# Generate portfolio accumulation tables based on conservative and optimistic return scenarios
annuity_accum_table_opti <- annuity_fv_v2(curr_dt, P, n, c, c_mthly_incr, return_scenarios_rng[[1]])
annuity_accum_table_pess <- annuity_fv_v2(curr_dt, P, n, c, c_mthly_incr, return_scenarios_rng[[2]])

# Combine tables together
annuity_accum_rng <- cbind.data.frame('date'=annuity_accum_table$date, 
                                      'tot_opti'=annuity_accum_table_opti$tot, 'tot_pess'=annuity_accum_table_pess$tot)


# Retirement Nest Egg Withdraw Schedule Projections
######################################################################################################################

retire_dt <- annuity_accum_table$date[nrow(annuity_accum_table)]  # target retirement date
P=annuity_accum_table$tot[nrow(annuity_accum_table)]  # portfolio balance at retirement (assumed interest rate)
P_opti=annuity_accum_table_opti$tot[nrow(annuity_accum_table_opti)]  # Optimistic portfolio balance at retirement
P_pess=annuity_accum_table_pess$tot[nrow(annuity_accum_table_pess)]  # conservative portfolio balance at retirement
c=init_withdraw_target  # initial distribution/withdrawal amount
annual_rate=assumed_avg_return_post_retirement  # assumed annual rate of return
mthly_rate <- (1+annual_rate)^(1/12)-1  # monthly rate of return
annual_inflation=annual_withdraw_increase_perc  # assumed annual rate of distribution increase
mthly_inflation <- (1+annual_inflation)^(1/12)-1 # monthly rate of distribution increase
m <- withdraw_period_count  # Number of periods to project


# Generate amortization schedule based on normal/assumed scenario
amort_withdraw_table <- amort_sched(retire_dt, P, c, mthly_rate, mthly_inflation, m)

# Geneerate amortization schedules based on optimistic/conservative scenarios
amort_withdraw_table_opti <- amort_sched(retire_dt, P_opti, c, mthly_rate, mthly_inflation, m)
amort_dispersion_table_opti <- amort_withdraw_table_opti %>% mutate(remain_prin_opti=remain_prin) %>% select(date, remain_prin_opti)

amort_withdraw_table_pess <- amort_sched(retire_dt, P_pess, c, mthly_rate, mthly_inflation, m)
amort_dispersion_table_pess <- amort_withdraw_table_pess %>% mutate(remain_prin_pess=remain_prin) %>% select(date, remain_prin_pess)


# Combine tables together
amort_dispersion_rng <- merge(amort_dispersion_table_opti, amort_dispersion_table_pess, by='date', all=TRUE)
amort_dispersion_rng %<>% mutate(remain_prin_pess=ifelse(is.na(remain_prin_pess),0,remain_prin_pess))


# Retirement Projection Plotting 
#######################################################################################################################

# Combine accumulation and amortization tables together
overall_proj_table <- merge(annuity_accum_table, amort_withdraw_table, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, annuity_accum_rng, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, amort_dispersion_rng, by='date', all=TRUE)
overall_proj_table <- merge(overall_proj_table, actual_table, by='date', all=TRUE)

cutoff_yr <- 2090  # Specify cut off year for plotting
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



