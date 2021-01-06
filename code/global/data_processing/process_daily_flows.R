# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It adds future dates & recession flows to the daily flow time series
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.cfs.df0 - table with daily flows created by import_data.R
# demands.daily.df - table with daily demands, production, & withdrawals
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.daily.cfs.df
# flows.daily.mgd.df (includes total Potomac demand - change to withdrawals???)
#   - used for the plots on sit awareness & 10 day ops tabs
#   - used to compute baseflow correction for lffs flows
#   - DELETE? used to create inflows.df in reservoirs_make.R
#   - DELETE? used to create potomac.data.df in potomac_flows_init.R
# *****************************************************************************

#------------------------------------------------------------------------------
# Add rest of the year's dates to this df
#  - this seems to make the app more robust if missing data
#  - added flow values are set to NA

# Identify the last date with daily flow data
daily_flow_data_last_date <- tail(flows.daily.cfs.df0, 1)$date_time

# Add future dates and dummy data to the df
flows.daily.cfs.df <- flows.daily.cfs.df0 %>%
  add_row(date_time = seq.Date(daily_flow_data_last_date + 1, 
                               date_dec31, 
                               by = "day"))

# Add recession flows for selected gages---------------------------------------
flows.daily.cfs.df <- recess_daily_flows_func(flows.daily.cfs.df, 
                                              # demands.daily.mgd.df, 
                                              daily_flow_data_last_date,
                                              n_gages_daily + 1)

# Convert from cfs to MGD------------------------------------------------------
func_cfs_to_mgd <- function(cfs) {round(cfs/mgd_to_cfs,0)}
flows.daily.mgd.df <- flows.daily.cfs.df %>%
  dplyr::mutate_at(2:(n_gages_daily + 1), func_cfs_to_mgd)

# Add Potomac withdrawals -----------------------------------------------------
flows.daily.mgd.df <- left_join(flows.daily.mgd.df, demands.daily.df,
                                by = "date_time")

# Predict LFalls from upstream gages using constant lags ----------------------
flows.daily.mgd.df <- flows.daily.mgd.df %>%
  dplyr::mutate(lfalls_from_upstr = lag(por, 2) + lag(monoc_jug, 2)
                + lag(goose, 1) + lag(seneca, 1) - lag(d_pot_total, 1))