# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Compute simulation evaluation metrics - for both flows and withdrawals
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows_daily_cfs.csv - current downloaded daily flow data, beginning Jan 1
# demands.daily.df - WMA supplier daily withdrawal data
# daily_flow_data_last_date - date of last day of available data
# n_cols - number of columns in flows_daily_cfs.csv
# *****************************************************************************
# OUTPUT
# *****************************************************************************
# flows.daily.mgd.df
#   - units changed from cfs to mgd
#   - recession flows added based on past 3-day min flows
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Add future flows to selected gages, estimated by simple "recession" algorithm
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


verification_metrics_daily_func <- function(
  obs_df = flows.daily.cfs.df, 
  sim_df = lffs.daily.mgd.df,
  location = lfalls,
  simtype = "lffs",
  ts_type = "daily",
  date_first = date_today0 - 30,
  date_last = date_today0 - 1) {
  

  

  
  return(flows.daily.cfs.df)
}



