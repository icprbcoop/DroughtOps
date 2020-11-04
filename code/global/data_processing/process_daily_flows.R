# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It processes the daily time series data
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.cfs.df0 - table with daily flows created by import.R
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.daily.cfs.df
# flows.daily.mgd.df
#   - used for the plots on sit awareness & 10 day ops tabs
#   - used to compute baseflow correction for lffs flows
#   - DELETE? used to create inflows.df in reservoirs_make.R
#   - DELETE? used to create potomac.data.df in potomac_flows_init.R
# *****************************************************************************

#------------------------------------------------------------------------------
# Add rest of the year's dates to this df
#  - this seems to make the app more robust if missing data
#  - added flow values are set to NA

# identify the last date with daily flow data
daily_flow_data_last_date <- tail(flows.daily.cfs.df0, 1)$date_time

# add future dates and dummy data to the df
flows.daily.cfs.df <- flows.daily.cfs.df0 %>%
  add_row(date_time = seq.Date(daily_flow_data_last_date + 1, 
                               date_dec31, 
                               by = "day"))

#------------------------------------------------------------------------------
# Convert daily flows to MGD, add recession flows for selected gages, 
#    and combine with daily demands

flows.daily.mgd.df <- recess_daily_flows_func(flows.daily.cfs.df, 
                                              demands.daily.mgd.df, 
                                              daily_flow_data_last_date,
                                              llen)