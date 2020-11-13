# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It processes the real-time North Branch reservoir storage data
# *****************************************************************************
# INPUTS
# *****************************************************************************
# jrr.rt.storage.df0 - table with rt jrr storage, 
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.hourly.cfs.df
# flows.daily.mgd.df
#   - used for the plots on sit awareness & 10 day ops tabs
#   - used to compute baseflow correction for lffs flows
#   - DELETE? used to create inflows.df in reservoirs_make.R
#   - DELETE? used to create potomac.data.df in potomac_flows_init.R
# *****************************************************************************

# Add 3 days of rows with added flow values = NA-------------------------------
#   (revisit - not sure if this is necessary or why it's being done)
last_hour <- tail(flows.hourly.cfs.df$date_time, 1)
last_hour <- last_hour + lubridate::hours(1)
flows.hourly.cfs.df <- flows.hourly.cfs.df %>%
  add_row(date_time = seq.POSIXt(last_hour, length.out = 72, by = "hour"))