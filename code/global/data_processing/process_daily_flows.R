# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R, so df's are globally accessible
# It adds future dates & recession flows to the daily flow time series
# *****************************************************************************
# INPUTS
# *****************************************************************************
# historical_flows_daily_cfs_df - 2014 thru 2020 daily flows, archived
# flows.daily.cfs.df0 - daily flows created by import_data.R
# withdrawals.daily.df - daily withdrawals created by process_withdrawals.R
# data/flow_data_daily_from2014thru2020.csv - historical data
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.daily.cfs.df - currently from 2014-01-01 thru yesterday
# flows.daily.mgd.df (includes total Potomac demand - change to withdrawals???)
#   - used for the plots on sit awareness & 10 day ops tabs
#   - used to compute baseflow correction for lffs flows
#   - DELETE? used to create inflows.df in reservoirs_make.R
#   - DELETE? used to create potomac.data.df in potomac_flows_init.R
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Create daily flow df from 2014-01-01 to end of current year
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# First read the historical data-----------------------------------------------
historical_flows_daily_cfs_df <- data.table::fread(
  paste("data/", "flow_data_daily_from2014thru2020.csv", sep = ""),
  header = TRUE,
  stringsAsFactors = FALSE,
  colClasses = c("character", rep("numeric", n_gages_daily)), # force numeric
  col.names = list_gages_daily_locations, # 1st column is "date"
  na.strings = c("eqp", "Ice", "Bkw", "", "#N/A", "NA", -999999),
  data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date)) %>%
  select(-date) %>%
  filter(!is.na(date_time)) %>%
  select(date_time, everything()) %>%
  arrange(date_time)

# Append historical daily data to this year's data-----------------------------
flows.daily.cfs.df1 <- rbind(historical_flows_daily_cfs_df,
                             flows.daily.cfs.df0)

#------------------------------------------------------------------------------
# Add rest of the year's dates to this df
#  - this seems to make the app more robust if missing data
#  - added flow values are set to NA

# Identify the last date with daily flow data
daily_flow_data_last_date <- tail(flows.daily.cfs.df1, 1)$date_time

# Add future dates and dummy data to the df
flows.daily.cfs.df <- flows.daily.cfs.df1 %>%
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
flows.daily.mgd.df <- left_join(flows.daily.mgd.df, withdrawals.daily.df,
                                by = "date_time") %>%
  # temporary fix - at some point need to eliminate d_pot_total
  dplyr::mutate(d_pot_total = w_pot_total_net)

# Predict LFalls from upstream gages using constant lags ----------------------
flows.daily.mgd.df <- flows.daily.mgd.df %>%
  dplyr::mutate(lfalls_from_upstr = lag(por, 2) + lag(monoc_jug, 2)
                + lag(goose, 1) + lag(seneca, 1) - lag(d_pot_total, 1))