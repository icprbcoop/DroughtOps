# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R, so df's are globally accessible
# It adds historical flows, today's recent hourly, 
#     future dates & recession flows to the daily flow time series
# *****************************************************************************
# INPUTS
# *****************************************************************************
# historical_flows_daily_cfs_df - 2014 thru 2020 daily flows, archived
# flows.daily.cfs.df0 - daily flows created by import_data.R
# flows.hourly.cfs.df0 - hourly flows created by import_data.R
# withdrawals.daily.df - daily withdrawals created by process_withdrawals.R
# data/flow_data_daily_from2014thru2020.csv - historical data
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.daily.cfs.df - from 2014-01-01 thru today; w/ some recession flows
#   - df actually has records up to end of calendar year
# flows.daily.mgd.df (includes Potomac demands)
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

if(DREX == 0) {
  # Read  and append the historical data-----------------------------------------
  # needs_turn_of_year_update
  #   - update and rename flow_data_daily_from2014thru2021.csv
  historical_flows_daily_cfs_df <- data.table::fread(
    paste("data/", "flow_data_daily_from2014thru2021.csv", sep = ""),
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
  # Append this year's data to historical daily data-----------------------------
  flows.daily.cfs.df1 <- rbind(historical_flows_daily_cfs_df,
                             flows.daily.cfs.df0)
  }

# if it's a DREX, won't be verifying fc's, so don't need more data
if(DREX == 1) {flows.daily.cfs.df1 <- flows.daily.cfs.df0}

# Next grab flows of most recent hourlies for "today's" values
#   actually, step back an hour to avoid missing data
today_hourly_cfs.df00 <- tail(flows.hourly.cfs.df0, 2)
today_hourly_cfs.df0 <- head(today_hourly_cfs.df00, 1)  %>%
  mutate(date_time = as.Date(date_time)) 

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

# Finally, update with today's recent values
flows.daily.cfs.df <- rows_update(flows.daily.cfs.df, today_hourly_cfs.df0, 
                              by = "date_time")

# Add recession flows for selected gages---------------------------------------
flows.daily.cfs.df <- recess_daily_flows_func(flows.daily.cfs.df, 
                                              # demands.daily.mgd.df, 
                                              daily_flow_data_last_date + 1,
                                              n_gages_daily + 1)

# Convert from cfs to MGD------------------------------------------------------
func_cfs_to_mgd <- function(cfs) {round(cfs/mgd_to_cfs,0)}
flows.daily.mgd.df <- flows.daily.cfs.df %>%
  dplyr::mutate(across(where(is.numeric), func_cfs_to_mgd))

# Add Potomac withdrawals to satisfy legacy sim code --------------------------
flows.daily.mgd.df <- left_join(flows.daily.mgd.df, withdrawals.daily.df0,
                                by = "date_time") %>%
  # temporary fix - at some point need to eliminate d_pot_total
  dplyr::mutate(d_pot_total = w_pot_total_net) # %>%
  # # Predict LFalls from upstream gages using constant lags
  # dplyr::mutate(lfalls_from_upstr = lag(por, 2) + lag(monoc_jug, 2)
  #                 + lag(goose, 1) + lag(seneca, 1) - lag(d_pot_total, 1))
