# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It imports time series (ts) data.
# The path to the local files with time series data is defined in global.R
# The paths to the automatically accessed online data are given below
# *****************************************************************************
# INPUTS
# *****************************************************************************
# gages_daily.csv - file listing USGS stream gages we use for daily data
# 
#------------------------------------------------------------------------------
# DAILY FLOW DATA
#   Can be obtained from 2 sources.
#   The OPTION switch is set in global.R:
#     OPTION 1: read file directly from NWIS
#             (autoread_dailyflows == 1)
#     OPTION 2: read local file, /input/ts/current/flows_daily_cfs.csv
#             (autoread_dailyflows == 0)
#   For autoread option, if date >= June 1, start date is Jan 1;
#             end date is yesterday.
#------------------------------------------------------------------------------
# HOURLY FLOW DATA
#   Can be obtained from 2 sources:
#   (start date is user-selected and end date is today)
#
#   The OPTION switch is set in global.R:
#     OPTION 1: read file directly from USGS's NWIS websites
#             (autoread_hourlyflows == 1)
#     OPTION 2: read local file, /input/ts/current/flows_hourly_cfs.csv
#             (autoread_hourlyflows == 0)
#------------------------------------------------------------------------------
# WITHDRAWAL DATA
#   Can be obtained from 2 sources:
#   (start date is user-selected and end date is 15 days into the future)
#
#   The OPTION switch is set in global.R:
#     OPTION 1: read file directly from Data Portal
#             (autoread_hourlywithdrawals == 1)
#     OPTION 2: read local file, /input/ts/current/flows_hourly_cfs.csv
#             (autoread_hourlywithdrawals == 0)

#------------------------------------------------------------------------------
# RESERVOIR STORAGE DATA
#   Can be obtained from 2 sources:
#   (start date is user-selected and end date is current day)
#
#   The OPTION switch is set in global.R:
#     OPTION 1: read file directly from Data Portal
#             (autoread_dailystorage == 1)
#     OPTION 2: read local file, /input/ts/current/flows_hourly_cfs.csv
#             (autoread_dailystorage == 0)
#------------------------------------------------------------------------------
# LFFS DATA
#   Can be read from two locations

#   The OPTION switch is set in global.R:
#     OPTION 1: read file directly from Data Portal
#             (autoread_dailyflows == 1)
#     OPTION 2: read local file, /input/ts/current/flows_daily_cfs.csv
#             (autoread_dailyflows == 0)
#------------------------------------------------------------------------------
# STATE DROUGHT STATUS
#   - time series of gw, precip, etc indices for MD, VA
#   - read from state_drought_status.csv 
#   - this is currently a dummy file from 2018 DREX

#------------------------------------------------------------------------------
# Fake reservoir ops dfs, e.g., drex2018_output_sen.csv
#   - used to initialize the res.ts.df's until I decide how to handle this

# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows.daily.cfs.df0 - processed by /data_processing/process_daily_flows.R
# flows.hourly.cfs.df0 - processed by /data_processing/process_hourly_flows.R
# withdrawals.hourly.mgd.df0 - really withdrawals right now
#   - used to create potomac.data.df in potomac_flows_init.R
#   - used in sim_main_func in call to simulation_func
#   - used in sim_add_days_func in call to simulation_func
# lffs.hourly.cfs.df0
# state.drought.df
#   - used in state_status_ts_init.R
#   - used in state_indices_update_func.R
# sen.ts.df00, pat.ts.df00, ..., from date_start to date_end (from parameters)
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PRELIMINARIES
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Read list of daily flow gages: id, location, description --------------------
#   - e.g. 1638500, por, Potomac River at Point of Rocks
gages_daily <- data.table::fread(paste(parameters_path, "gages_daily.csv", 
                                       sep = ""),
                           header = TRUE,
                           colClasses = c('gage_id' = 'character',
                                          'location' = 'character',
                                          'area_mi2' = 'numeric',
                                          'k' = 'numeric',
                                          'description' = 'character'), 
                           data.table = FALSE) %>%
  mutate(gage_id = paste("0", gage_id, sep=""))
list_gages_daily_locations <- c("date", gages_daily$location)
list_gages_daily_ids <- c(gages_daily$gage_id)

# first find number of columns in Sarah's flow data files
n_gages_daily <- length(list_gages_daily_locations) - 1
gages_daily_locations <- list_gages_daily_locations[2:(n_gages_daily + 1)]
gages_daily_locations <- as.list(gages_daily_locations)

# Will make use of first and last day of current year
date_dec31 <- lubridate::ceiling_date(date_today0, unit = "year") - 1
date_jan1 <- lubridate::floor_date(date_today0, unit = "year")

# Also may use
today_month <- substring(date_today0, first = 6, last = 7)
today_day <- substring(date_today0, first = 9, last = 10)
today_year <- substring(date_today0, first = 1, last = 4)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# DAILY FLOW DATA
# This will be appended to historical dailies, currently ending 2020-12-31
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Set switch (this has been moved to global)-----------------------------------
# autoread_dailyflows <- 1 # automatic data retrieval from NWIS
# autoread_dailyflows <- 0 # read data from file in local directory

#------------------------------------------------------------------------------
# DAILY FLOW OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read daily flow data automatically from Data Portal
#   - start date is January 1 of the current year
#------------------------------------------------------------------------------

if(autoread_dailyflows == 1) {
    start_date_string <- paste(year(date_today0), "-01-01", sep="")
    end_date_string <- paste(date_today0 - 1)
    # the relevant fields are: site_no, Date, X00060_00003:
    flows_daily_long_cfs_df0 <- dataRetrieval::readNWISdv(
      siteNumbers = list_gages_daily_ids,
      parameterCd = "00060",
      startDate = start_date_string,
      endDate = end_date_string,
      statCd = "00003") %>%
      mutate(date_time = as.Date(Date), flow_cfs = X_00060_00003) %>%
      select(date_time, site_no, flow_cfs)
    flows_daily_long_cfs_df <- left_join(flows_daily_long_cfs_df0, 
                                     gages_daily, 
                                     by = c("site_no" = "gage_id")) %>%
      select(date_time, location, flow_cfs)
    flows.daily.cfs.df0 <- flows_daily_long_cfs_df %>%
      pivot_wider(names_from = location, values_from = flow_cfs)
}

#------------------------------------------------------------------------------
# DAILY FLOW OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - read daily flow data from file residing in /input/ts/current/
#   - file name is flows_daily_cfs.csv
#   - code set up so that these time series should begin on Jan 1 of current year
#   - can create this file by hitting the "Write output time series" button 
#        on the sidebar to the left, then copying from /output and pasting
#        into /input/ts/current/.
#   - OR, daily data can be downloaded from CO-OP's Data Portal
#      - link for manual download is https://icprbcoop.org/drupal4/icprb/flow-data
#      - name appropriately then save the file to /input/ts/current/
#------------------------------------------------------------------------------

if(autoread_dailyflows == 0) {
  
  # read the lacal data table--------------------------------------------------
  flows.daily.cfs.df0 <- data.table::fread(
    paste(ts_path, "flows_daily_cfs.csv", sep = ""),
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
}
print("finished importing daily flows")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# HOURLY FLOW DATA:
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Set switch (this has been moved to global)-----------------------------------
# autoread_hourlyflows <- 1 # automatic data retrieval from USGS NWIS
# autoread_hourlyflows <- 0 # read data from file in local directory

#------------------------------------------------------------------------------
# HOURLY FLOW OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read hourly data automatically from NWIS using package, dataRetrieval
#------------------------------------------------------------------------------

if(autoread_hourlyflows == 1) {
  
  # define the desired gages---------------------------------------------------
  gages_hourly_names <- c("lfalls", 
                          "seneca",
                          "goose",
                          "monoc_jug", 
                          "por", 
                          "luke",
                          "kitzmiller",
                          "barnum",
                          "bloomington",
                          "barton")
  gages_hourly_ids <- c("01646500", 
                        "01645000",
                        "01644000",
                        "01643000", 
                        "01638500",
                        "01598500",
                        "01595500",
                        "01595800",
                        "01597500",
                        "01596500")
  gages_hourly <- data.frame(gage_id = gages_hourly_ids,
                             location = gages_hourly_names)
  
  # n_gages_hourly <- length(gages_hourly_ids)
  
  # set desired number of past days--------------------------------------------
  n_past_days <- 90
  # start_date <- as.POSIXct(date_today0) - lubridate::days(n_past_days)
  # start_datetime <- time_now0 - lubridate::days(n_past_days)
  # start_datetime <- lubridate::with_tz(start_datetime, "EST")
  # # round to nearest hour in order to use dataRetrieval
  # start_datetime <- lubridate::floor_date(start_datetime, unit = "hours")
  # 
  # # Create dummy df of dates and hours-----------------------------------------
  # temp0 <- start_datetime - lubridate::hours(1) # first step back 1 hour
  # flows.hourly.empty.df <- data.frame(date_time = temp0) %>%
  #   add_row(date_time = seq.POSIXt(start_datetime,
  #                                  by = "hour",
  #                                  length.out = n_past_days*24))
  
  # download hourly flows into a df--------------------------------------------
  #   - the function below makes use of the USGS's package, dataRetrieval
  #   - timezone is set as EST
  
  # the relevant fields are: site_no, Date, X00060_00003:
  flows_rt_long_cfs_df0 <- dataRetrieval::readNWISuv(
    # siteNumbers = gages_hourly_ids,
    siteNumbers = gages_hourly_ids,
    parameterCd = "00060",
    # startDate = start_date,
    startDate = date_today0 - n_past_days,
    endDate = date_today0,
    tz = "America/New_York"
  ) %>%
    mutate(date_time = dateTime, flow_cfs = X_00060_00000) %>%
    select(date_time, site_no, flow_cfs)
  flows_rt_long_cfs_df <- left_join(flows_rt_long_cfs_df0, 
                                       gages_hourly, 
                                       by = c("site_no" = "gage_id")) %>%
    select(date_time, location, flow_cfs)
  flows_rt_cfs_df <- flows_rt_long_cfs_df %>%
    pivot_wider(names_from = location, values_from = flow_cfs)
  
  flows.hourly.cfs.df0 <- flows_rt_cfs_df %>%
    mutate(date_hour = lubridate::round_date(date_time,
                                             unit = "hour")) %>%
    select(-date_time) %>%
    group_by(date_hour) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    rename(date_time = date_hour) %>%
    ungroup()
  
  # flows.hourly.cfs.df0 <- get_hourly_flows_func(gage_nos = gages_hourly_nos, 
  #                                              gage_names = gages_hourly_names, 
  #                                              flows_empty = flows.hourly.empty.df,
  #                                              start_date = date_today0 - 
  #                                                lubridate::days(n_past_days),
  #                                              end_date = date_today0,
  #                                              usgs_param = "00060")
  
  # Trim off a day of beginning rows to get rid of NA's------------------------
  #   (this needs to be improved - ripe for creating a bug!)
  # flows.hourly.cfs.df0 <- tail(flows.hourly.cfs.df0, (n_past_days - 1)*24)
  }

#------------------------------------------------------------------------------
# HOURLY FLOW OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - flow data file resides in /input/ts/current/
#   - file name is flows_hourly_cfs.csv
#   - hourly data can be downloaded from CO-OP's Data Portal
#   - link for manual download is https://icprbcoop.org/drupal4/icprb/flow-data
#      - grab last few days of data only (or memory error!)
#      - name appropriately then save the file to /input/ts/current/
#      - can create a longer time series by pasting new data into existing file
#------------------------------------------------------------------------------

if(autoread_hourlyflows == 0) {

  # read the lacal data table--------------------------------------------------
  #   (need to convert date times to POSIXct for hourly's)
  flows.hourly.cfs.df0 <- data.table::fread(
    paste(ts_path, "flows_hourly_cfs.csv", sep = ""),
    header = TRUE,
    stringsAsFactors = FALSE,
    colClasses = c('date' = 'character'), 
    # col.names = list_gages_daily_locations, # 1st column is "date"
    na.strings = c("eqp", "Ice", "Bkw", "", "#N/A", "NA", -999999),
    data.table = FALSE) %>%
    dplyr::mutate(date_time = as.POSIXct(date, tz = "EST")) %>%
    select(-date) %>%
    arrange(date_time) %>%
    filter(!is.na(date_time)) %>% # sometime these are sneaking in
    head(-1) %>% # the last record is sometimes missing most data
    select(date_time, everything())
}
print("finished importing hourly flows")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# WITHDRAWAL DATA
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Set switch (this has been moved to global)-----------------------------------
# autoread_hourlywithdrawals <- 1 # automatic data retrieval from Data Portal
# autoread_hourlywithdrawals <- 0 # read data from file in local directory

# a temporary need until the time series becomes available
discharge_broadrun <- 5 # MGD
# might be temporary - FW Central SA demand - water purchased from WA
d_fw_c <- 10 # MGD

#------------------------------------------------------------------------------
# WITHDRAWAL OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read hourly withdrawal data automatically from Data Portal
#------------------------------------------------------------------------------

if(autoread_hourlywithdrawals == 1) {
  # read the online table -----------------------------------------------------
  
  # Apr-2021: col names on line 16, data begins line 17
  withdrawals.hourly.mgd.df0 <- data.table::fread(
    "https://icprbcoop.org/drupal4/products/wma_withdrawals.csv",
    skip = 16,
    header = FALSE,
    stringsAsFactors = FALSE,
    # colClasses = c("character", rep("numeric", 6)), # force cols 2-6 numeric
    na.strings = c("", "#N/A", "NA", -999999),
    data.table = FALSE)
  names(withdrawals.hourly.mgd.df0) <- c("DateTime",
                                         "FW_POT",
                                         "WSSC_POT",
                                         "WA_GF",
                                         "WA_LF",
                                         "LW_POT",
                                         "LW_FW",
                                         "FW_OC",
                                         "WSSC_PA",
                                         "LW_BR")
}

#------------------------------------------------------------------------------
# WITHDRAWAL OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - withdrawal data file resides in /input/ts/current/
#   - file name is wma_withdrawals.csv
#   - data can be downloaded from CO-OP's Data Portal via the "Products" tab
#      - data is expressed as hourly
#      - data extends from 30 days in past to 15 days in future
#      - name appropriately then save the file to /input/ts/current/
#      - can create a longer time series by pasting new data into existing file
#------------------------------------------------------------------------------

if(autoread_hourlywithdrawals == 0) {
  withdrawals.hourly.mgd.df0 <- data.table::fread(
    paste(ts_path, "wma_withdrawals.csv", sep = ""),
    skip = 16,
    header = TRUE,
    stringsAsFactors = FALSE,
    # colClasses = c("character", rep("numeric", 6)), # force cols 2-6 numeric
    na.strings = c("", "#N/A", "NA", -999999),
    data.table = FALSE)
  names(withdrawals.hourly.mgd.df0) <- c("DateTime",
                                         "FW_POT",
                                         "WSSC_POT",
                                         "WA_GF",
                                         "WA_LF",
                                         "LW_POT",
                                         "LW_FW",
                                         "FW_OC",
                                         "WSSC_PA",
                                         "LW_BR")
}
print("finished importing withdrawals")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# DAILY RESERVOIR STORAGE DATA
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Set switch (this has been moved to global)-----------------------------------
# autoread_dailystorage <- 1 # automatic data retrieval from Data Portal
# autoread_dailystorage <- 0 # read data from file in local directory

#------------------------------------------------------------------------------
# DAILY STORAGE OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read daily storage data automatically from Data Portal
#------------------------------------------------------------------------------
# &startdate=07%2F12%2F2021&enddate=07%2F19%2F2021&format=csv&submit=Submit


if(autoread_dailystorage == 1) {
  # read the online data ------------------------------------------------------
  
  # paste together the url for Data Portal's daily storage data 
  url_dailystor0 <- paste("https://icprbcoop.org/drupal4/icprb/data-view?",
  "patuxent_reservoirs_current_usable_storage_wssc=patuxent_reservoirs_current_usable_storage_wssc",
  "&little_seneca_reservoir_current_usable_storage_wssc=little_seneca_reservoir_current_usable_storage_wssc",
  "&occoquan_reservoir_current_usable_storage=occoquan_reservoir_current_usable_storage",
  # "&jrr_current_usable_storage=jrr_current_usable_storage",
  # "&jrr_current_usable_ws_storage=jrr_current_usable_ws_storage",
  # "&sr_current_usable_storage=sr_current_usable_storage", 
  sep = "")
  
  year_temp <- today_year
  day_first <- "01"
  month_first <- "06"
  start_date_string <- paste("&startdate=", month_first, "%2F", 
                             day_first, "%2F",
                             year_temp, "&enddate=", sep="")
  url_dailystor <- paste(url_dailystor0, start_date_string, 
                     today_month, "%2F", 
                     today_day, "%2F", 
                     today_year, "&format=csv&submit=Submit", 
                     sep="")
  
  # the name storage.daily.bg.df0 is used in the legacy sim code (line 566)

  storage_local_daily_bg_df <- data.table::fread(
    url_dailystor,
    skip = 3,
    header = FALSE,
    stringsAsFactors = FALSE,
    # colClasses = c("character", rep("numeric", 6)), # force cols 2-6 numeric
    na.strings = c("", "#N/A", "NA", -999999),
    data.table = FALSE)
  names(storage_local_daily_bg_df) <- c("date_time",
                                         "patuxent",
                                         "seneca",
                                         "occoquan")
  
  storage_nbr_df <- dataRetrieval::readNWISuv(
    siteNumbers = c("01595790", "01597490"),
    parameterCd = "00054",
    startDate = paste0(year_temp, "-06-01"),
    endDate = date_today0,
    tz = "America/New_York"
  ) %>%
    mutate(stor_mg = X_00054_00000*0.3259/1000, date_time = dateTime) %>%
    select(date_time, site_no, stor_mg) %>%
    pivot_wider(names_from = site_no, names_prefix = "X", 
                values_from = stor_mg) %>%
    mutate(jrr_total = X01595790, jrr_ws = 13.1, savage = X01597490,
           hour = lubridate::hour(date_time),
           minute = lubridate::minute(date_time), 
           date = lubridate::date(date_time))
  
  # just grab value at 8 AM for daily data df
  storage_nbr_daily_df <- storage_nbr_df %>%
    filter(hour == 8 & minute == 0) %>%
    select(date, jrr_total, jrr_ws, savage) %>%
    rename(date_time = date)
}

#------------------------------------------------------------------------------
# DAILY STORAGE OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - daily storage data file resides in /input/ts/current/
#   - file name is wma_storage.csv
#   - data can be downloaded from CO-OP's Data Portal via the "Products" tab
#      - data is expressed as daily
#      - data extends from June 1 through the current day
#      - name appropriately then save the file to /input/ts/current/
#      - can create a longer time series by pasting new data into existing file
#------------------------------------------------------------------------------

if(autoread_dailystorage == 0) {
  storage_daily_bg_df0 <- data.table::fread(
    paste(ts_path, "wma_storage.csv", sep = ""),
    skip = 3,
    header = FALSE,
    stringsAsFactors = FALSE,
    # colClasses = c("character", rep("numeric", 6)), # force cols 2-6 numeric
    na.strings = c("", "#N/A", "NA", -999999),
    data.table = FALSE)
  names(storage_daily_bg_df0) <- c("date_time",
                                   "patuxent",
                                   "seneca",
                                   "occoquan",
                                   "jrr_total",
                                   "jrr_ws",
                                   "savage")
}

print("finished importing reservoir storages")
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# DAILY STORAGE DATA
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Set switch (this has been moved to global)-----------------------------------
# autoread_lffs <- 1 # automatic data retrieval from Data Portal
# autoread_lffs <- 0 # read data from file in local directory

#------------------------------------------------------------------------------
# LFFS OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read LFFS data automatically from Data Portal
#------------------------------------------------------------------------------

if(autoread_lffs == 1) {
  
# Read LFFS LFalls online data ------------------------------------------------
lffs.hourly.cfs.all.df0 <- data.table::fread(
  # paste(ts_path, "PM7_4820_0001.flow", sep = ""),
  # from cooplinux1:
  "http://icprbcoop.org/dss_data_exchange/PM7_4820_0001.flow", 
  # from cooplinux2 - a backup source if FEWS Live is not working
  # "http://icprbcoop.org/dss_data_exchange/PM7_4820_0001.flow_s2", 
  skip = 25,
  header = FALSE,
  stringsAsFactors = FALSE,
  colClasses = c(rep("numeric", 6)), # force cols to numeric
  col.names = c("year", "month", "day", "minute", "second", "lfalls_lffs"),
  # na.strings = c("eqp", "Ice", "Bkw", "", "#N/A", "NA", -999999),
  data.table = FALSE) 

}

#------------------------------------------------------------------------------
# LFFS OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - LFFS data file resides in /input/ts/current/
#   - file name is PM7_4820_0001.flow
#   - data can be downloaded from CO-OP's Data Portal
#      - http://icprbcoop.org/dss_data_exchange/PM7_4820_0001.flow"
#      - name appropriately then save the file to /input/ts/current/
#------------------------------------------------------------------------------

if(autoread_lffs == 0) {
  
# Read the local LFFS data file------------------------------------------------
lffs.hourly.cfs.all.df0 <- data.table::fread(
  paste(ts_path, "PM7_4820_0001.flow", sep = ""),
  skip = 25,
  header = FALSE,
  stringsAsFactors = FALSE,
  colClasses = c(rep("numeric", 6)), # force cols to numeric
  col.names = c("year", "month", "day", "minute", "second", "lfalls_lffs"),
  # na.strings = c("eqp", "Ice", "Bkw", "", "#N/A", "NA", -999999),
  data.table = FALSE) 
}

print("finished creating lffs.hourly.cfs.all.df0")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import time series representing state drought status.
#   - temporarily just use time series from 2018drex.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# 
state.drought.df <- data.table::fread(paste(ts_path, "state_drought_status.csv", sep = ""),
                                      data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr::select(date_time, 
                gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
                gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
                gw_md_cent, p_md_cent, sw_md_cent, r_md_cent,
                gw_md_west, p_md_west, sw_md_west, r_md_west,
                region_md_cent, region_md_west
                ) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import reservoir storage data
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Set switch (move this to global?)--------------------------------------------
# autoread_resstorage <- 1 # automatic data retrieval from Data Portal
autoread_resstorage <- 0 # read data from file in local directory

#------------------------------------------------------------------------------
# STORAGE OPTION 1 - AUTOMATIC DATA RETRIEVAL
#   - read daily flow data automatically from Data Portal
#   - start date is January 1 of the current year
#------------------------------------------------------------------------------

if(autoread_resstorage == 1) {
  
  # paste together the url for Data Portal's storage data-------------------
  # https://icprbcoop.org/drupal4/icprb/data-view?patuxent_reservoirs_current_usable_storage_wssc=patuxent_reservoirs_current_usable_storage_wssc&little_seneca_reservoir_current_usable_storage_wssc=little_seneca_reservoir_current_usable_storage_wssc&occoquan_reservoir_current_usable_storage=occoquan_reservoir_current_usable_storage&startdate=01%2F01%2F2020&enddate=11%2F10%2F2020&format=csv&submit=Submit
  url_storage0 <- "https://icprbcoop.org/drupal4/icprb/data-view?"
  urlplus1 <- "patuxent_reservoirs_current_usable_storage_wssc=patuxent_reservoirs_current_usable_storage_wssc"
  urlplus2 <- "&little_seneca_reservoir_current_usable_storage_wssc=little_seneca_reservoir_current_usable_storage_wssc"
  urlplus3 <- "&occoquan_reservoir_current_usable_storage=occoquan_reservoir_current_usable_storage"
    url_storagedaily <- paste(url_storage0, 
                            urlplus1, urlplus2, urlplus3,
                            "&startdate=01%2F01%2F2020&enddate=", 
                     today_month, "%2F", 
                     today_day, "%2F", 
                     today_year, "&format=csv&submit=Submit", 
                     sep="")
  
  # read the online data table-------------------------------------------------
  storage.daily.bg.df0 <- data.table::fread(
    url_storagedaily,
    skip = 1,
    header = FALSE,
    col.names = c("date_time", "stor_pat", "stor_sen", "stor_occ"),
    stringsAsFactors = FALSE,
    na.strings = c("", "#N/A", -999999, "NA"),
    data.table = FALSE) %>%
    dplyr::mutate(date_time = as.Date(date_time)) %>%
    filter(!is.na(date_time)) %>%
    select(date_time, everything()) %>%
    arrange(date_time)

# Now read N Br reservoir data ------------------------------------------------
start_date_resstor <- as.POSIXct("2020-01-01")
end_date_resstor <- as.POSIXct(date_today0)
storage_level_jrr_rt <- dataRetrieval::readNWISuv(siteNumbers = "01595790",
                                      parameterCd = 62615, 
                                      startDate = start_date_resstor,
                                      endDate = end_date_resstor,
                                      tz = "EST" # time zone is Eastern Standard Time
                                      )
storage_level_sav_rt <- dataRetrieval::readNWISuv(siteNumbers = "01597490",
                                                  parameterCd = 62615, 
                                                  startDate = start_date_resstor,
                                                  endDate = end_date_resstor,
                                                  tz = "EST" # time zone is Eastern Standard Time
                                                  )
}
#------------------------------------------------------------------------------
# STORAGE OPTION 2 - READ DATA FROM FILE IN LOCAL DIRECTORY
#   - read daily flow data from file residing in /input/ts/current/
#   - file name is flows_daily_cfs.csv
#   - code set up so that these time series should begin on Jan 1 of current year
#   - daily data can be downloaded from CO-OP's Data Portal
#      - link for manual download is https://icprbcoop.org/drupal4/icprb/flow-data
#      - name appropriately then save the file to /input/ts/current/
#------------------------------------------------------------------------------

if(autoread_resstorage == 0) {
  
  # read the lacal data table--------------------------------------------------
  storage.daily.bg.df0 <- data.table::fread(
    paste(ts_path, "storage_daily_bg.csv", sep = ""),
    skip = 1,
    header = FALSE,
    col.names = c("date_time", "stor_pat", "stor_sen", "stor_occ"),
    # colClasses = c("Date", "numeric", "numeric", "numeric"), 
    stringsAsFactors = FALSE,
    na.strings = c("", "#N/A", -999999),
    data.table = FALSE) %>%
    dplyr::mutate(date_time = as.Date(date_time),
                  stor_pat = as.numeric(stor_pat),
                  stor_sen = as.numeric(stor_sen),
                  stor_occ = as.numeric(stor_occ)) %>%
    filter(!is.na(date_time)) %>%
    select(date_time, everything()) %>%
    arrange(date_time)
}
print("Finished importing reservoir storages")




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import reservoir dfs - this is a temporary fix to get us going in 2019
#   - just importing tables outputted by 2018drex to serve as temporary reservoir data frames
#   - these will be used to initialize the res dfs from date_start to date_today0 (ie today())
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# print("date_today0")
# print(date_today0)
sen.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_sen.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
jrr.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_jrr.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
occ.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_occ.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
pat.ts.df00 <- data.table::fread(paste(ts_path, "drex2018_output_pat.csv", sep = ""),
                                data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date_time)) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)

#----------------------------------------shapefile load----------------------------------
# read map shapefiles in ---------------------
# Luke - CS July 2020: the lines below cause errors,
# but the variables are never used so I'm commenting out
# clipcentral = readOGR(dsn=map_path, layer = "clipcentral")
# western_dslv = readOGR(dsn=map_path, layer = "western_dslv")

#transform map shapefiles  ---------------------
# Luke - CS July 2020: the lines below cause errors,
# but the variables are never used so I'm commenting out
# clipcentral_t <- spTransform(clipcentral, CRS("+init=epsg:4326"))
# western_region_t <- spTransform(western_dslv, CRS("+init=epsg:4326"))
#----------------------------------------------------------------------------------------


#----------------------drought maps updating---------------------------------------------
# Luke - these functions seem to be broken - are hanging up

# calls function to get the latest version of the maryland drought map
# md_drought_map = md_drought_map_func(date_today0)

#####this has been set to 
#this pulls directly from the url
md_drought_map <- 'https://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2019jan31/Drought2020-08-31.png'
#change input to below to pull the map from input directory
#readPNG("input/MD_droughtmap_temp.png")

#calls function to get the latest version of the virginia drought map
#---toggle
##for day to day

# va_drought_map = va_drought_map_func()
#va_drought_map = readPNG("input/VA_droughtmap_temp.png")
# this works in Aug 2021 - CS:
va_drought_map <-'https://deq1.bse.vt.edu/drought/state/images/maps/virginia_drought.png'
##to publish
# project.dir <- rprojroot::find_rstudio_root_file()
# va_drought_map = file.path(project.dir,'/global/images/va_drought_placeholder.png')
#---
#----------------------------------------------------------------------------------------


# #------------------------------
# #load in test data for ten day
# ten_day.df <- data.table::fread(file.path(ts_path, "ten_day_test/ten_day_test.csv", sep=""),
# data.table = FALSE)
# 
# #------------------------------
# #load in data for demands from Sarah's Drupal site (if site is up, otherwise do nothing)
# if(url.exists("https://icprbcoop.org/drupal4/products/coop_pot_withdrawals.csv" == TRUE))
# {demands_raw.df <- data.table::fread("https://icprbcoop.org/drupal4/products/coop_pot_withdrawals.csv",
#                                      data.table = FALSE)}
                                        