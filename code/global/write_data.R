# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script writes the imported time series data to files for future use.
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.cfs.df0
# flows.hourly.cfs.df0
# withdrawals.hourly.mgd.df0
# storage.daily.bg.df0 - Pat, Sen, Occ storages from Data Portal, from Jan 1
# jrr.rt.df0
# sav.rt.df0

# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows_daily_cfs.csv
# flows_hourly_cfs.csv
# wma_withdrawals.csv
# storage_daily_bg.csv
# PM7_4820_0001.flow

# *****************************************************************************

# Set path---------------------------------------------------------------------
path_out_temp <- "input/ts/current/"

# Write downloaded ts input files to local directory---------------------------
flows_daily_temp.df0 <- flows.daily.cfs.df0 %>%
  dplyr::rename(date = date_time)
write_csv(flows_daily_temp.df0, paste(path_out_temp, "flows_daily_cfs.csv",
                                      sep=""))

flows_hourly_temp.df0 <- flows.hourly.cfs.df0 %>%
  dplyr::rename(date = date_time)
write_csv(flows_hourly_temp.df0, paste(path_out_temp, "flows_hourly_cfs.csv",
                                       sep=""))

withdrawals_hourly_temp.df0 <- withdrawals.hourly.mgd.df0
write_csv(withdrawals_hourly_temp.df0, paste(path_out_temp, 
                                             "wma_withdrawals.csv", sep=""))

lffs_hourly

# storage_temp.df0 <- storage.daily.bg.df0
# write_csv(storage_temp.df0, paste(path_out_temp,
#                                              "storage_daily_bg.csv", sep=""))

