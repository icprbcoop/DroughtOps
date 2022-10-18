#******************************************************************
# run_all_offline runs the model outside of Shiny, for QAing
# by running in this way, the data dataframes appear in Environment
#******************************************************************
# First run global.R, which loads packages, paths, functions
# It also runs import_data.R and scripts in /global/data_processing
# Thus, you can inspect most dataframes of interest
source("global.R", local = TRUE)

source("input/parameters/parameters_ops.R", local = TRUE)
source("input/parameters/parameters_physical.R", local = TRUE)
source("input/parameters/css_ui_values.R", local = TRUE)
source("code/global/import_data.R", local = TRUE)
print("finished all imports")
source("code/global/data_processing/process_hourly_flows.R", local = TRUE)
print("finished processing hourly flows")
source("code/global/data_processing/process_withdrawals.R", local = TRUE)
print("finished processing withdrawals")
source("code/global/data_processing/process_daily_flows.R", local = TRUE)
print("finished processing daily flows")
source("code/global/data_processing/process_lffs.R", local = TRUE)
print("finished processing LFFS flows")
source("code/global/data_processing/process_rt_storages.R", local = TRUE)
print("finished processing rt storages")

#******************************************************************
# For 2019 DREX
# date_today0 <- as.Date("2019-11-21")

#-----drought map functions are declared before they are used in import data
source("code/functions/display/md_drought_map_func.R", local = TRUE)
source("code/functions/display/va_drought_map_func.R", local = TRUE)
#------------------------------------------------------------------------------

#-----define parameters and import data ----------------------
# source("config/paths.R", local = TRUE)
source("input/parameters/parameters_ops.R", local = TRUE)
source("input/parameters/parameters_physical.R", local = TRUE)

#******************************************************************
# Code related to the simulation, which is currently broken:

  # Run the main simulation to the hard-coded input, date_today
  #    - ts here is the precursor of the set of reactive values
  ts0 <- list(sen = sen.ts.df0, 
              jrr = jrr.ts.df0, 
              pat = pat.ts.df0,
              occ = occ.ts.df0,
              flows = potomac.ts.df0)
  ts <- sim_main_func(date_today, ts0)
  #
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After initial main run, the last date is ", date_last))
  # Now rerun, just as in the Shiny model
  ts <- sim_main_func(date_today, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After second main run, the last date is ", date_last))
    #
  # Now add chunks of days twice
  chunkofdays <- 7
  ts <- sim_add_days_func(chunkofdays, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After adding 1st chunkofdays, the last date is ", date_last))
  ts <- sim_add_days_func(chunkofdays, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After adding 2nd chunkofdays, the last date is ", date_last))
  #
  # Now write some output
  write.csv(ts$flows, paste(ts_output, "offline_flows.csv"))
  write.csv(ts$sen, paste(ts_output, "offline_sen.csv"))
  write.csv(ts$jrr, paste(ts_output, "offline_jrr.csv"))
  write.csv(ts$occ, paste(ts_output, "offline_occ.csv"))
  write.csv(ts$pat, paste(ts_output, "offline_pat.csv"))
  #
  # The End