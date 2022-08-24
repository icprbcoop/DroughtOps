# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Load R packages, define classes and functions, and create time series df's
# Info found online:
#   "global.R is a script that is executed before
#    the application launch. For this reason, it can include the same
#    pieces of code to execute the reactive independent processes,
#    but it also has an additional capability: the objects 
#    generated in global.R can be used both in server.R and ui.R"
# Here global.R does all time series (ts) data importing & processing
# *****************************************************************************
# INPUTS - NA
# *****************************************************************************

# *****************************************************************************
# OUTPUTS - NA
# *****************************************************************************

# *****************************************************************************
# NOTES
# Search needs_turn_of_year_update at beginning of new year
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Take care of preliminaries
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Load packages ---------------------------------------------------------------  
# Use this one when not publishing to shinyapp.io, comment out when publishing:
source("code/global/load_packages.R", local = TRUE)
# Use this one when publishing, can comment out otherwise:
# source("code/global/import_packages.R", local = TRUE)

# Set paths -------------------------------------------------------------------
ts_path <- "input/ts/current/" # path for data in local directory
parameters_path <- "input/parameters/"
ts_output <- "output/" # path of output directory
map_path <- "data/Drought_Regions" #MD shapefiles

# Set "today's" date ----------------------------------------------------------
# date_today0 <- as.Date(today(), tz = "America/New_York")
date_today0 <- today()
# date_today0 <- as.Date("2022-05-13")
# date_today0 <- force_tz(date_today0, tzone = "America/New_York")
time_now0 <- Sys.time()

# DREX switch: set to 1 if drought exercise, 0 if not
DREX <- 0

# Set DATA OPTIONS SWITCHES (used in import_data.R)----------------------------
# This can be useful for drought exercises (DREX) 
#    where factors can be applied to rt data to simulate drought conditions
#    or in case there are data problems - can edit local files by hand
# To write downloaded input time series to ts_path, run app and then
#    press "Write... to input dir" on LHS panel
# Set switches to:
#   1's to download online data
#   0's to read from local files in ts_path/

# local files are also useful to allow the app to run (not crash), 
#    if an online data source is not available,
#    or if you want to do some minor edits to data!

autoread_dailyflows <- 1 # change to 0 if online data not available SOMETHING WRONG WHEN 0
autoread_hourlyflows <- 1 # change to 0 if online data not available
autoread_hourlywithdrawals <- 1 # change to 0 if online data not available
autoread_dailystorage <- 1 # change to 0 if online data not available
autoread_lffs <- 1 # change to 0 if online data not available

# Switch to use public or beta withdrawal forecast file (in import_data.R)
withdr_file <- 1 # 1 if public or 2 if beta-private

# ******************************************************************************
# ******************************************************************************
# Temporary for 2020 DREX
# 1. Set all switches (except storage) to 1 and run app to fetch real-time data
# 2. Run write_data.R to write new data into input/ts/current/

# date_today0 <- as.Date("2020-09-15") # 2020_drex_day1
# ts_path <- "input/ts/2020_drex_day1_Sep15/" # for 2020 DREX
# # 
# date_today0 <- as.Date("2020-09-18") # 2020_drex_day2
# ts_path <- "input/ts/2020_drex_day2_Sep18/" # for 2020 DREX
# #
# date_today0 <- as.Date("2020-09-20") # 2020_drex_day3
# ts_path <- "input/ts/2020_drex_day3_Sep20/" # for 2020 DREX
# # 
# # (these data source switches are ordinarily set above)
# #   - 1's to download online data
# #   - 0's to read from ts/path/
# autoread_dailyflows <- 0
# autoread_hourlyflows <- 0
# autoread_hourlywithdrawals <- 0
# autoread_resstorage <- 0
# autoread_lffs <- 0
# ******************************************************************************
# ******************************************************************************

# Read classes and functions --------------------------------------------------
source("code/functions/data_processing/recess_daily_flows.R", local = TRUE)
source("code/functions/data_processing/compute_verification_metrics.R", local = TRUE)
source("code/functions/data_processing/date_standards_func.R", local = TRUE)
source("code/functions/data_processing/variable_lagk_func.R", local = TRUE)
source("code/classes/reservoir_class.R", local = TRUE)
source("code/functions/reservoir_ops/reservoir_ops_init_func.R", local = TRUE)
source("code/functions/reservoir_ops/reservoir_ops_today_func.R", local = TRUE)
source("code/functions/reservoir_ops/jrr_reservoir_ops_today_func.R", local = TRUE)
source("code/functions/reservoir_ops/jrr_reservoir_ops_today_func2.R", local = TRUE)
source("code/functions/simulation/forecasts_demands_func.R", local = TRUE)
source("code/functions/simulation/forecasts_flows_func.R", local = TRUE)
source("code/functions/state/state_indices_update_func.R", local = TRUE)
source("code/functions/simulation/estimate_need_func.R", local = TRUE)
source("code/functions/simulation/restriction_flow_benefits_func.R", local = TRUE)
source("code/functions/simulation/sim_main_func.R", local = TRUE)
source("code/functions/simulation/simulation_func.R", local = TRUE)
source("code/functions/simulation/sim_add_days_func.R", local = TRUE)
source("code/functions/simulation/rule_curve_func.R", local = TRUE)
source("code/functions/simulation/nbr_rule_curve_func.R", local = TRUE)
source("code/functions/display/display_graph_res_func.R", local = TRUE)
source("code/functions/data_get/get_hourly_flows_func.R", local = TRUE)
source("code/functions/display/md_drought_map_func.R", local = TRUE)
source("code/functions/display/va_drought_map_func.R", local = TRUE)
source("code/functions/display/date_func.R", local = TRUE)
source("code/functions/display/warning_color_func.R", local = TRUE)
# Luke: are the next 2 lines obsolete?
# this is a lazy Friday fix that should be changed later:
source("code/functions/display/warning_color_map_func.R", local = TRUE)

# Read input parameters -------------------------------------------------------
source("input/parameters/parameters_ops.R", local = TRUE)
source("input/parameters/parameters_physical.R", local = TRUE)
source("input/parameters/css_ui_values.R", local = TRUE)

# Import time series data and do some processing-------------------------------
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

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Create things that need to be accessed by simulation code
# (Note, June 2021, sim code no longer works - needs to be redone)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Make reservoir objects and time series df's ---------------------------------
#   - the reservoir objects are jrr, sen, occ, pat
#   - the ts dfs are 
source("code/server/simulation/reservoirs_make.R", local = TRUE) 
# What this does is create the reservoir "objects", jrr, sen, occ, pat
#    and the reservor time series, res.ts.df
#    e.g., sen.ts.df - initialized with first day of ops time series

# Make the Potomac input data and flow time series dataframes -----------------
source("code/server/simulation/potomac_flows_init.R", local = TRUE)
# What this does is create:
# potomac.data.df - filled with all nat flow, trib flow data
# potomac.ts.df - initialized with first day of flows
#    - contains lfalls_obs, sen_outflow, jrr_outflow

# Make and initialize state drought status time series dataframes -------------
source("code/server/simulation/state_status_ts_init.R", local = TRUE)
# What this does is create:
# state.ts.df - filled with status indices:
#    - 0 = Normal
#    - 1 = Watch
#    - 0 = Warning
#    - 0 = Emergency


