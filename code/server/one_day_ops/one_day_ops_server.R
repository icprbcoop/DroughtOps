# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Create graphs and values displayed for 1-day operations
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.mgd.df - current daily streamflow data
# lffs.daily.bfc.mgd.df - LFFS daily baseflow-corrected flows
#
# flows_hourly_cfs.csv - current hourly streamflow data in cfs
# withdrawals_hourly_mgd_df - WMA supplier hourly withdrawal data
#                  - past 30 days and future 14 days
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# flows_hourly_mgd.csv- current daily streamflow data in mgd
#
# For display on 1-Day Ops page
#   Plots:
#   - output$one_day_ops_plot - graph of LFalls observed & forecasted flows
#   Value boxes:
#   - output$lfalls_1day_fc - LFalls forecast
#   - output$wma_withdr_1day_fc - WMA Potomac withdrawal 1-day forecast
#   - output$1day_deficit - estimated need at LFalls tomorrow
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Prepare 1-day LFalls fc's, constant lags, daily data, in mgd
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# ops_1day_daily.df columns include:
#   - date_time (format is Date)
#   - lfalls (obs), mgd
#   - lfalls_fc_prrism (constant lag), mgd
#   - lfalls_lffs_bfc (LFFS base flow corrected), mgd

lag_daily_por <- 1
lag_daily_sen <- 1 
ops_1day_daily.df0 <- flows.daily.mgd.df %>%
  dplyr::select(date_time, lfalls, seneca, goose, 
                monoc_jug, por, d_pot_total, w_wa_lf) %>%
  # dplyr::mutate(lfalls_fc_constant_lags = 
  #                 lag(seneca, lag_sen) + lag(goose, lag_sen) + 
  #                 lag(monoc_jug, lag_por) + lag(por, lag_por) -
  #                 d_pot_total) %>%
  dplyr::mutate(lfalls_fc_prrism = 
                  lag(lfalls, 1) +
                  lag(por, lag_daily_por+1) - lag(por, lag_daily_por) +
                  lag(monoc_jug, lag_daily_por+1) - lag(monoc_jug, lag_daily_por) + 
                  lag(seneca, lag_daily_sen+1) - lag(seneca, lag_daily_sen) +
                  lag(goose, lag_daily_sen+1) - lag(goose, lag_daily_sen)
                  - d_pot_total) %>%
  dplyr::mutate(gfalls = lead(lfalls, 1)*15/24 + lfalls*9/24
                + w_wa_lf, 
                gfalls_flowby = 300) %>%
  select(-w_wa_lf) # now 8

# add LFFS-bflow corrected daily 
ops_1day_daily.df <- left_join(ops_1day_daily.df0, lffs.daily.bfc.mgd.df,
                               by = "date_time") %>%
  select (-lfalls_obs, -lfalls_lffs_daily, -lfalls_bf_correction) # now 9
                                                       
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Prepare 1-day LFalls fc's, hourly data, in cfs
# [Sticking to cfs during calc's, 
#    since starting values of variable lags were from NWS - based on cfs]
# *****************************************************************************
# NOTES on status of lfalls_flow_accum_klag:
#    This gives a very nice prediction!
#    BUT, if flow ~ 5000 cfs, it can only predict 18 hrs into future :(
#    [During very low flows it would do much better - more than 48 hrs.]
#    So need to think about how to verify 1 and 2 day predictions.
#    Also, still need to add LFalls correction since POR underpredicts LFalls
#       when flows get very low.
#    May want to use Shepherdstown + Millville instead of POR.
#******************************************************************************
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# ops_1day_hourly.df columns include:
#   - date_time
#   - lfalls (obs), cfs
#   - lfalls_por_constant_lag, cfs (but remember different from PRRISM)
#   - lfalls_fc_prrism (constant lag), mgd
#   - lfalls_lffs_hourly_bfc (LFFS base flow corrected), cfs
#------------------------------------------------------------------------------

# Gather the hourly data into ops_1day_hourly.df00-----------------------------

# Function to convert flows to mgd - applied just before graphing & value boxes 
func_cfs_to_mgd <- function(cfs) {round(cfs/mgd_to_cfs,0)}

# The number of columns in the hourly table depends on the datq source.
ncol_hourly_flows <- length(flows.hourly.cfs.df[1,])

# For comparison with lagk, add constant lag = 2.33 days = 54 hours
lag_hourly_por <- 54

# flows.hourly.cfs.df <- flows.hourly.cfs.df %>%
  # dplyr::mutate_at(2:32, func_cfs_to_mgd) %>%
  # dplyr::mutate_at(2:ncol_hourly_flows, func_cfs_to_mgd) %>%
  # dplyr::mutate(date = as.Date(round_date(date_time, unit = "days"))) 

# Add Potomac withdrawals
# demands.df <- demands.daily.df %>%
#   dplyr::mutate(date = date_time) %>%
#   dplyr::select(-date_time)

ops_1day_hourly.df00 <- left_join(flows.hourly.cfs.df, 
                                 withdrawals_hourly_mgd_df, 
                                 by = "date_time") %>%
  # compute accumulated flow above lfalls
  dplyr::mutate(upstr_lfalls_accum = por + monoc_jug 
                + seneca + goose) %>%
  # # Don't subtract withdr's while testing - ts too short
  # dplyr::mutate(lfalls_accum = lfalls_accum 
  #               - wnet_wma_pot*mgd_to_cfs) %>%
  
  # For comparison, add constant lag = 2.33 days = 54 hours
  dplyr::mutate(lfalls_accum_constant_lag = lag(upstr_lfalls_accum, 
                                              lag_hourly_por)) %>%
  # Select the gages of interest 
  dplyr::select(date_time, lfalls, lfalls_accum_constant_lag, 
                seneca, goose, monoc_jug, 
                por, upstr_lfalls_accum, wnet_wma_pot)

# Add LFFS fc------------------------------------------------------------------
ops_1day_hourly.df0 <- left_join(ops_1day_hourly.df00,
                                lffs.hourly.mgd.df, by = "date_time") %>%
  mutate(lfalls_lffs_hourly_bfc = lfalls_lffs_hourly_bfc*mgd_to_cfs) %>%
  select(-lfalls_lffs_hourly, lfalls_accum_constant_lag,
         -lfalls_lffs_daily, -lfalls_bf_correction,
         -lfalls_obs, -lfalls_lffs_bfc) # these last 2 were daily - for QAing

# Add lagk hourly fc from upstr_lfalls_accum-----------------------------------

# I'm keeping the function, variable_lagk, fairly general so that in the future
#    we could apply to successive reaches
klags.df <- fread("input/parameters/klags.csv")
location_up <- "upstr_lfalls_accum"
# location_up <- "por"
location_down <- "lfalls"
por_lagk_df <- variable_lagk(ops_1day_hourly.df0, location_up, location_down, 
                      "por_to_lfalls", "por_to_lfalls_1", klags.df) %>%
  dplyr::rename(lfalls_flow_accum_klag = lfalls)

# For comparison, also compute fc from constant lag = 2.3 days = 54 hrs--------
ops_1day_hourly.df <- left_join(ops_1day_hourly.df0,
                                por_lagk_df, by = "date_time") %>%
  dplyr::select(-upstr_lfalls_accum)
# the klag procedure can produce missing times, so interpolate
ops_1day_hourly.df$lfalls_flow_accum_klag <- 
  na.approx(ops_1day_hourly.df$lfalls_flow_accum_klag, na.rm = FALSE)

# Compute lagk DAILY fc from upstr_lfalls_accum--------------------------------
# This includes subtraction of withdrawals
#    for verification data and calculations of deficits in value boxes
# It also includes "LFalls correction" like PRRISM, because 
#    when flows are very low POR+tribs overpredicts LFalls
# For means, na.rm = FALSE is default, so get NA when not a whole day of data
por_klag_daily_mgd_df <- ops_1day_hourly.df %>%
  dplyr::filter(date > date_today0 - 30) %>%
  dplyr::select(date, lfalls, lfalls_accum_constant_lag, 
         wnet_wma_pot, lfalls_flow_accum_klag) %>%
  dplyr::mutate(wnet_wma_pot = wnet_wma_pot*mgd_to_cfs) %>%
  # change to mgd for daily graph & value boxes
  dplyr::mutate(across(where(is.numeric), func_cfs_to_mgd)) %>%
  dplyr::mutate(lfalls_daily_accum_klag = lfalls_flow_accum_klag 
                - wnet_wma_pot) %>%
  group_by(date) %>%
  summarize(across(where(is.numeric), mean)) %>%
              ungroup() %>%
  # the following does a PRRISM-type LFalls correction:
  dplyr::mutate(date_time = as.Date(date),
                lfalls_klag_corrected = lag(lfalls, n=1) # lfalls yesterday
                + lfalls_daily_accum_klag # diff btwn today & yesterday's lagk
                - lag(lfalls_daily_accum_klag, n=1),
                lfalls_daily = lfalls) %>%
  dplyr::select(date_time, lfalls_daily, lfalls_klag_corrected)
 
# Create today's recent and forecasted daily flows for archiving---------------
klag.daily.fc.mgd.df <- por_klag_daily_mgd_df %>%
  dplyr::rename(date = date_time,
                lfalls = lfalls_klag_corrected) %>%
  dplyr::mutate(date_fc = date_today0,
                length_fc = as.integer(date - date_fc))  %>%
  dplyr::select(-lfalls_daily)

prrism.daily.fc.mgd.df <- ops_1day_daily.df %>%
  dplyr::select(date_time, lfalls_fc_prrism) %>%
  dplyr::rename(date = date_time,
                lfalls = lfalls_fc_prrism) %>%
  dplyr::mutate(date_fc = date_today0,
                length_fc = as.integer(date - date_fc)) 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# DAILY LFalls predicted from constant lags - first graph on ui ---------------
lfalls_1day.plot1.df <- left_join(ops_1day_daily.df, por_klag_daily_mgd_df,
                                  by = "date_time") %>%
  mutate(lfalls_flowby = lfalls_flowby) %>%
  select(-d_pot_total, -goose, -por, -lfalls_daily) %>%
  gather(key = "site", value = "flow", -date_time)

output$one_day_ops_plot1 <- renderPlot({
  lfalls_1day.plot1.df <- lfalls_1day.plot1.df %>%
  filter(date_time >= input$plot_range[1],
         date_time <= input$plot_range[2]) 
  ggplot(lfalls_1day.plot1.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c( "steelblue", "darkorange1",
                                  "deepskyblue1", "deepskyblue4",
                                   "red",
                                  "magenta1",
                                  "purple", "plum", 
                                  "palegreen3",
                                  "blue4")) +
    scale_linetype_manual(values = c("solid", "dashed", 
                                     "solid", "dotted", "dashed",
                                     "solid", "solid", "solid",
                                     "solid", "solid")) +
    scale_size_manual(values = c(0.5, 1, 2, 1, 1, 0.5, 0.5, 1, 1, 1)) +
    labs(x = "", y = "MGD")
})


# HOURLY LFalls predicted from LFFS - second graph on ui ----------------------
lfalls_1day.plot2.df <- ops_1day_hourly.df %>%
  mutate(lfalls_flowby = lfalls_flowby) %>%
  dplyr::select(-date, -goose, -por) %>%
  gather(key = "site", value = "flow", -date_time)

output$one_day_ops_plot2 <- renderPlot({
  lfalls_1day.plot2.df <- lfalls_1day.plot2.df %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2]) 
  ggplot(lfalls_1day.plot2.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c( "deepskyblue1", "darkorange1", "magenta1",
                                   "red", "purple",
                                  "plum",  "palegreen3", "red", 
                                  "palegreen4")) +
    scale_linetype_manual(values = c("solid", "dotted", "solid",
                                     "dashed", "solid",
                                     "solid", "solid", "solid", 
                                     "solid")) +
    scale_size_manual(values = c(2, 0.5, 0.5, 1, 0.5, 1, 1, 1, 1)) +
  labs(x = "", y = "MGD")
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct value box content
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

date_time_1dayhence = date_today0 + 1

# LFalls 1-day fc1 - PRRISM algorithm, daily data ----------------------------
lfalls_1day_fc1_mgd <- ops_1day_daily.df %>%
  filter(date_time == date_time_1dayhence)
lfalls_1day_fc1_mgd <- round(
  lfalls_1day_fc1_mgd$lfalls_fc_prrism[1], 0)
lfalls_1day_fc1_cfs <- round(lfalls_1day_fc1_mgd*mgd_to_cfs, 0)
output$lfalls_1day_fc1 <- renderValueBox({
  lfalls_1day_fc1 <- paste(
    "Forecasted daily flow at Little Falls in 1 day: ",
                          lfalls_1day_fc1_mgd, " MGD (",
                          lfalls_1day_fc1_cfs, " cfs)",
                          sep = "")
  valueBox(
    value = tags$p(lfalls_1day_fc1, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Little Falls 1-day deficit1--------------------------------------------------
lfalls_1day_deficit1_mgd <- estimate_need_func(
  lfalls_flow = lfalls_1day_fc1_mgd,
  mos = mos_1day0
)
lfalls_1day_deficit1_cfs <- round(lfalls_1day_deficit1_mgd*mgd_to_cfs, 0)

output$lfalls_1day_deficit1 <- renderValueBox({
  lfalls_1day_def1 <- paste(
    "Forecasted deficit at Little Falls in 1 day: ",
    lfalls_1day_deficit1_mgd, " MGD (",
    lfalls_1day_deficit1_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_1day_def1, style = "font-size: 50%;"),
    subtitle = NULL,
    # LukeV: change color to orange if value > 0
    color = "light-blue"
  )
})

# LFalls 1-day fc2 - LFFS with "baseflow correction" --------------------------
lfalls_1day_fc2_mgd <- lffs.daily.bfc.mgd.df %>%
  filter(date_time == date_time_1dayhence)
lfalls_1day_fc2_mgd <- round(
  lfalls_1day_fc2_mgd$lfalls_lffs_bfc[1], 0)
lfalls_1day_fc2_cfs <- round(lfalls_1day_fc2_mgd*mgd_to_cfs, 0)
output$lfalls_1day_fc2 <- renderValueBox({
  lfalls_1day_fc2 <- paste(
    "Forecasted flow at Little Falls in 1 day: ",
    lfalls_1day_fc2_mgd, " MGD (",
    lfalls_1day_fc2_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_1day_fc2, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Little Falls 1-day deficit2
lfalls_1day_deficit2_mgd <- estimate_need_func(
  lfalls_flow = lfalls_1day_fc2_mgd,
  mos = mos_1day0
)
lfalls_1day_deficit2_cfs <- round(lfalls_1day_deficit2_mgd*mgd_to_cfs, 0)

output$lfalls_1day_deficit2 <- renderValueBox({
  lfalls_1day_def2 <- paste(
    "Forecasted deficit at Little Falls in 1 day: ",
    lfalls_1day_deficit2_mgd, " MGD (",
    lfalls_1day_deficit2_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_1day_def2, style = "font-size: 50%;"),
    subtitle = NULL,
    # LukeV: change color to orange if value > 0
    color = "light-blue"
  )
})

#********************************************

# LFalls 0-day fc1 - PRRISM algorithm, daily data ----------------------------
lfalls_0day_fc1_mgd <- ops_1day_daily.df %>%
  filter(date_time == date_today0)
lfalls_0day_fc1_mgd <- round(
  lfalls_0day_fc1_mgd$lfalls_fc_prrism[1], 0)
lfalls_0day_fc1_cfs <- round(lfalls_0day_fc1_mgd*mgd_to_cfs, 0)
output$lfalls_0day_fc1 <- renderValueBox({
  lfalls_0day_fc1 <- paste(
    "Forecasted daily flow at Little Falls today: ",
    lfalls_0day_fc1_mgd, " MGD (",
    lfalls_0day_fc1_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_0day_fc1, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Little Falls 0-day deficit1
lfalls_0day_deficit1_mgd <- estimate_need_func(
  lfalls_flow = lfalls_0day_fc1_mgd,
  mos = mos_0day0
)
lfalls_0day_deficit1_cfs <- round(lfalls_0day_deficit1_mgd*mgd_to_cfs, 0)

output$lfalls_0day_deficit1 <- renderValueBox({
  lfalls_0day_def1 <- paste(
    "Forecasted deficit at Little Falls today: ",
    lfalls_0day_deficit1_mgd, " MGD (",
    lfalls_0day_deficit1_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_0day_def1, style = "font-size: 50%;"),
    subtitle = NULL,
    # LukeV: change color to orange if value > 0
    color = "light-blue"
  )
})

# LFalls 0-day fc2 - LFFS with "baseflow correction" --------------------------
lfalls_0day_fc2_mgd <- lffs.daily.bfc.mgd.df %>%
  filter(date_time == date_today0)
lfalls_0day_fc2_mgd <- round(
  lfalls_0day_fc2_mgd$lfalls_lffs_bfc[1], 0)
lfalls_0day_fc2_cfs <- round(lfalls_0day_fc2_mgd*mgd_to_cfs, 0)
output$lfalls_0day_fc2 <- renderValueBox({
  lfalls_0day_fc2 <- paste(
    "Forecasted flow at Little Falls today: ",
    lfalls_0day_fc2_mgd, " MGD (",
    lfalls_0day_fc2_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_0day_fc2, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Little Falls 0-day deficit2
lfalls_0day_deficit2_mgd <- estimate_need_func(
  lfalls_flow = lfalls_0day_fc2_mgd,
  mos = mos_0day0
)
lfalls_0day_deficit2_cfs <- round(lfalls_0day_deficit2_mgd*mgd_to_cfs, 0)

output$lfalls_0day_deficit2 <- renderValueBox({
  lfalls_0day_def2 <- paste(
    "Forecasted deficit at Little Falls today: ",
    lfalls_0day_deficit2_mgd, " MGD (",
    lfalls_0day_deficit2_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_0day_def2, style = "font-size: 50%;"),
    subtitle = NULL,
    # LukeV: change color to orange if value > 0
    color = "light-blue"
  )
})
