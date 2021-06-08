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
# Prepare 1-day LFalls fc, constant lags, daily data
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
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
                  ) %>%
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
# Prepare 1-day LFalls fc's, hourly data
# Sticking to cfs during calc's, 
#    since starting values of variable lags were from NWS - based on cfs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Prepare the hourly data -----------------------------------------------------

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
  dplyr::mutate(lfalls_por_constant_lag = lag(upstr_lfalls_accum, 
                                              lag_hourly_por)) %>%
  # Select the gages of interest 
  dplyr::select(date_time, lfalls, lfalls_por_constant_lag, 
                seneca, goose, monoc_jug, 
                por, upstr_lfalls_accum, wnet_wma_pot)

# Add LFFS data
ops_1day_hourly.df0 <- left_join(ops_1day_hourly.df00,
                                lffs.hourly.mgd.df, by = "date_time") %>%
  mutate(lfalls_lffs_hourly_bfc = lfalls_lffs_hourly_bfc*mgd_to_cfs) %>%
  select(-date, -lfalls_lffs_hourly, lfalls_por_constant_lag,
         -lfalls_lffs_daily, -lfalls_bf_correction,
         -lfalls_obs, -lfalls_lffs_bfc) # these last 2 were daily - for QAing

# Add lagk from POR hourly
klags.df <- fread("input/parameters/klags.csv")
location_up <- "upstr_lfalls_accum"
# location_up <- "por"
location_down <- "lfalls"
por_lagk_df <- variable_lagk(ops_1day_hourly.df0, location_up, location_down, 
                      "por_to_lfalls", "por_to_lfalls_1", klags.df) #%>%
  # dplyr::mutate(lfalls_por_lagk = testing)  %>%
  # dplyr::select(-flow)

# For comparison, also do constant lag = 2.3 days = 54 hours

ops_1day_hourly.df <- left_join(ops_1day_hourly.df0,
                                por_lagk_df, by = "date_time")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# LFall daily predicted from constant lags - first graph on ui ----------------
lfalls_1day.plot1.df <- ops_1day_daily.df %>%
  mutate(lfalls_flowby = lfalls_flowby) %>%
  select(-d_pot_total, -goose) %>%
  gather(key = "site", value = "flow", -date_time)

output$one_day_ops_plot1 <- renderPlot({
  lfalls_1day.plot1.df <- lfalls_1day.plot1.df %>%
  filter(date_time >= input$plot_range[1],
         date_time <= input$plot_range[2]) 
  ggplot(lfalls_1day.plot1.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c("darkorange1", "darkorange3",
                                  "deepskyblue1", "deepskyblue4",
                                  "red",
                                  "purple", "plum", 
                                  "steelblue", "palegreen3")) +
    scale_linetype_manual(values = c("solid", "dashed", "solid",
                                     "dotted", "dashed", "solid",
                                     "solid","solid", "solid")) +
    scale_size_manual(values = c(1, 1, 2, 1, 1, 1, 1, 1, 1)) +
    labs(x = "", y = "MGD")
})


# LFalls hourly predicted from LFFS - second graph on ui ----------------------
lfalls_1day.plot2.df <- ops_1day_hourly.df %>%
  mutate(lfalls_flowby = lfalls_flowby) %>%
  dplyr::select(-seneca, -goose) %>%
  gather(key = "site", value = "flow", -date_time)

output$one_day_ops_plot2 <- renderPlot({
  lfalls_1day.plot2.df <- lfalls_1day.plot2.df %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2]) 
  ggplot(lfalls_1day.plot2.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c( "deepskyblue1", "red", "dodgerblue",
                                  "deepskyblue2", "purple", 
                                  "plum",  "steelblue", "palegreen3",
                                  "palegreen4")) +
    scale_linetype_manual(values = c("solid", "dotted", "solid",
                                     "dashed", "solid",
                                     "solid", "solid", "dashed", 
                                     "solid")) +
    scale_size_manual(values = c(2, 1, 1, 1, 1, 1, 1, 1, 1)) +
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
