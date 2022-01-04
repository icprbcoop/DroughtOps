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
# Prepare 1-day LFalls daily fc's, constant lags, in mgd
#------------------------------------------------------------------------------

# ops_1day_daily.df columns include:
#   - date_time (format is Date)
#   - lfalls (obs), mgd
#   - lfalls_fc_prrism (constant lag), mgd
#   - lfalls_lffs_bfc (LFFS base flow corrected), mgd

# 1-day fc as computed by PRRISM (constant lags)-------------------------------
#   - actually PRRISM is slightly more complex, assuming  2.3 day lag

lag_daily_por <- 2
lag_daily_sen <- 2 
w_wa_lf_default <- 0 # need this to calculate w_wa_gf
ops_1day_daily.df0 <- flows.daily.mgd.df %>%
  dplyr::select(date_time, lfalls, seneca, goose, 
                monoc_jug, por, w_pot_total_net, w_wa_lf) %>%
  dplyr::mutate(w_pot_total_net = case_when(
    # seems that case_when feels input$'s are integer
    # is.na(w_pot_total_net) == TRUE ~ 1.000001*input$default_w_pot_net,
    is.na(w_pot_total_net) == TRUE ~ 1.000001*400,
    is.na(w_pot_total_net) == FALSE ~ w_pot_total_net,
    TRUE ~ -9999.9),
    # this first try was wrong:
    # lfalls_fc_prrism =
    #               lag(lfalls, 1) +
    #               lag(por, lag_daily_por+1) - lag(por, lag_daily_por) +
    #               lag(monoc_jug, lag_daily_por+1) - lag(monoc_jug, lag_daily_por) +
    #               lag(seneca, lag_daily_sen+1) - lag(seneca, lag_daily_sen) +
    #               lag(goose, lag_daily_sen+1) - lag(goose, lag_daily_sen),
    # this matches 2025 WS study, Eq. 5-4, p. 5-8
    lfalls_fc_prrism =
      lag(lfalls, 1) +
      lag(por, lag_daily_por) - lag(por, lag_daily_por + 1) +
      lag(monoc_jug, lag_daily_por) - lag(monoc_jug, lag_daily_por + 1) +
      lag(seneca, lag_daily_sen) - lag(seneca, lag_daily_sen + 1) +
      lag(goose, lag_daily_sen) - lag(goose, lag_daily_sen + 1),
    w_wa_lf = w_wa_lf_default
    ) %>%
  dplyr::mutate(gfalls = lead(lfalls, 1)*15/24 + lfalls*9/24
                + w_wa_lf, 
                gfalls_flowby = 300) %>%
  dplyr::select(-w_wa_lf)

# add LFFS-bflow corrected daily-----------------------------------------------
ops_1day_daily.df <- left_join(ops_1day_daily.df0, lffs.daily.bfc.mgd.df,
                               by = "date_time") %>%
  select (-lfalls_obs, -lfalls_lffs_daily, -lfalls_bf_correction)
                                                       
#------------------------------------------------------------------------------
# Prepare 1-day LFalls hourly fc's, in cfs
#------------------------------------------------------------------------------

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

# provides upstr_lfalls_accum and lfalls_lffs_hourly_bfc
ops_1day_hourly.df00 <- left_join(flows.hourly.cfs.df, 
                                 withdrawals_hourly_mgd_df, 
                                 by = "date_time") %>%
  # compute accumulated flow above lfalls
  # don't subtract withdr's - most of route is pre-intakes
  dplyr::mutate(upstr_lfalls_accum = por + monoc_jug 
                + seneca + goose) %>%
  
  # For QAing, add constant lag = 2.33 days = 54 hours
  dplyr::mutate(upstr_lfalls_accum_constant_lag = lag(upstr_lfalls_accum,
                                              lag_hourly_por)) %>%
  # Select the gages of interest 
  dplyr::select(date_time, lfalls, 
                seneca, goose, monoc_jug, upstr_lfalls_accum_constant_lag,
                por, upstr_lfalls_accum, wnet_wma_pot)

# Add LFFS fc------------------------------------------------------------------
ops_1day_hourly.df0 <- left_join(ops_1day_hourly.df00,
                                lffs.hourly.mgd.df, by = "date_time") %>%
  mutate(lfalls_lffs_hourly_bfc = lfalls_lffs_hourly_bfc*mgd_to_cfs) %>%
  select(-lfalls_lffs_hourly, 
         -lfalls_lffs_daily, -lfalls_bf_correction,
         -lfalls_obs, -lfalls_lffs_bfc) # these last 2 were daily - for QAing

# Add lagk hourly fc from upstr_lfalls_accum-----------------------------------
klags.df <- fread("input/parameters/klags.csv")
location_up <- "upstr_lfalls_accum"
# location_up <- "por"
location_down <- "lfalls"
por_lagk_df <- variable_lagk(ops_1day_hourly.df0, location_up, location_down, 
                      "por_to_lfalls", "por_to_lfalls_1", klags.df) %>%
  dplyr::rename(lfalls_flow_accum_klag = lfalls)

# Add constant lag fc, lag = 2.3 days = 54 hrs---------------------------------
ops_1day_hourly.df <- left_join(ops_1day_hourly.df0,
                                por_lagk_df, by = "date_time") %>%
  dplyr::select(-upstr_lfalls_accum)

# The klag procedure can produce missing times, so interpolate-----------------
ops_1day_hourly.df$lfalls_flow_accum_klag <- 
  na.approx(ops_1day_hourly.df$lfalls_flow_accum_klag, na.rm = FALSE)

#------------------------------------------------------------------------------
# Compute lagk DAILY fc from upstr_lfalls_accum--------------------------------
# This includes subtraction of withdrawals
#    for verification data and calculations of deficits in value boxes
# It also includes "LFalls correction" like PRRISM, because 
#    when flows are very low POR+tribs overpredicts LFalls
# For means, na.rm = FALSE is default, so get NA when not a whole day of data
por_klag_daily_mgd_df <- reactive({
  ops_1day_hourly.df %>%
  dplyr::filter(date > date_today0 - 150) %>%
  dplyr::select(date, lfalls, 
         wnet_wma_pot, lfalls_flow_accum_klag) %>%
    
  # Fill in missing wnet_wma_pot's with reactive default value
  dplyr::mutate(wnet_wma_pot = case_when(
    # seems that case_when feels input$'s are integer
    is.na(wnet_wma_pot) == TRUE ~ 1.00001*input$default_w_pot_net,
    is.na(wnet_wma_pot) == FALSE ~ wnet_wma_pot, 
    TRUE ~ -9999.9)) %>%
    
  dplyr::mutate(wnet_wma_pot = wnet_wma_pot*mgd_to_cfs) %>%
  # change to mgd for daily graph & value boxes
  dplyr::mutate(across(where(is.numeric), func_cfs_to_mgd)) %>%
  dplyr::mutate(lfalls_daily_accum_klag = lfalls_flow_accum_klag 
                - wnet_wma_pot) %>%
  group_by(date) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
              ungroup() %>%
  # the following does a PRRISM-type LFalls correction:
  dplyr::mutate(date_time = as.Date(date),
                # do correction based on error yesterday
                lfalls_klag_corrected = lfalls_daily_accum_klag 
                + lag(lfalls, n=2) # lfalls obs 2 days ago
                - lag(lfalls_daily_accum_klag, n=2), # lfalls sim 2 days ago
                lfalls_daily = lfalls) %>%
  dplyr::select(date_time, lfalls_daily, lfalls_klag_corrected)
})
# ********************************************************
# non-reactive just for QA'ing
por_klag_daily_mgd_df_QA <- ops_1day_hourly.df %>%
    dplyr::filter(date > date_today0 - 150) %>%
    dplyr::select(date, lfalls, 
                  wnet_wma_pot, lfalls_flow_accum_klag) %>% 
    
    # Fill in missing wnet_wma_pot's with reactive default value
    dplyr::mutate(wnet_wma_pot = case_when(
      # seems that case_when feels input$'s are integer
      is.na(wnet_wma_pot) == TRUE ~ 1.00001*400,
      is.na(wnet_wma_pot) == FALSE ~ wnet_wma_pot, 
      TRUE ~ -9999.9)) %>%
    
    dplyr::mutate(wnet_wma_pot = wnet_wma_pot*mgd_to_cfs) %>%
    # change to mgd for daily graph & value boxes
    dplyr::mutate(across(where(is.numeric), func_cfs_to_mgd)) %>%
    dplyr::mutate(lfalls_daily_accum_klag = lfalls_flow_accum_klag 
                  - wnet_wma_pot) %>%
    group_by(date) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
    ungroup() %>%
  
    # the following does a PRRISM-type LFalls correction:
    dplyr::mutate(date_time = as.Date(date),
                  # do correction based on error yesterday
                  lfalls_klag_corrected = lfalls_daily_accum_klag 
                  + lag(lfalls, n=2) # lfalls obs 2 days ago
                  - lag(lfalls_daily_accum_klag, n=2), # lfalls sim 2 days ago
                  lfalls_daily = lfalls) %>%
    dplyr::select(date_time, lfalls_daily, lfalls_klag_corrected)

# ******************************************************** 
# # Create today's recent and forecasted daily flows for archiving---------------
# klag.daily.fc.mgd.df <- por_klag_daily_mgd_df %>%
#   dplyr::rename(date = date_time,
#                 lfalls = lfalls_klag_corrected) %>%
#   dplyr::mutate(date_fc = date_today0,
#                 length_fc = as.integer(date - date_fc))  %>%
#   dplyr::select(-lfalls_daily)
# 
# prrism.daily.fc.mgd.df <- ops_1day_daily.df %>%
#   dplyr::select(date_time, lfalls_fc_prrism) %>%
#   dplyr::rename(date = date_time,
#                 lfalls = lfalls_fc_prrism) %>%
#   dplyr::mutate(date_fc = date_today0,
#                 length_fc = as.integer(date - date_fc)) 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# DAILY LFalls predicted from constant lags - first graph on ui ---------------
# lfalls_1day.plot1.df <- left_join(ops_1day_daily.df, por_klag_daily_mgd_df(),
#                                   by = "date_time") %>%
#   mutate(lfalls_flowby = lfalls_flowby) %>%
#   select(-w_pot_total_net, -goose, -por, -lfalls_daily) %>%
#   gather(key = "site", value = "flow", -date_time)

output$one_day_ops_plot1 <- renderPlot({
  
  lfalls_1day.plot0.df <- left_join(ops_1day_daily.df, por_klag_daily_mgd_df(),
                                    by = "date_time") %>%
    mutate(lfalls_flowby = lfalls_flowby) %>%
    select(-w_pot_total_net, -goose, -por, -lfalls_daily) 
  write_csv(lfalls_1day.plot0.df, 
            paste("output/",
                  "one-day-ts.csv",
                  sep=""))
  lfalls_1day.plot1.df <- lfalls_1day.plot0.df %>%
    gather(key = "site", value = "flow", -date_time) %>%
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
    scale_linetype_manual(values = c("solid", "solid", 
                                     "solid", "dashed", "dashed",
                                     "solid", "solid", "solid",
                                     "solid", "solid")) +
    scale_size_manual(values = c(0.5, 1, 2, 0.5, 1, 0.5, 0.5, 1, 1, 1)) +
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
    scale_color_manual(values = c( "deepskyblue1", "magenta1", "red", 
                                   "deepskyblue4", "indianred2",
                                  "plum",  "palegreen3", "red", 
                                  "palegreen4")) +
    scale_linetype_manual(values = c("solid", "solid", "dashed",
                                     "solid", "solid",
                                     "solid", "solid", "solid", 
                                     "solid")) +
    scale_size_manual(values = c(2, 0.5, 1, 0.5, 1, 1, 0.5, 1, 1)) +
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
