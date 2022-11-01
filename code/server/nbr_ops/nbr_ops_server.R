# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Create graphs and values displayed for 10-day operations
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows_daily_cfs.csv - current daily streamflow data
# demands.daily.df - WMA supplier daily withdrawal data
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# All for display on 10-Day Ops page
#   Plots:
#   - output$ten_day_plot - graph of LFalls observed & forecasted flows
#   - output$nbr_ten_day_plot - graph of NBr res. inflows & outflows; & Luke
#   Value boxes:
#   - output$lfalls_empirical_9day_fc - LFalls forecast from our empirical eq.
#   - output$wma_withdr_9day_fc - WMA Potomac withdrawal 9-day forecast
#   - output$luke - today's flow at Luke before water supply release request
#   - output$deficit - estimated need at LFalls 9 days hence
#   - output$luke_target - today's target
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct time series
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Select flows of interest ----------------------------------------------------
# date_today_ops <- force_tz(date_today0, tzone = "America/New_York")
date_today_ops <- date_today0

# Pare down flows.daily.mgd.df
ops_10day.df000 <- flows.daily.mgd.df %>%
  dplyr::mutate(lfalls_from_upstr = lag(por, 2) + lag(monoc_jug, 2)
                  + lag(goose, 1) + lag(seneca, 1) - lag(w_pot_total_net, 1)) %>%
  select(date_time, lfalls, por, monoc_jug, 
         lfalls_from_upstr,
         goose, seneca,
         luke, kitzmiller, barnum,
         bloomington, barton, 
         w_pot_total_net
         ) %>%
  # filter(date_time < date_today_ops) %>%
  filter(date_time > date_today_ops - days(90))

# Now join with lffs results
ops_10day.df00 <- left_join(ops_10day.df000, lffs.daily.bfc.mgd.df,
                          by = "date_time") %>%
  dplyr::select(date_time, lfalls, 
                lfalls_from_upstr, 
                lfalls_lffs_bfc,
                por, monoc_jug,
                luke, kitzmiller, barnum,
                bloomington, barton, 
                w_pot_total_net
                )

flow_today_df <- ops_10day.df00 %>%
  filter(date_time == date_today_ops)

# Prepare 9-day LFalls forecast from empirical eq. ----------------------------
#   - still use constant 9-day lag from reservoirs to LFalls for now
date_time_9dayshence = date_today_ops + days(9)
# date_time_9dayshence = force_tz(date_today_ops + days(9),
#                                 tzone = "America/New_York")

# Compute LSen/JJR "buffer" based on imbalance in fractional storage
# Temporary placeholder:
storage_local_today <- storage_local_daily_bg_df %>%
  filter(date_time == date_today_ops)
lsen_percent <- 100*storage_local_today$seneca[1]/(sen_cap/1000)

jrr_ws_cap <- jrr_cap*jrr_ws_frac
storage_nbr_today <- storage_nbr_daily_df %>%
  filter(date_time == date_today_ops)
jrr_percent <- 100*storage_nbr_today$jrr_ws[1]/(jrr_ws_cap/1000)

# If JRR is fuller (buffer is positive), it should release a bit more:
lsen_jrr_buffer <- 10*(jrr_percent -lsen_percent)

# This is JUST FOR QA'ING - non-reactive!
ops_10day.dfQA <- ops_10day.df00 %>%
  dplyr::mutate(res_inflow = kitzmiller + barton,
                res_outflow = barnum + bloomington,
                res_augmentation = res_outflow - res_inflow,
                w_pot_total_net = case_when(
                  # seems that case_when feels input$'s are integer
                  is.na(w_pot_total_net) == TRUE ~ 1.000001*400,
                  is.na(w_pot_total_net) == FALSE ~ w_pot_total_net,
                  TRUE ~ -9999.9),
                res_aug_lagged8 = lag(res_augmentation, 8),
                res_aug_lagged9 = lag(res_augmentation, 9),
                res_aug_lagged10 = lag(res_augmentation, 10),
                res_aug_lagged = (res_aug_lagged8 + res_aug_lagged9
                                  + res_aug_lagged10)/3,
                lfalls_nat = lfalls + w_pot_total_net - res_aug_lagged,
                lfalls_nat_empirical_fc = 288.79*exp(0.0009*lfalls_nat),
                lfalls_nat_empirical_fc = case_when(
                  lfalls_nat_empirical_fc > lfalls_nat ~ lfalls_nat,
                  lfalls_nat_empirical_fc <= lfalls_nat
                  ~ lfalls_nat_empirical_fc,
                  TRUE ~ -9999),
                lfalls_adj_empirical_fc = lfalls_nat_empirical_fc +
                  res_augmentation,
                lfalls_adj_empirical_fc_lagged =
                  lag(lfalls_adj_empirical_fc, 9),
                # TEMPORARY!
                lfalls_empirical_fc = lfalls_adj_empirical_fc_lagged - 400,
                # add flowby to the graph
                lfalls_flowby = lfalls_flowby
                ) %>%
  # Do some decluttering
  dplyr::select(-res_aug_lagged8, -res_aug_lagged9, -res_aug_lagged10)

# REACTIVE df - because want to use input$
ops_10day.df <- reactive({
  req(input$default_w_pot_net)
  final.df <- ops_10day.df00 %>%
    # Fill in missing w_pot_total_net's with reactive default value
    dplyr::mutate(w_pot_total_net = case_when(
      # seems that case_when feels input$'s are integer
      is.na(w_pot_total_net) == TRUE ~ 1.00001*input$default_w_pot_net,
      is.na(w_pot_total_net) == FALSE ~ w_pot_total_net, 
      TRUE ~ -9999.9)) %>%
    dplyr::mutate(res_inflow = kitzmiller + barton,
                res_outflow = barnum + bloomington,
                res_augmentation = res_outflow - res_inflow,
                res_aug_lagged8 = lag(res_augmentation, 8),
                res_aug_lagged9 = lag(res_augmentation, 9),
                res_aug_lagged10 = lag(res_augmentation, 10),
                res_aug_lagged = (res_aug_lagged8 + res_aug_lagged9
                                  + res_aug_lagged10)/3,
                lfalls_nat = lfalls + w_pot_total_net - res_aug_lagged,
                lfalls_nat_empirical_fc = 288.79*exp(0.0009*lfalls_nat),
                lfalls_nat_empirical_fc = case_when(
                  lfalls_nat_empirical_fc > lfalls_nat ~ lfalls_nat,
                  lfalls_nat_empirical_fc <= lfalls_nat 
                  ~ lfalls_nat_empirical_fc,
                  TRUE ~ -9999.9),
                lfalls_adj_empirical_fc = lfalls_nat_empirical_fc + 
                  res_augmentation,
                lfalls_adj_empirical_fc_lagged = 
                  lag(lfalls_adj_empirical_fc, 9),
                w_pot_total_net_lagged = lag(w_pot_total_net, 9),
                lfalls_empirical_fc = lfalls_adj_empirical_fc_lagged
                - w_pot_total_net_lagged,
                lfalls_flowby = lfalls_flowby
                ) %>%
  # Do some decluttering
    dplyr::select(-res_aug_lagged8, -res_aug_lagged9, -res_aug_lagged10)
  return(final.df)
  })

flows_today.df <- reactive({
  ops_10day.df() %>%
  filter(date_time == date_today_ops)
})

fc_9days_hence.df <- reactive({ops_10day.df() %>%
    dplyr::filter(date_time == date_time_9dayshence)
})

fc_w_9day_hence.df <- reactive({withdrawals.daily.df() %>%
    dplyr::filter(date_time == date_time_9dayshence)
  })

fc_w_9days_hence <- reactive({
  fc_9days_hence.df()$w_pot_total_net_lagged[1]
})

fc_lfalls_9days_hence <- reactive({
  fc_9days_hence.df()$lfalls_empirical_fc[1]
})

fc_lfalls_adj_9days_hence <- reactive({
  fc_9days_hence.df()$lfalls_adj_empirical_fc_lagged[1]
})


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Prepare for plotting LFalls & POR flows - first graph on ui -----------------
# lfalls_10day.plot.df <- ops_10day.df %>%
#   dplyr::select(date_time, lfalls,
#                 por, monoc_jug, d_pot_total,
#                 lfalls_from_upstr, lfalls_lffs_bfc,
#                 lfalls_flowby
#                 ) %>%
#   gather(key = "site", value = "flow", -date_time)

# lfalls_10day.plot2.df <- ops_10day.df0 %>%
#   dplyr::select(date_time, lfalls_empirical_fc) %>%
#   gather(key = "site", value = "flow", -date_time) %>%
#   filter(date_time == date_time_9dayshence & site == "lfalls_empirical_fc")

# Create LFalls flow plot - 1st graph on tab ----------------------------------
output$ten_day_plot <- renderPlot({
  # Prepare df for plotting LFalls flows - first graph on ui ------------------
  req(!is.null(input$default_w_pot_net))  
  # 1st piece of plot - the line graphs
  lfalls_10day_plot1 <- ops_10day.df() %>%
    dplyr::select(date_time, lfalls,
                  por, monoc_jug, w_pot_total_net,
                  lfalls_from_upstr, lfalls_lffs_bfc,
                  lfalls_flowby
    ) %>%
    gather(key = "site", value = "flow", -date_time) %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])

  # 2nd piece of the plot - the empirical 9-day fc - as a bubble
  lfalls_10day_plot2 <- ops_10day.df() %>%
    dplyr::select(date_time, lfalls_empirical_fc) %>%
    gather(key = "site", value = "flow", -date_time) %>%
    filter(date_time == date_time_9dayshence & site == "lfalls_empirical_fc") %>%
    filter(date_time >= input$plot_range[1],
           date_time <= input$plot_range[2])
  
  # Construct the graph
  ggplot(lfalls_10day_plot1, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c("deepskyblue1", "red", 
                                  "deepskyblue2", "purple",
                                   "plum", "navy", "orange")) +
    scale_size_manual(values = c(2, 1, 1, 1, 1, 1, 1)) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed",
                          "solid", "solid", "dotdash", "solid")) +
    labs(x = "", y = "Flow, MGD") +
    # shape=1 is open circle, stroke is border width
    geom_point(data = lfalls_10day_plot2, aes(x = date_time, y = flow),
               size = 5, colour = "deepskyblue3", shape = 1, stroke = 1.0)
})
  
# N Br flows plot -------------------------------------------------------------
output$nbr_ten_day_plot <- renderPlot({
  #   - flows at Luke and flows into and out of reservoirs
  nbr_10day.plot.df <- ops_10day.df() %>%
    dplyr::select(date_time, kitzmiller, barnum, 
                  bloomington, barton, luke) %>%
    gather(key = "site", value = "flow", -date_time)
  
  nbr_10day.plot.df <- nbr_10day.plot.df %>%
  filter(date_time >= input$plot_range[1],
         date_time <= input$plot_range[2])
  ggplot(nbr_10day.plot.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site)) +
    labs(x = "", y = "Flow, MGD")
})
  
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct content for value boxes
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Value box to display LFalls today -------------------------------------------

output$lfalls_today <- renderValueBox({
  
  lfalls_mgd <- round(flows_today.df()$lfalls[1], 0)
  lfalls_today <- paste(
    "Current observed flow at Little Falls: ",
    lfalls_mgd,
    " MGD", sep = "")
  valueBox(
    value = tags$p(lfalls_today, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Value box to display 9-day fc for total WMA withdrawal-----------------------
output$wma_withdr_9day_fc <- renderValueBox({
  wma_withdr <- paste(
    "Forecasted WMA total Potomac withdrawals in 9 days: ",
    round(fc_w_9days_hence()), " MGD", sep = "")
  valueBox(
    value = tags$p(wma_withdr, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Value box to display today's flow at Luke -----------------------------------
output$luke <- renderValueBox({
  
  luke_mgd <- round(flows_today.df()$luke[1], 0)
  luke_cfs <- round(luke_mgd*mgd_to_cfs, 0)
  luke_today <- paste("Flow at Luke today 
                      before water supply release request: ",
                      luke_mgd,
                      " MGD (", 
                      luke_cfs, 
                      " cfs)", sep = "")
  valueBox(
    value = tags$p(luke_today, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# 3 value boxes giving N Br water supply release info -----------------------
#   - based on empirical recession eq. 9-day forecast

# LFalls 9-day empirical eq. fc value
output$lfalls_empirical_9day_fc <- renderValueBox({
  fc_lfalls_adj_9days_hence <- 
    round(fc_9days_hence.df()$lfalls_adj_empirical_fc_lagged[1])
  
  fc_w_9days_hence <- 
    round(fc_9days_hence.df()$w_pot_total_net_lagged[1])
  
  lfalls_9day_fc1 <- paste(
    "9 day mpirical fc (MGD): lfalls_adj, w, lfalls: ",
    fc_lfalls_adj_9days_hence, ", " , 
    fc_w_9days_hence, ", " , 
    fc_lfalls_adj_9days_hence - fc_w_9days_hence,
    sep = "")
  
  valueBox(
    value = tags$p(lfalls_9day_fc1, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

# Deficit in nine days time
output$empirical_9day_deficit <- renderValueBox({

  deficit1_mgd <- round(lfalls_flowby - fc_lfalls_9days_hence(), 0)
  deficit1_cfs <- round(deficit1_mgd*mgd_to_cfs) 
  
  deficit1_9days <- paste("Flow deficit in 9 days time: ",
                         deficit1_mgd,
                      " MGD (", 
                      deficit1_cfs, 
                      " cfs) [Negative deficit is a surplus]", sep = "")
  valueBox(
    value = tags$p(deficit1_9days, style = "font-size: 35%;"),
    subtitle = NULL,
    # LukeV - want orange if deficit_mgd is positive, light-blue if negative
    color = "light-blue"
  )
})

# Today's Luke target 
output$luke_target1 <- renderValueBox({
  lfalls_fc <- ops_10day.df() %>%
    filter(date_time == date_time_9dayshence) %>%
    mutate(flow = lfalls_empirical_fc)
  lfalls_9day_fc1_mgd <- round(lfalls_fc$flow[1], 0)
  deficit1_mgd <- round(lfalls_flowby - lfalls_9day_fc1_mgd, 0)
  luke_mgd <- round(flows_today.df()$luke[1], 0)
  luke_extra1 <- if_else(deficit1_mgd <= 0, 0, deficit1_mgd)
  luke_target1_mgd <- round(luke_mgd + luke_extra1 + lsen_jrr_buffer, 0)
  luke_target1_cfs <- round(luke_target1_mgd*mgd_to_cfs, 0)
  
  luke_target1 <- paste("Today's Luke target plus 'buffer': ",
                       luke_target1_mgd,
                         " MGD (", 
                       luke_target1_cfs, 
                         " cfs)", sep = "")
  valueBox(
    value = tags$p(luke_target1, style = "font-size: 35%;"),
    subtitle = NULL,
    # LukeV - want orange if luke_extra > 0, light-blue if 0
    color = "light-blue"
  )
})

# Display 3 boxes giving N Br water supply release info -----------------------
#   - based on LFFS 9-day forecast

output$lfalls_lffs_9day_fc <- renderValueBox({
  
  # Grab the 9-day forecasts
  fc_9day <- ops_10day.df() %>%
    filter(date_time == date_time_9dayshence)
  fc_9day_lffs <- fc_9day$lfalls_lffs_bfc[1]
  fc_9day_emp_eq <- fc_9day$lfalls_empirical_fc[1]
  
  # LFalls 9-day LFFS fc value
  lfalls_9day_fc2_mgd <- round(fc_9day_lffs, 0)
  lfalls_9day_fc2_cfs <- round(lfalls_9day_fc2_mgd*mgd_to_cfs, 0)
  
  lfalls_9day_fc2 <- paste(
    "Forecasted flow at Little Falls in 9 days (from LFFS): ",
    lfalls_9day_fc2_mgd, " MGD (",
    lfalls_9day_fc2_cfs, " cfs)",
    sep = "")
  valueBox(
    value = tags$p(lfalls_9day_fc2, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

output$lffs_9day_deficit <- renderValueBox({
  
  # Grab the 9-day forecasts
  fc_9day <- ops_10day.df() %>%
    filter(date_time == date_time_9dayshence)
  fc_9day_lffs <- fc_9day$lfalls_lffs_bfc[1]
  fc_9day_emp_eq <- fc_9day$lfalls_empirical_fc[1]
  
  # LFalls 9-day LFFS fc value
  lfalls_9day_fc2_mgd <- round(fc_9day_lffs, 0)
  lfalls_9day_fc2_cfs <- round(lfalls_9day_fc2_mgd*mgd_to_cfs, 0)
  
  # Deficit in nine days time
  deficit2_mgd <- round(lfalls_flowby - lfalls_9day_fc2_mgd, 0)
  deficit2_cfs <- round(deficit2_mgd*mgd_to_cfs)
  
  deficit2_9days <- paste("Flow deficit in 9 days time: ",
                         deficit2_mgd,
                         " MGD (",
                         deficit2_cfs,
                         " cfs) [Negative deficit is a surplus]", sep = "")
  valueBox(
    value = tags$p(deficit2_9days, style = "font-size: 35%;"),
    subtitle = NULL,
    # LukeV - want orange if deficit_mgd is positive, light-blue if negative
    color = "light-blue"
  )
})

output$luke_target2 <- renderValueBox({
  
  # Grab the 9-day forecasts
  fc_9day <- ops_10day.df() %>%
    filter(date_time == date_time_9dayshence)
  fc_9day_lffs <- fc_9day$lfalls_lffs_bfc[1]
  fc_9day_emp_eq <- fc_9day$lfalls_empirical_fc[1]
  
  # LFalls 9-day LFFS fc value
  lfalls_9day_fc2_mgd <- round(fc_9day_lffs, 0)
  lfalls_9day_fc2_cfs <- round(lfalls_9day_fc2_mgd*mgd_to_cfs, 0)
  
  # Deficit in nine days time
  deficit2_mgd <- round(lfalls_flowby - lfalls_9day_fc2_mgd, 0)
  deficit2_cfs <- round(deficit2_mgd*mgd_to_cfs)
  
  # Today's Luke target
  luke_mgd <- round(flows_today.df()$luke[1], 0)
  luke_extra2 <- if_else(deficit2_mgd <= 0, 0, deficit2_mgd)
  luke_target2_mgd <- round(luke_mgd + luke_extra2 + lsen_jrr_buffer, 0)
  luke_target2_cfs <- round(luke_target2_mgd*mgd_to_cfs, 0)
  
  luke_target2 <- paste("Today's Luke target plus 'buffer': ",
                       luke_target2_mgd,
                       " MGD (",
                       luke_target2_cfs,
                       " cfs)", sep = "")
  valueBox(
    value = tags$p(luke_target2, style = "font-size: 35%;"),
    subtitle = NULL,
    # LukeV - want orange if luke_extra > 0, light-blue if 0
    color = "light-blue"
  )
})

