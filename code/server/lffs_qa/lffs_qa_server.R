# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Create graphs and values displayed on LFFS QA "sandbox" tab
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
date_today_ops <- date_today0

lffs.df <- left_join(flows.daily.mgd.df, lffs.daily.bfc.mgd.df,
                          by = "date_time") %>%
  dplyr::select(date_time, lfalls, lfalls_from_upstr, 
                lfalls_lffs, lfalls_lffs_bfc,
                por, monoc_jug, goose, seneca,
                # luke, kitzmiller, barnum,
                d_pot_total)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Prepare for plotting LFalls & POR flows - first graph on ui -----------------
lffs.plot.df <- lffs.df %>%
  dplyr::select(date_time, lfalls,
                por, monoc_jug, d_pot_total,
                lfalls_from_upstr, lfalls_lffs, 
                lfalls_lffs_bfc #,
                # lfalls_flowby
                ) %>%
  gather(key = "site", value = "flow", -date_time)

# There must be a better way to plot the fc point -----------------------------
# lfalls_10day.plot2.df <- ops_10day.df %>%
#   dplyr::select(date_time, lfalls_empirical_fc) %>%
#   gather(key = "site", value = "flow", -date_time) %>%
#   filter(date_time == date_time_9dayshence & site == "lfalls_empirical_fc")

# Create LFalls flow plot -----------------------------------------------------
output$lffs_qa_plot <- renderPlot({
  
  # Want user control of plot range
  lffs.plot.df <- lffs.plot.df %>%  
  filter(date_time >= input$plot_range[1],
         date_time <= input$plot_range[2])
  
  # lffs.plot2.df <- lffs.plot2.df %>%  
  #   filter(date_time >= input$plot_range[1],
  #          date_time <= input$plot_range[2])
  
  # Construct the graph
  ggplot(lffs.plot.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c("orange", "deepskyblue1", 
                                  "steelblue4", "darkorchid3",
                                  "plum", "navy",
                                  "slateblue3")) +
    scale_size_manual(values = c(1, 2, 1, 1, 1, 1, 1)) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid",
                          "solid", "solid", "dotted")) +
    labs(x = "", y = "Flow, MGD") # +

})
  
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Compute stats
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
low_flow_threshold <- 2000 # low-flow threshold - maybe 5th percentile value
location <- "lfalls"
fc <- "lffs_daily"
obs_df <- flows.daily.cfs.df %>%
  mutate(obs = c_across(matches(location))) %>%
  dplyr::select(date_time, obs)
sim_df <- lffs.daily.mgd.df %>%
  mutate(sim = c_across(matches(location))) %>%
  dplyr::select(date_time, sim)

fc_df <- left_join(obs_df, sim_df, 
                      by = "date_time") %>%
  mutate(location = location, fc = fc) %>%
  drop_na()
obs_mean <- mean(fc_df$obs)

stats_df <- fc_df %>%
  mutate(err = obs - sim,
         ae = abs(obs - sim),
         pae = 100*ae/obs,
         se = (obs - sim)^2,
         nse_denominator = (obs - obs_mean)^2 ) %>%
  summarise(across(where(is.character), first),
            across(where(is.numeric), mean),
            count = n()) %>%
  dplyr::select(-obs, -sim)

nse <- 1 - stats_df$se[1]/stats_df$nse_denominator[1]
stats_df <- stats_df %>%
  mutate(nse = nse) %>%
  select(-nse_denominator) %>%
  relocate(count, .after = last_col())

stats_low_flow_df <- fc_df %>%
  filter(obs <= low_flow_threshold) %>%
  mutate(err = obs - sim,
         ae = abs(obs - sim),
         pae = 100*ae/obs,
         se = (obs - sim)^2,
         nse_denominator = (obs - means$mean_obs[1])) %>%
  summarise(across(2:8, mean))
nse_value <- 1 - stats_low_flow_df$se[1]/stats_low_flow_df$nse_denominator[1]


means <- summarise(stats_df0, mean_obs = mean(obs), mean_sim = mean(sim))

stats_df <- stats_df0 %>%
  mutate(err = obs - sim,
                ae = abs(obs - sim),
                pae = 100*ae/obs,
                se = (obs - sim)^2,
                nse1 = (obs - means$mean_obs[1])) %>%
  summarise(across(everything(), mean))
  

  
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct value box content
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Show LFalls today -----------------------------
flows_today2.df <- lffs.df %>%
  filter(date_time == date_today_ops)
# lffs_mgd <- round(flows_today2.df$lffs[1], 0)
lffs_mgd <- 100000
output$lffs_today <- renderValueBox({
  lffs_today <- paste(
    "Current observed flow at Little Falls: ",
    lffs_mgd,
    " MGD", sep = "")
  valueBox(
    value = tags$p(lffs_today, style = "font-size: 35%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})


# # Grab total WMA withdrawal 9-day fc for display -----------------------------
# wma_withdr_fc <- ops_10day.df %>%
#   filter(date_time == date_today_ops + 9)
# wma_withdr_fc <- round(wma_withdr_fc$d_pot_total[1], 0)
# output$wma_withdr_9day_fc <- renderValueBox({
#   wma_withdr <- paste(
#     "Forecasted WMA total withdrawals in 9 days (from COOP regression eqs.): ",
#                           wma_withdr_fc,
#                           " MGD", sep = "")
#   valueBox(
#     value = tags$p(wma_withdr, style = "font-size: 35%;"),
#     subtitle = NULL,
#     color = "light-blue"
#   )
# })
# 
# # Display today's flow at Luke ------------------------------------------------
# luke_flow_today <- ops_10day.df %>%
#   filter(date_time == date_today_ops)
# luke_mgd <- round(luke_flow_today$luke[1], 0)
# luke_cfs <- round(luke_mgd*mgd_to_cfs, 0)
# output$luke <- renderValueBox({
#   luke_today <- paste("Flow at Luke today 
#                       before water supply release request: ",
#                       luke_mgd,
#                       " MGD (", 
#                       luke_cfs, 
#                       " cfs)", sep = "")
#   valueBox(
#     value = tags$p(luke_today, style = "font-size: 35%;"),
#     subtitle = NULL,
#     color = "light-blue"
#   )
# })
# 
# # Display 3 boxes giving N Br water supply release info -----------------------
# #   - based on empirical recession eq. 9-day forecast
# print(round(lfalls_10day.plot2.df$flow[1]))
# # LFalls 9-day empirical eq. fc value
# lfalls_9day_fc1_mgd <- round(lfalls_10day.plot2.df$flow[1], 0)
# lfalls_9day_fc1_cfs <- round(lfalls_9day_fc1_mgd*mgd_to_cfs, 0)
# output$lfalls_empirical_9day_fc <- renderValueBox({
#   lfalls_9day_fc1 <- paste(
#     "Forecasted flow at Little Falls in 9 days (from empirical eq.): ",
#     lfalls_9day_fc1_mgd, " MGD (", 
#     lfalls_9day_fc1_cfs, " cfs)",
#     sep = "")
#   valueBox(
#     value = tags$p(lfalls_9day_fc1, style = "font-size: 35%;"),
#     subtitle = NULL,
#     color = "light-blue"
#   )
# })
# 
# # Deficit in nine days time
#   deficit1_mgd <- round(lfalls_flowby - lfalls_9day_fc1_mgd, 0)
#   deficit1_cfs <- round(deficit1_mgd*mgd_to_cfs)  
# output$empirical_9day_deficit <- renderValueBox({
#   deficit1_9days <- paste("Flow deficit in 9 days time: ",
#                          deficit1_mgd,
#                       " MGD (", 
#                       deficit1_cfs, 
#                       " cfs) [Negative deficit is a surplus]", sep = "")
#   valueBox(
#     value = tags$p(deficit1_9days, style = "font-size: 35%;"),
#     subtitle = NULL,
#     # LukeV - want orange if deficit_mgd is positive, light-blue if negative
#     color = "light-blue"
#   )
# })
# 
# # Today's Luke target 
# luke_extra1 <- if_else(deficit1_mgd <= 0, 0, deficit1_mgd)
# luke_target1_mgd <- round(luke_mgd + luke_extra1 + lsen_jrr_buffer, 0)
# luke_target1_cfs <- round(luke_target1_mgd*mgd_to_cfs, 0)
# output$luke_target1 <- renderValueBox({
#   luke_target1 <- paste("Today's Luke target plus 'buffer': ",
#                        luke_target1_mgd,
#                          " MGD (", 
#                        luke_target1_cfs, 
#                          " cfs)", sep = "")
#   valueBox(
#     value = tags$p(luke_target1, style = "font-size: 35%;"),
#     subtitle = NULL,
#     # LukeV - want orange if luke_extra > 0, light-blue if 0
#     color = "light-blue"
#   )
# })
# 
# # Display 3 boxes giving N Br water supply release info -----------------------
# #   - based on LFFS 9-day forecast
# 
# # LFalls 9-day LFFS fc value
# lfalls_9day_fc2_mgd <- round(fc_9day_lffs, 0)
# lfalls_9day_fc2_cfs <- round(lfalls_9day_fc2_mgd*mgd_to_cfs, 0)
# output$lfalls_lffs_9day_fc <- renderValueBox({
#   lfalls_9day_fc2 <- paste(
#     "Forecasted flow at Little Falls in 9 days (from LFFS): ",
#     lfalls_9day_fc2_mgd, " MGD (",
#     lfalls_9day_fc2_cfs, " cfs)",
#     sep = "")
#   valueBox(
#     value = tags$p(lfalls_9day_fc2, style = "font-size: 35%;"),
#     subtitle = NULL,
#     color = "light-blue"
#   )
# })
# 
# # Deficit in nine days time
#   deficit2_mgd <- round(lfalls_flowby - lfalls_9day_fc2_mgd, 0)
#   deficit2_cfs <- round(deficit2_mgd*mgd_to_cfs)
# output$lffs_9day_deficit <- renderValueBox({
#   deficit2_9days <- paste("Flow deficit in 9 days time: ",
#                          deficit2_mgd,
#                          " MGD (",
#                          deficit2_cfs,
#                          " cfs) [Negative deficit is a surplus]", sep = "")
#   valueBox(
#     value = tags$p(deficit2_9days, style = "font-size: 35%;"),
#     subtitle = NULL,
#     # LukeV - want orange if deficit_mgd is positive, light-blue if negative
#     color = "light-blue"
#   )
# })
# 
# # Today's Luke target
# luke_extra2 <- if_else(deficit2_mgd <= 0, 0, deficit2_mgd)
# luke_target2_mgd <- round(luke_mgd + luke_extra2 + lsen_jrr_buffer, 0)
# luke_target2_cfs <- round(luke_target2_mgd*mgd_to_cfs, 0)
# output$luke_target2 <- renderValueBox({
#   luke_target2 <- paste("Today's Luke target plus 'buffer': ",
#                        luke_target2_mgd,
#                        " MGD (",
#                        luke_target2_cfs,
#                        " cfs)", sep = "")
#   valueBox(
#     value = tags$p(luke_target2, style = "font-size: 35%;"),
#     subtitle = NULL,
#     # LukeV - want orange if luke_extra > 0, light-blue if 0
#     color = "light-blue"
#   )
# })

