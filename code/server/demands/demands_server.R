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
# Grab daily time series
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Grab yesterday's values to display
withdrawals_yesterday.df <- withdrawals.daily.df %>%
  filter(date_time == date_today0 - 1)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Graph Potomac River withdrawals
output$pot_withdrawals <- renderPlot({
  # gather the data into long format; use the dynamic plot ranges
  withdrawals.plot.df <- withdrawals.daily.df %>%
    select(Date = date_time,
           "FW Potomac" = w_fw_pot,
           "WSSC Water Potomac" = w_wssc_pot,
           "WA Great Falls" = w_wa_gf,
           "WA Little Falls" = w_wa_lf,
           "LW Potomac" = w_lw_pot,
           "Broad Run discharge" = w_lw_br,
           "Net Potomac withdrawals" = w_pot_total_net) %>%
    gather(key = "Legend", 
           value = "MGD", -Date) %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  
  # plot the data
  ggplot(withdrawals.plot.df, aes(x = Date, y = MGD, group = Legend)) + 
    geom_point(aes(colour = Legend, size = Legend)) +
    labs(x = "", y = "Potomac withdrawals, MGD") +
    scale_size_manual(values = c(1,1,1,2,1,1,1))
})

# Graph WMA production
output$wma_production <- renderPlot({
  # gather the data into long format; use the dynamic plot ranges
  production.plot.df <- production.daily.df %>%
    select(Date = date_time,
           "Fairfax Water" = p_fw,
           "WSSC Water" = p_wssc,
           "Washington Aqueduct" = p_wa,
           "Loudoun Water" = p_lw,
           "WMA total" = p_wma_total) %>%
    gather(key = "Legend", 
           value = "MGD", -Date) %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  
  # plot the data
  ggplot(production.plot.df, aes(x = Date, y = MGD, group = Legend)) + 
    geom_point(aes(colour = Legend, size = Legend)) +
    labs(x = "", y = "WMA production, MGD") +
    scale_size_manual(values = c(1,1,1,2,1))
})


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct value box content
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Display yesterday's Potomac withdrawals -------------------------------------
output$w_fw_pot_yesterday <- renderValueBox({
  w_fw_pot_yesterday <- paste(
    "Fairfax Water: ",
    round(withdrawals_yesterday.df$w_fw_pot[1],0),
    " MGD", sep = "")
  valueBox(
    value = tags$p(w_fw_pot_yesterday, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

output$w_wssc_pot_yesterday <- renderValueBox({
  w_wssc_pot_yesterday <- paste(
    "WSSC Water: ",
    round(withdrawals_yesterday.df$w_wssc_pot[1],0),
    " MGD", sep = "")
  valueBox(
    value = tags$p(w_wssc_pot_yesterday, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

output$w_wa_gf_yesterday <- renderValueBox({
  w_wa_gf_yesterday <- paste(
    "Washington Aqueduct Great Falls: ",
    round(withdrawals_yesterday.df$w_wa_gf[1], 0),
    " MGD", sep = "")
  valueBox(
    value = tags$p(w_wa_gf_yesterday, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

output$w_wa_lf_yesterday <- renderValueBox({
  w_wa_lf_yesterday <- paste(
    "Washington Aqueduct Little Falls: ",
    round(withdrawals_yesterday.df$w_wa_lf[1], 0),
    " MGD", sep = "")
  valueBox(
    value = tags$p(w_wa_lf_yesterday, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

output$w_lw_pot_yesterday <- renderValueBox({
  w_lw_pot_yesterday <- paste(
    "Loudoun Water: ",
    round(withdrawals_yesterday.df$w_lw_pot[1], 1),
    " MGD", sep = "")
  valueBox(
    value = tags$p(w_lw_pot_yesterday, style = "font-size: 50%;"),
    subtitle = NULL,
    color = "light-blue"
  )
})

