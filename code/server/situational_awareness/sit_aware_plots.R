# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Create graphs, values, and displays on situational awareness tab
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.mgd.df - df with daily streamflow data & Potomac withdrawals
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# All for display on Situational Awareness page
#   Plots:
#   - output$sit_aware_plot - various obs. flows & recession estimates
#   - output$jrr_plot
#   - output$occ_plot
#   - output$sen_plot
#   - output$pat_plot
#   Value boxes:
#   - output$lfalls_empirical_9day_fc - LFalls forecast from our empirical eq.
#   - output$wma_withdr_9day_fc - WMA Potomac withdrawal 9-day forecast
#   - output$luke - today's flow at Luke before water supply release request
#   - output$deficit - estimated need at LFalls 9 days hence
#   - output$luke_target - today's target
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Construct graphs
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Select flows of interest ----------------------------------------------------
#   - plot will be cfs

sit_aware_cfs.df <- flows.daily.cfs.df %>%
  dplyr::mutate(lfalls_flowby = lfalls_flowby*mgd_to_cfs,
                por_trigger = 2000)

output$sit_aware_flows_plot <- renderPlot({
  
  flows.plot.df <- left_join(sit_aware_cfs.df, withdrawals.daily.df0,
                             by = "date_time") %>%
    select(date_time, lfalls, por, monoc_jug, shen_mill,
           seneca, lfalls_flowby, por_trigger, w_pot_total_net) %>%
    gather(key = "site", value = "flow", -date_time)
  
  flows.plot.df <- flows.plot.df %>%  
  filter(date_time >= input$plot_range[1],
         date_time <= input$plot_range[2])
  ggplot(flows.plot.df, aes(x = date_time, y = flow)) + 
    geom_line(aes(colour = site, size = site, linetype = site)) +
    scale_color_manual(values = c("deepskyblue1", "red", 
                                  "plum", "steelblue",
                                  "tomato1", "palegreen3", 
                                  "plum", "orange")) +
    scale_size_manual(values = c(2, 1, 1, 1, 1, 1, 1, 1)) +
    scale_linetype_manual(values = c("solid", "dashed",
                                     "solid", "solid",
                                     "dotted","solid",
                                     "solid", "solid")) +
    labs(x = "", y = "Flow, cfs")
})

#------------------------------------------------------------------
# Create graph of storage and releases for each reservoir
#------------------------------------------------------------------

output$sit_aware_jrr_stor <- renderPlot({
  graph_title <- "Jennings Randolph"
  # jrr.graph <- ts$jrr %>%
  #   select(Date = date_time,
  #          "WS stor" = storage_ws,
  #          "WQ stor" = storage_wq,
  #          "WS rel" = outflow_ws,
  #          "WQ rel" = outflow_wq
  #   ) %>%
  #   gather(key = "Legend",
  #          value = "MG", -Date) %>%
  #   filter(Date >= input$plot_range[1],
  #          Date <= input$plot_range[2])
  jrr.graph <- storage_nbr_daily_df %>%
    mutate(jrr_wq = jrr_total - jrr_ws) %>%
    select(Date = date_time, 
           "WS storage" = jrr_ws, 
           "WQ storage" = jrr_wq, 
           "Total storage" = jrr_total) %>%
      gather(key = "Legend",
             value = "BG", -Date) %>%
      filter(Date >= input$plot_range[1],
             Date <= input$plot_range[2])
  
  ggplot(data = jrr.graph,
         aes(x = Date, y = BG, group = Legend)) +
    geom_point(aes(color = Legend, size = Legend)) +
    scale_color_manual(values = c("blue", "green",
                                   "deepskyblue3")) +
    scale_size_manual(values = c(1.0,  0.5, 0.5)) +
    scale_y_continuous(limits = c(0.0, 32.0),
                       breaks = c(0.0, 10.0, 20.0, 30.0)) +
    ggtitle(graph_title) +
    theme(plot.title = element_text(size = 16)) +
    # face = "bold")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
}) # end jrr renderPlot testing

# Create graph of L Seneca storage --------------------------------------------  
  output$sit_aware_sen_stor <- renderPlot({
    # sen.graph <- storage.daily.bg.df0 %>%
    #   mutate(stor_sen = as.numeric(stor_sen)) %>%
    #   select(date_time, stor_sen)
    sen.graph <- storage_local_daily_bg_df %>%
      select(Date = date_time, 
               Storage = seneca) %>%
      filter(Date >= input$plot_range[1],
             Date <= input$plot_range[2])
    
    ggplot(data = sen.graph, aes(x = Date, y = Storage)) +
      geom_point(colour="blue", size=1) +
      scale_y_continuous(name = "Storage, BG", limits = c(0.0, 4.0),
                         breaks = c(0, 1, 2, 3, 4)) +
      ggtitle("Little Seneca") +
      theme(plot.title = element_text(size = 16)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      theme(legend.position = "none")
}) # end sen renderPlot
#
# Create graph of Patuxent storage --------------------------------------------
output$sit_aware_pat_stor <- renderPlot({
  # pat.graph <- storage.daily.bg.df0 %>%
  # mutate(stor_pat = as.numeric(stor_pat)) %>%
  #   select(date_time, stor_pat)

  pat.graph <- storage_local_daily_bg_df %>%
    select(Date = date_time, 
           Storage = patuxent) %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  
  ggplot(data = pat.graph, aes(x = Date, y = Storage)) +
    geom_point(colour="blue", size=1) +
    scale_y_continuous(name = "Storage, BG", limits = c(0.0, 11.0),
                       breaks = c(0, 2, 4, 6, 8, 10, 12)) +
    ggtitle("Patuxent") +
    theme(plot.title = element_text(size = 16)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "none")
}) # end pat renderPlot
#
# Create graph of Occoquan storage --------------------------------------------
output$sit_aware_occ_stor <- renderPlot({
  # occ.graph <- storage.daily.bg.df0 %>%
  #   mutate(stor_occ = as.numeric(stor_occ)) %>%
  #   select(date_time, stor_occ)

  occ.graph <- storage_local_daily_bg_df %>%
    select(Date = date_time, 
           Storage = occoquan) %>%
    filter(Date >= input$plot_range[1],
           Date <= input$plot_range[2])
  
  ggplot(data = occ.graph, aes(x = Date, y = Storage)) +
    geom_point(colour="blue", size=1) +
    scale_y_continuous(name = "Storage, BG", limits = c(0.0, 10.0), 
                       breaks = c(0, 2, 4, 6, 8, 10)) +
    ggtitle("Occoquan") +
    theme(plot.title = element_text(size = 16)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    theme(legend.position = "none")
}) # end occ renderPlot


print("finishing sit_aware_plots")

  
