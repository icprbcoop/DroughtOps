# Finally, create boxes with values and triggers
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
# Create values for use in boxes
#------------------------------------------------------------------------------
# First grab Point of Rocks & Little Falls daily mean yesterday
  flows_yesterday.df <- flows.daily.cfs.df0 %>%
    select(date_time, lfalls, por) %>%
    tail(1)
  
  # Error trapping: make sure last day is yesterday
  daily_flows_last_date <- flows_yesterday.df$date_time[1]
       if(daily_flows_last_date == date_today0 - 1) yesterday_available <- 1 else
         yesterday_available <- 0
  
  if(yesterday_available == 1) {por_yesterday_cfs <- flows_yesterday.df$por[1]
  por_yesterday_mgd <- round(por_yesterday_cfs/mgd_to_cfs)
  lfalls_yesterday_cfs <- flows_yesterday.df$lfalls[1]
  lfalls_yesterday_mgd <- round(lfalls_yesterday_cfs/mgd_to_cfs)
  } else {
    por_yesterday_mgd <- "NA"
    por_yesterday_cfs <- "NA"
    lfalls_yesterday_mgd <- "NA"
    lfalls_yesterday_cfs <- "NA"
  }
  
  # Next grab flow and time of most recent real-time data
  por_rt.df <- flows_rt_cfs_df %>%
    select(date_time, por) %>%
    drop_na(por) %>%
    arrange(date_time)
  por_rt_cfs <- tail(por_rt.df, 1)$por[1]
  por_rt_mgd <- round(por_rt_cfs/mgd_to_cfs)
  por_rt_time <- tail(por_rt.df, 1)$date_time[1]
  
  lfalls_rt.df <- flows_rt_cfs_df %>%
    select(date_time, lfalls) %>%
    drop_na(lfalls) %>%
    arrange(date_time)
  lfalls_rt_cfs <- tail(lfalls_rt.df, 1)$lfalls[1] 
  lfalls_rt_mgd <- round(lfalls_rt_cfs/mgd_to_cfs)
  lfalls_rt_time <- tail(lfalls_rt.df, 1)$date_time[1]
  
  # The following reactive objects seem to be accessible globally
  #   - probably should be moved to subcode of server.R
  #********************************************************
    withdrawals.daily.df <- reactive({
      final.df <- withdrawals.daily.df0 %>%
        mutate(w_pot_total_net = case_when(
          is.na(w_pot_total_net) == TRUE ~ 1.00001*input$default_w_pot_net,
          is.na(w_pot_total_net) == FALSE ~ w_pot_total_net,
          TRUE ~ -9999.9))
      return(final.df)
    })

    # Grab Potomac River withdrawals to compute LFalls adjusted flow
    withdrawals.sub.df <- reactive({withdrawals.daily.df() %>%
        filter(date_time >= date_today0 - 1 & date_time < date_today0 + 5)
    })

    withdr_pot_5dayfc <- reactive({
      max(tail(withdrawals.sub.df(), 5)$w_pot_total_net,
          na.rm = TRUE)
    })

    withdr_pot_yesterday <- reactive({
      head(withdrawals.sub.df(), 1)$w_pot_total_net[1]
    })
    
    lfalls_adj_yesterday <- reactive({
      lfalls_yesterday_mgd +
        withdr_pot_yesterday()
    })
    #********************************************************
  
  por_threshold <- 2000 # (cfs) CO-OP's trigger for daily monitoring/reporting 
  lfalls_threshold <- 100 # MGD

  #----------------------------------------------------------------------------  
  #----------------------------------------------------------------------------
  # Create values for Potomac River flow value boxes
  #----------------------------------------------------------------------------
  #----------------------------------------------------------------------------
  
  # Point of Rocks yesterday---------------------------------------------------
  output$por_flow_yesterday_text <- renderValueBox({
    por_flow_yesterday_text <- paste0("Point of Rocks yesterday: ",
                           por_yesterday_cfs,
                           " cfs (",
                           por_yesterday_mgd, " MGD)")

  valueBox(
    value = tags$p(por_flow_yesterday_text, style = "font-size: 40%;"),
    subtitle = NULL,
    color = if (por_yesterday_cfs >= por_threshold) "blue" else "yellow"
    # color = "blue"
  )
  })
  
  # Point of Rocks today (most recent real-time)-------------------------------
  output$por_flow_today_text <- renderValueBox({
    por_flow_today_text <- paste0("Point of Rocks today: ",
                                      por_rt_cfs,
                                      " cfs (",
                                      por_rt_mgd,
                                      " MGD) at ",
                                      por_rt_time)
    
    valueBox(
      value = tags$p(por_flow_today_text, style = "font-size: 40%;"),
      subtitle = NULL,
      color = if (por_rt_cfs >= por_threshold) "blue" else "yellow"
      # color = "blue"
    )
  })
  
  # Little Falls yesterday-----------------------------------------------------
  output$lfalls_flow_yesterday_text <- renderValueBox({
    lfalls_flow_yesterday_text <- paste0("Little Falls yesterday: ",
                                         lfalls_yesterday_cfs,
                                      " cfs (",
                                      lfalls_yesterday_mgd, " MGD)")
    
    valueBox(
      value = tags$p(lfalls_flow_yesterday_text, style = "font-size: 40%;"),
      subtitle = NULL,
      color = "blue"
    )
  })
  
  # Little Falls today (most recent real-time)---------------------------------
  output$lfalls_flow_today_text <- renderValueBox({
    lfalls_flow_today_text <- paste0("Little Falls today: ",
                                     lfalls_rt_cfs,
                                  " cfs (",
                                  lfalls_rt_mgd,
                                  " MGD) at ",
                                  lfalls_rt_time)
    
    valueBox(
      value = tags$p(lfalls_flow_today_text, style = "font-size: 40%;"),
      subtitle = NULL,
      color = "blue"
    )
  })
  
  # Little Falls adjusted yesterday & drought ops trigger----------------------
  output$lfalls_adj_yesterday_text <- renderValueBox({
    
    lfalls_adj_yesterday_text <- paste0("Yesterday's LFalls adj: ",
                                     round(lfalls_adj_yesterday()),
                                     " MGD; Twice fc'd withdr + 100: ",
                                     round(2*withdr_pot_5dayfc() + 100),
                                     " MGD")
    
    valueBox(
      value = tags$p(lfalls_adj_yesterday_text, style = "font-size: 40%;"),
      subtitle = NULL,
      color = "blue"
    )
  })
#------------------------------------------------------------------------------  
#------------------------------------------------------------------------------
# Create info for Status and Stages value boxes
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#----------------------------------------------------------------------------
# CO-OP operational status
#----------------------------------------------------------------------------
  
  # I think this should also be based on flows yesterday (?)
output$coop_ops <- renderUI({
  
# According to the Operations Manual of the WSCA, drought ops
  #  commences when adjusted flow at Little Falls, 
  #  minus the Little Falls flowby, is less than twice
  #  daily Potomac River withdrawals
  if(yesterday_available == 0){
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(por_yesterday_cfs >= por_threshold) {
      text_stage <- "NORMAL"
      text_stage2 <- ""
      color_stage <- green}
    if(por_yesterday_cfs < por_threshold) {
      text_stage <- "DAILY OPS" 
      text_stage2 <- "Daily monitoring & reporting"
      color_stage <- yellow}
    if(lfalls_adj_yesterday() < lfalls_threshold + 2*withdr_pot_5dayfc()) {
      text_stage <- "ENHANCED OPS" 
      text_stage2 <- "Drought operations"
      color_stage <- orange}
  }

  # Below is Luke's code because I asked for changes in box sizes
  div(class="longbox",
      div(class="ibox", style = "background-color:silver",
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class = "p1",paste0("CO-OP operations status "))#,text_stage2))
                  )))),
      div(class="squarei", style = color_stage,
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class="p2",text_stage)
                  ))))
  ) # end div(class="longbox" 
}) # end renderUI
  
#------------------------------------------------------------------------------
# LFAA stage
#------------------------------------------------------------------------------

output$lfaa_alert <- renderUI({

  #
  # sen.last <- last(ts$sen)
  # jrr.last <- last(ts$jrr)
  # sen_stor <- sen.last$stor[1]
  # jrr_ws_stor <- jrr.last$storage_ws[1]
  # jrr_ws_cap_cp <- jrr_cap*jrr_ws_frac
  # shared_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap + jrr_ws_cap_cp)
  #
  if(yesterday_available == 0) {
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(lfalls_adj_yesterday() > withdr_pot_yesterday()/0.5) {
      text_stage <- "NORMAL"
      color_stage <- green
      text_stage2 <- ""}
  
    if(lfalls_adj_yesterday() <= withdr_pot_yesterday()/0.5 
       & lfalls_adj_yesterday() > 
       (withdr_pot_yesterday() + lfalls_threshold)/0.8){
      text_stage <- "ALERT"
      color_stage <- yellow
      text_stage2 <- " (eligible)"}
  
    if(lfalls_adj_yesterday() <= (withdr_pot_yesterday() +
                                  lfalls_threshold)/0.8) 
      {
      text_stage <- "RESTRICTION"
      color_stage <- orange
      text_stage2 <- " (eligible)"}
  
    # if(shared_ws_frac <= 0.02){
    #   text_stage <- "EMERGENCY"
    #   color_stage <- red
    #   text_stage2 <- " (eligible)"}
    }
  
  # Below is Luke's code because I asked for changes in box sizes
  div(class="longbox",
      div(class="ibox", style = "background-color:silver",
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class = "p1",paste0("LFAA stage",text_stage2))
                  )))),
      div(class="squarei", style = color_stage,
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class="p2",text_stage)
                  ))))
      
  ) # end div(class="longbox"
}) # end renderUI
#
#------------------------------------------------------------------------------
# MWCOG drought stage
#------------------------------------------------------------------------------

  # Let daily monitoring trigger be based on POR flow yesterday----------------
  por_yesterday <- round(flows_yesterday.df$por[1]*mgd_to_cfs)
  
  # Tentatively, use POR flow as a surrogate for NOAA's D1 stage---------------
  noaa_d1_surrogate <- 1700 # trigger for yesterday's flow at POR
  
  # Need combined jrr & lsen water supply storage today------------------------
  nbr_storage_yesterday <- storage_nbr_daily_df %>%
    dplyr::filter(date_time == date_today0 - 1)
  reservoir_local_yesterday <- storage_local_daily_bg_df %>%
    dplyr::filter(date_time == date_today0 - 1)
  
  jrr_ws_stor <- nbr_storage_yesterday$jrr_ws[1]
  sen_stor <- reservoir_local_yesterday$seneca[1]
  
  # capacity of jrr ws storage, in BG
  jrr_ws_cap_bg <- jrr_cap*jrr_ws_frac/1000
  
  sen_cap_bg <- sen_cap/1000
  
output$mwcog_stage <- renderUI({

  # sen_last <- last(ts$sen)
  # jrr.last <- last(ts$jrr)
  # sen_stor <- sen.last$stor[1]
  # jrr_ws_stor <- jrr.last$storage_ws[1]

  combined_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap_bg + jrr_ws_cap_bg)
  
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date) {
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(por_yesterday > noaa_d1_surrogate) {
      text_stage <- "NORMAL" 
      text_stage2 <- "- Wise Water Use"
      color_stage <- green}
    if(por_yesterday <= noaa_d1_surrogate) { # surrogate
      # based on NOAA drought status - D1
      # then "notifications" upon 1st release, & when jrr+sen at 75%
      text_stage <- "WATCH" 
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- yellow}
    if(combined_ws_frac <= 0.60){
      text_stage <- "WARNING"
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- orange}
    # if(shared_ws_frac <= 0.05){
    if(combined_ws_frac <= 0.05){
      text_stage <- "EMERGENCY"
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- red}
  }
  
  # Below is Luke's code because I asked for changes in box sizes
  div(class="longbox",
      div(class="ibox", style = "background-color:silver",
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class = "p1",paste0("MWCOG drought stage "))#,text_stage2))
                  )))),
      div(class="squarei", style = color_stage,
          div(class="my_content",
              div(class="table",
                  div(class="table-cell2",
                      p(class="p2",text_stage)
                  ))))
      
      
  ) # end div(class="longbox",
}) # end renderUI
#------------------------------------------------------------------
# Temporary output for QAing purposes
#------------------------------------------------------------------
output$QA_out <- renderValueBox({
  potomac.df <- ts$flows
  sen.df <- ts$sen
  jrr.df <- ts$jrr
  pat.df <- ts$pat
  occ.df <- ts$occ
  QA_out <- paste("Min flow at LFalls = ",
                  round(min(potomac.df$lfalls_obs, na.rm = TRUE)),
                  " mgd",
                  "________ Min sen, jrr, pat, occ stor = ",
                  round(min(sen.df$storage, na.rm = TRUE)), " mg, ",
                  round(min(jrr.df$storage_ws, na.rm = TRUE)), " mg,  ",
                  round(min(pat.df$storage, na.rm = TRUE)), " mg,  ",
                  round(min(occ.df$storage, na.rm = TRUE)),
                  " mg")
  valueBox(
    value = tags$p(QA_out, style = "font-size: 60%;"),
    subtitle = NULL,
    color = "blue"
  )
})
  
#------------------------------------------------------------------
#------------------------------------------------------------------
# Temporary output for QAing
#------------------------------------------------------------------

#------------------------------------------------------------------
#this outputs the last date to the login bar at the top right of the screen.
output$date_text  <- renderText({
  potomac.ts.df <- ts$flows
  test_date <- last(potomac.ts.df$date_time)
  paste("Today's date is ", as.character(date_today0),"  ")
})
#
