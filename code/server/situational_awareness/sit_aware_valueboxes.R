# Finally, create boxes with values and triggers
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#------------------------------------------------------------------
# Create value for Potomac River flow at Point of Rocks yesterday
#------------------------------------------------------------------
  flows_yesterday.df <- flows.daily.mgd.df %>%
    filter(date_time == date_today0 - 1)
  print(flows_yesterday.df$date_time[1])

  output$por_flow <- renderValueBox({
  por_threshold <- 2000 # (cfs) CO-OP's trigger for daily monitoring/reporting
  
  # potomac.ts.df <- ts$flows # trying to get rid of sim code in main ops tabs
  por_mgd <- flows_yesterday.df$por[1]
  
  # Error trapping in case there is no data available for yesterday:
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date)
     por_flow <- paste("Flow at Point of Rocks yesterday = ",
                    round(por_mgd*mgd_to_cfs), " cfs",
                    " (", round(por_mgd), " MGD)", sep = "") else
                      por_flow <- "Yesterday's Point of Rocks daily flow value is not available"
  valueBox(
    value = tags$p(por_flow, style = "font-size: 60%;"),
    subtitle = NULL,
         # color = if (por_flow >= por_threshold) "green" else "yellow"
    color = "blue"
  )
})

#------------------------------------------------------------------
# Create value for Potomac River flow at Little Falls yesterday
#------------------------------------------------------------------
output$lfalls_obs <- renderValueBox({
  lfalls_mgd <- flows_yesterday.df$lfalls[1]
  lfalls_obs <- paste("Flow at Little Falls yesterday = ",
                      round(lfalls_mgd*mgd_to_cfs),
                      " cfs (", round(lfalls_mgd),
                      " MGD)", sep = "")
  # Error trapping in case there is no data available for yesterday:
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date)
    lfalls_obs <- paste("Flow at Little Falls yesterday = ",
                      round(lfalls_mgd*mgd_to_cfs), " cfs",
                      " (", round(lfalls_mgd), " MGD)", sep = "") else
                        lfalls_obs <- "Yesterday's Little Falls daily flow value is not available"
  
  valueBox(
    value = tags$p(lfalls_obs, style = "font-size: 60%;"),
    subtitle = NULL,
    color = "blue"
  )
})
#------------------------------------------------------------------
# Create info on CO-OP operational status
#------------------------------------------------------------------
# I think this should also be based on flows yesterday (?)
output$coop_ops <- renderUI({
  por_flow <- round(flows_yesterday.df$por[1]*mgd_to_cfs)
  withdr_pot <- flows_yesterday.df$d_pot_total[1]  
  lfalls_adj <- flows_yesterday.df$lfalls[1] + withdr_pot

  #
  # if(is.na(por_flow)) {
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date) {
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(por_flow > 2000) {
      text_stage <- "NORMAL"
      text_stage2 <- ""
      color_stage <- green}
    if(por_flow <= 2000) {
      text_stage <- "DAILY OPS" 
      text_stage2 <- "Daily monitoring & reporting"
      color_stage <- yellow}
    if(lfalls_adj <= 100 + 2*withdr_pot) {
      text_stage <- "HOURLY OPS" 
      text_stage2 <- "Hourly monitoring & reporting"
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
  
#------------------------------------------------------------------
# Create info on LFAA status
#------------------------------------------------------------------
#
output$lfaa_alert <- renderUI({
  withdr_pot <- flows_yesterday.df$d_pot_total[1]  
  lfalls_adj <- flows_yesterday.df$lfalls[1] + withdr_pot 
  #
  sen.last <- last(ts$sen)
  jrr.last <- last(ts$jrr)
  sen_stor <- sen.last$stor[1]
  jrr_ws_stor <- jrr.last$storage_ws[1]
  jrr_ws_cap_cp <- jrr_cap*jrr_ws_frac
  shared_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap + jrr_ws_cap_cp)
  #
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date) {
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(lfalls_adj > withdr_pot/0.5) {
      text_stage <- "NORMAL"
      color_stage <- green
      text_stage2 <- ""}
  
    if(lfalls_adj <= withdr_pot/0.5 & lfalls_adj > (withdr_pot + 100)/0.8){
      text_stage <- "ALERT"
      color_stage <- yellow
      text_stage2 <- " (eligible)"}
  
    if(lfalls_adj <= (withdr_pot + 100)/0.8) {
      text_stage <- "RESTRICTION"
      color_stage <- orange
      text_stage2 <- " (eligible)"}
  
    if(shared_ws_frac <= 0.02){
      text_stage <- "EMERGENCY"
      color_stage <- red
      text_stage2 <- " (eligible)"}
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
#------------------------------------------------------------------
# Create info on MWCOG Drought Plan stage
#------------------------------------------------------------------
# 
output$mwcog_stage <- renderUI({
  # flows.last <- last(ts$flows)
  # por_flow <- flows.last$por_nat[1]*mgd_to_cfs
  por_flow <- round(flows_yesterday.df$por[1]*mgd_to_cfs)
  # withdr_pot <- flows_yesterday.df$d_pot_total[1]  
  # lfalls_adj <- flows_yesterday.df$lfalls[1] + withdr_pot 
  
  sen.last <- last(ts$sen)
  jrr.last <- last(ts$jrr)
  sen_stor <- sen.last$stor[1]
  jrr_ws_stor <- jrr.last$storage_ws[1]
  jrr_ws_cap_cp <- jrr_cap*jrr_ws_frac
  shared_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap + jrr_ws_cap_cp)
  
  # would POR at 1500 cfs work as a surrogate for NOAA's D1 stage?
  noaa_d1_surrogate <- 1700
  
  if(flows_yesterday.df$date_time[1] > daily_flow_data_last_date) {
    text_stage <- "NO DATA"
    text_stage2 <- ""
    color_stage <- red} # alas there is no grey
  else {
    if(por_flow > noaa_d1_surrogate) {
      text_stage <- "NORMAL" 
      text_stage2 <- "- Wise Water Use"
      color_stage <- green}
    if(por_flow <= noaa_d1_surrogate) { # surrogate
      # based on NOAA drought status - D1
      # then "notifications" upon 1st release, & when jrr+sen at 75%
      text_stage <- "WATCH" 
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- yellow}
    if(shared_ws_frac <= 0.60){
      text_stage <- "WARNING"
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- orange}
    # if(shared_ws_frac <= 0.05){
    if(shared_ws_frac <= 0.05){
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
