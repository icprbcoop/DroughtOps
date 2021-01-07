#******************************************************************
# server.R defines reactive values & uses observeEvent to do simulation,
# then produces output
#******************************************************************
shinyServer(function(input, output, session) {

  # Situational awareness tab
  source("code/server/situational_awareness/sit_aware_plots.R", local=TRUE)
  source("code/server/situational_awareness/sit_aware_valueboxes.R", local=TRUE)
  source("code/server/situational_awareness/sit_aware_MD.R", local=TRUE)
  source("code/server/situational_awareness/sit_aware_VA.R", local=TRUE)
  
  # One day ops tab
  source("code/server/one_day_ops/one_day_ops_server.R", local=TRUE)
  
  # N Branch ops tab
  source("code/server/nbr_ops/nbr_ops_server.R", local=TRUE)
  
  # Long term tab
  source("code/server/demands/demands_server.R", local=TRUE)
  
  # Simulation tab
  source("code/server/simulation/sim_run.R", local=TRUE)  
  source("code/server/simulation/sim_plots.R", local=TRUE)
  
  # LFFS QA "sandbox" tab
  # source("code/server/lffs_qa_server.R", local=TRUE)

  #Data Download tab
  source("code/server/download_data/download_data_server.R", local=TRUE)
  
  # To save time can generate input ts files for input/ts/current/
  observeEvent(input$write_ts2, {
    flows_daily_temp <- flows.daily.cfs.df0 %>%
      dplyr::mutate(date = date_time) %>%
      select(-date_time) %>%
      relocate(date)
    write_csv(flows_daily_temp, paste(ts_output, 
                                       "flows_daily_cfs.csv",
                                       sep=""))
    flows_hourly_temp <- flows.hourly.cfs.df0 %>%
      dplyr::mutate(date = date_time) %>%
      select(-date_time) %>%
      relocate(date)
    write_csv(flows_hourly_temp, paste(ts_output, 
                                       "flows_hourly_cfs.csv",
                                       sep=""))
  })
  
  }) # end shinyServer

