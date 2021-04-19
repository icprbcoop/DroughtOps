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
  source("code/server/lffs_qa/lffs_qa_server.R", local=TRUE)

  #Data Download tab
  # source("code/server/download_data/download_data_server.R", local=TRUE)
  
  # To save time can generate input ts files for input/ts/current/-------------
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
    wma_withdrawals_temp <- withdrawals.hourly.mgd.df0 %>%
      #  write header & 14 dummy rows, to mimic file from the Data Portal
      # add_row(DateTime = "DateTime",
      #         FW_POT = "FW_POT",
      #         WSSC_POT = "WSSC_POT",
      #         WA_GF = "WA_GF",
      #         WA_LF = "WA_LF",
      #         LW_POT = "LW_POT",
      #         LW_FW = "LW_FW",
      #         FW_OC = "FW_OC",
      #         WSSC_PA = "WSSC_PA",
      #         LW_BR = "LW_BR", .before=1) %>%
      # add_row(DateTime = rep("dummy-row", 14), .before=1) 
    add_row(FW_POT = rep(-99999.9, 16), .before=1)
    write_csv(wma_withdrawals_temp, paste(ts_output,
                                       "wma_withdrawals.csv",
                                       sep=""),
              col_names = FALSE)
  })
  
  }) # end shinyServer

