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
  # source("code/server/lffs_qa/lffs_qa_server.R", local=TRUE)
  source("code/server/qa/qa_server.R", local=TRUE)

  #Data Download tab
  # source("code/server/download_data/download_data_server.R", local=TRUE)
  
  # Code for sidebar button to write input ts files in input/ts/current/-------
  observeEvent(input$write_ts2, {
    flows_daily_temp <- flows.daily.cfs.df0 # %>%
      # dplyr::mutate(date = date_time) %>%
      # select(-date_time) %>%
      # relocate(date)
    write_csv(flows_daily_temp, paste(ts_path, 
                                       "flows_daily_cfs.csv",
                                       sep=""))
    
      flows_rt_temp <- flows_rt_long_cfs_df0 %>%
      # need to change datetime to character or will be written as UTC
      dplyr::mutate(date = as.character(date_time)) %>%
      select(-date_time) %>%
      relocate(date)
    write_csv(flows_rt_temp, paste(ts_path, 
                                       "flows_rt_cfs.csv",
                                       sep=""))
    
    wma_withdrawals_temp <- withdrawals.hourly.mgd.df0 # %>%
      #  add 16 dummy rows, to mimic file from the Data Portal
    # add_row(FW_POT = rep(-99999.9, 16), .before=1)
    write_csv(wma_withdrawals_temp, paste(ts_path,
                                       "wma_withdrawals.csv",
                                       sep=""),
              col_names = TRUE)
    
    wma_storage_local_temp <- storage_local_daily_bg_df # %>%
      #  add 3 dummy rows, to mimic file from the Data Portal
      # add_row(patuxent = rep(-99999.9, 3), .before=1)
    write_csv(wma_storage_local_temp, paste(ts_path,
                                          "wma_storage_local_daily.csv",
                                          sep=""),
              col_names = TRUE)
    
    wma_storage_nbr_temp <- storage_nbr_df # %>%
      #  add 3 dummy rows, to mimic file from the Data Portal
      # add_row(patuxent = rep(-99999.9, 3), .before=1)
      write_csv(wma_storage_nbr_temp, paste(ts_path,
                                              "wma_storage_nbr.csv",
                                              sep=""),
                col_names = TRUE)
      
      lffs_hourly_temp <- lffs.hourly.cfs.all.df0  %>%
      #  add 25 dummy rows, to mimic file from the Data Portal
      add_row(year = rep(1888, 25), .before=1)
      write_csv(lffs_hourly_temp, paste(ts_path,
                                            "PM7_4820_0001.flow",
                                            sep=""),
                col_names = TRUE)
  })
  
  # Code to write today's forecasts to /data/forecasts-------------------------
  observeEvent(input$write_fcs, {
  
    write_csv(lffs.daily.fc.mgd.df, 
              paste("data/forecasts/lffs_daily/",
                    "flows_mgd_",
                    date_today0,
                    ".csv",
                    sep=""))
    
    write_csv(lffs.daily.bfc.fc.mgd.df, 
              paste("data/forecasts/lffs_bfc_daily/",
                    "flows_mgd_",
                    date_today0,
                    ".csv",
                    sep=""))
  
    write_csv(klag.daily.fc.mgd.df, 
              paste("data/forecasts/klag_corrected_daily/",
                    "flows_mgd_",
                    date_today0,
                    ".csv",
                    sep=""))
    
    write_csv(prrism.daily.fc.mgd.df, 
              paste("data/forecasts/prrism_constant_lag/",
                    "flows_mgd_",
                    date_today0,
                    ".csv",
                    sep=""))
    
    write_csv(withdrawals.daily.df,
              paste("data/forecasts/coop1_withdrawals_daily/",
                    "withdrawals_mgd_",
                    date_today0,
                    ".csv",
                    sep=""))
  })
  
  }) # end shinyServer

