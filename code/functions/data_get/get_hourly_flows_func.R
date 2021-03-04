#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Create dataframe of recent hourly flows
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# gage_nos = list of USGS gage station numbers, e.g. c("01638500", "01646500")
# gage_names = list of user-selected station names, e.g. c("por", "lfalls")
# flows_empty = dataframe of POXISct date/hours, from start_date to end_date
# start_date = date in the past 
# end_date = usually today's date
# usgs_param = USGS parameter ID number 
# - this code currently just works for usgs_param = "00060" - discharge (mean)
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# dataframe of recent hourly flows, beginning n days in the past
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Comments
#--------------------------------------------------------------------------------
# This code relies on the USGS's package, dataRetrieval
# The USGS times are in UTM and this function converts to EST
#--------------------------------------------------------------------------------

get_hourly_flows_func <- function(gage_nos, gage_names, 
                                  flows_empty,
                                  start_date, 
                                  end_date,
                                  usgs_param) {
  n_gages_hourly <- length(gage_nos)
  flows_df <- flows_empty
  for(i in 1:n_gages_hourly) {
    flows_rt <- dataRetrieval::readNWISuv(siteNumbers = gage_nos[i],
                                          parameterCd = usgs_param, 
                                          startDate = start_date,
                                          endDate = end_date,
                                          tz = "EST" # time zone is Eastern Standard Time
    )
    flows_hourly <- flows_rt %>%
      mutate(date_time = lubridate::round_date(dateTime,
                                               unit = "hour"),
             flows_cfs = X_00060_00000) %>%
      select(date_time, flows_cfs) %>%
      group_by(date_time) %>%
      summarise(flows_cfs = round(mean(flows_cfs), 0), .groups = "keep") %>%
      ungroup()
    
    flows_df <- left_join(flows_df, flows_hourly, by = "date_time")
    names(flows_df)[i+1] <- gage_names[i]
  }
  return(flows_df)
}
