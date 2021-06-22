# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Use variable lag times to route flow in cfs
# Based on code by Zachary Smith
# *****************************************************************************
# INPUTS
# *****************************************************************************
# long.df - a df of hourly flow data in long format, with columns:
#    - location: gage location, e.g. por (Potomac River at Point of Rocks)
#    - date_time, in POSIXct date/time format
#    - flow, in cfs
# reach - e.g. por_to_lfalls
# subreach - e.g. por_to_lfalls_1 - por downstream to monoc confluence
#                 por_to_lfalls_2 - monoc to goose confluence
#                 por_to_lfalls_3 - goose to seneca confluence
#                 por_to_lfalls_4 - seneca confluence to wssc intake
#                 por_to_lfalls_5 - wssc intake to gfalls intake
#                 por_to_lfalls_6 - gfalls intake to lfalls
# klag_df - table of lags, with columns
#    - reach
#    - subreach
#    - flow
#    - lag_reach
#    - pct_subreach: lag_subreach = lag_reach*pct_subreach 
#          - should upgrade to subreach lags later
# *****************************************************************************
# OUTPUT
# *****************************************************************************
# df of flows routed from upstream to downstream point in subreach
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

variable_lagk <- function(long.df, location_up, location_down, 
                          myreach, mysubreach, klags.df) {
  #------------------------------------------------------------------------------
  thissubreach <- paste(location_up, "_to_", location_down, sep="")
  klags.sub <- klags.df %>% 
    dplyr::filter(subreach == thissubreach) %>%
    dplyr::mutate(flowlow = flow, lag = lag_reach*pct_subreach) %>%
    dplyr::select(flowlow, lag)
  #------------------------------------------------------------------------------
  predicted.df <- long.df %>%
    dplyr::select(date_time, location_up) %>%
    dplyr::rename(flow = 2) %>% # can rename column number 2 this way
    dplyr::mutate(range = as.character(cut(flow, breaks = klags.sub$flowlow)),
                  lower_flow = as.numeric(gsub("\\(|,.*", "", range)),
                  upper_flow = as.numeric(gsub(".*,|\\]", "", range))) %>%
    dplyr::left_join(klags.sub, by = c("lower_flow" = "flowlow")) %>%
    dplyr::left_join(klags.sub, by = c("upper_flow" = "flowlow")) %>%
    dplyr::rename(lower_lag = lag.x,
                  upper_lag = lag.y) %>%
    dplyr::mutate(lag = lower_lag + ((upper_lag - lower_lag) 
                                     * (flow - lower_flow) 
                                     / (upper_flow - lower_flow)), 
                  date_time_lagged = date_time + lag * 3600) %>%
    dplyr::select(date_time_lagged, flow) %>%
    dplyr::rename(date_time = date_time_lagged)  %>%
    dplyr::mutate(date_time = as.POSIXct(round(date_time, units = "hours"))) %>%
    dplyr::group_by(date_time, flow) %>%
    dplyr::summarise(flow = mean(flow))
  #------------------------------------------------------------------------------
  start.date <- min(predicted.df$date_time, na.rm = TRUE)
  end.date <- max(predicted.df$date_time, na.rm = TRUE)
  date.frame <- date_frame(start.date, end.date, "hour")
  final.df <- left_join(date.frame, predicted.df, by = "date_time") %>%
    # mutate(flow = c(rep(NA, which.min(is.na(flow)) - 1),
    #                      zoo::na.approx(flow)),
    #        site = "predicted") %>%
    dplyr::filter(!is.na(flow))
  names(final.df)[2] <- {{location_down}} 
  #------------------------------------------------------------------------------
  return(final.df)
}
