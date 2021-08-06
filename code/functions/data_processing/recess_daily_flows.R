# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Create full year of daily time series - adding recession flows
#    - also adding LFalls fc from upstream gages - constant lags
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows_daily_cfs.csv - current downloaded daily flow data, beginning Jan 1
# demands.daily.df - WMA supplier daily withdrawal data
# daily_flow_data_last_date - date of last day of available data
# n_cols - number of columns in flows_daily_cfs.csv
# *****************************************************************************
# OUTPUT
# *****************************************************************************
# flows.daily.mgd.df
#   - units changed from cfs to mgd
#   - recession flows added based on past 3-day min flows
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Add future flows to selected gages, estimated by simple "recession" algorithm
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


recess_daily_flows_func <- function(flows.daily.cfs.df, 
                                    # demands.daily.mgd.df, 
                                    daily_flow_data_last_date,
                                    n_cols) {
  
  # # Convert flows to mgd --------------------------------------------------------
  # func_cfs_to_mgd <- function(cfs) {round(cfs/mgd_to_cfs,0)}
  # flows.daily.mgd.df <- flows.daily.cfs.df %>%
  #   dplyr::mutate_at(2:n_cols, func_cfs_to_mgd)
  
  # Grab the 3 most recent records, for use in recession estimates --------------
  flows.last3days.df <- flows.daily.cfs.df %>%
    filter(date_time <= daily_flow_data_last_date) %>%
    tail(3)
  
  # Minimum of past 3 days flow will be starting point for recessions
  recess_mins <- flows.last3days.df %>%
    summarise_all(min)
  
  # # Add Potomac withdrawals -----------------------------------------------------
  # flows.daily.mgd.df <- left_join(flows.daily.mgd.df, demands.daily.df,
  #                                 by = "date_time")
  
  # Compute recession flows for future dates ------------------------------------
  # Currently using placeholder recession coefficients of 0.04
  #   - should read recession coeffs from parameters.R file
  
  flows.daily.cfs.df <- flows.daily.cfs.df %>%
    
    # recess por:
    dplyr::mutate(por = case_when(
      date_time < date_today0 ~ por,
      date_time >= date_today0 ~ recess_mins$por
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Goose
    dplyr::mutate(goose = case_when(
      date_time < date_today0 ~ goose,
      date_time >= date_today0 ~ recess_mins$goose
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Seneca Cr (later should take into acc. wwtps, releases)
    dplyr::mutate(seneca = case_when(
      date_time < date_today0 ~ seneca,
      date_time >= date_today0 ~ recess_mins$seneca
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Monocacy, Jug Br
    dplyr::mutate(monoc_jug = case_when(
      date_time < date_today0 ~ monoc_jug,
      date_time >= date_today0 ~ recess_mins$monoc_jug
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Kitzmiller
    dplyr::mutate(kitzmiller = case_when(
      date_time < date_today0 ~ kitzmiller,
      date_time >= date_today0 ~ recess_mins$kitzmiller
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Bennett
    dplyr::mutate(bennett = case_when(
      date_time < date_today0 ~ bennett,
      date_time >= date_today0 ~ recess_mins$bennett
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Cedar
    dplyr::mutate(cedar = case_when(
      date_time < date_today0 ~ cedar,
      date_time >= date_today0 ~ recess_mins$cedar
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Unity
    dplyr::mutate(unity = case_when(
      date_time < date_today0 ~ unity,
      date_time >= date_today0 ~ recess_mins$unity
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Cattail
    dplyr::mutate(cattail = case_when(
      date_time < date_today0 ~ cattail,
      date_time >= date_today0 ~ recess_mins$cattail
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9)) %>%
    
    # recess Hawlings
    dplyr::mutate(hawlings = case_when(
      date_time < date_today0 ~ hawlings,
      date_time >= date_today0 ~ recess_mins$hawlings
      *exp(-0.04*as.numeric((date_time - date_today0))),
      TRUE ~ -9999.9))
  
  return(flows.daily.cfs.df)
}



