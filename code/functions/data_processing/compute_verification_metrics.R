# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Compute time series (ts) simulation evaluation metrics 
#   - works for both flows and withdrawals
# *****************************************************************************
# INPUTS
# *****************************************************************************
# obs_df0 - dataframe with observed ts values, eg flows.daily.mgd.df
# sim_df0 - dataframe with simulated ts values, eg lffs.daily.bfc.mgd.df
# date_first - earliest date of ts to be used
# date_last - latest date of ts to be used
# location - location of ts, eg lfalls, por, ... w_fw_pot, ...
# sim_type - eg lffs, lffs_bfc, marfc
# lowflow_threshold - obs flow threshold for computing lowflow stats
# *****************************************************************************
# OUTPUT
# *****************************************************************************
# stats - a one-row df with
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Add future flows to selected gages, estimated by simple "recession" algorithm
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# obs_df0 <- flows.daily.cfs.df
# sim_df0 <- lffs.daily.mgd.df
# date_first <- date_today0 - 30
# date_last <- date_today0 - 1
# location <- "lfalls"
# simtype <- "lffs"
# lowflow_threshold <- 200000

verification_metrics_daily_func <- function(
  obs_df0, 
  sim_df0,
  date_first,
  date_last,
  location,
  simtype,
  lowflow_threshold) {
  
  obs_df <- obs_df0 %>%
    # this worked with flows.daily.cfs.df but not here:
    # mutate(obs = across(matches(location))) %>%
    # mutate(obs = lfalls) %>%
    dplyr::mutate(obs = .data[[location]]) %>% # this works here
    dplyr::filter(obs <= lowflow_threshold) %>%
    dplyr::select(date_time, obs)
  ts_df0 <- sim_df0 %>%
    # mutate(sim = c_across(starts_with(location))) %>% 
    mutate(sim = c_across(matches(paste(location, "_", simtype, sep="")))) %>%
    # dplyr::mutate(sim = .data[[location]]) %>% # this doesn't work here
    dplyr::select(date_time, sim)
  
  # left_join will only include records with dates in obs_df - ie only low flow
  ts_df <- left_join(obs_df, ts_df0,
                      by = "date_time") %>%
    mutate(location = location, simtype = simtype) %>% # the name of the fc
    drop_na()
  
  # Need mean of obs - need for Nash-Sutcliffe (NSE)
  obs_mean <- mean(ts_df$obs)
  
  ts_df <- ts_df %>%
    mutate(err = obs - sim,
           ae = abs(obs - sim),
           ape = 100*ae/obs,
           se = (obs - sim)^2,
           nse_denominator = (obs - obs_mean)^2 )
  stats_df <- ts_df %>%
    summarise(across(where(is.character), first), # this grabs location name
              across(where(is.numeric), mean), # this computes means
              count = n()) %>% # this provides count of no. of records
    dplyr::rename(mape = ape, mae = ae, sse = se, bias = err) %>%
    dplyr::select(-obs, -sim)
  
  # Add Nash-Sutcliffe efficiency value
  nse <- 1 - stats_df$sse[1]/stats_df$nse_denominator[1]
  stats_df <- stats_df %>%
    mutate(nse = nse,
           date_first = as.character(date_first),
           date_last = as.character(date_last)) %>%
    select(-nse_denominator) %>%
    relocate(count, .after = last_col())
  
  return(stats_df)
}

# test <- verification_metrics_daily_func(
#   obs_df0 = flows.daily.cfs.df,
#   sim_df0 = lffs.daily.mgd.df,
#   date_first = date_today0 - 30,
#   date_last = date_today0 - 1,
#   location = "lfalls",
#   simtype = "lffs",
#   lowflow_threshold = 200000
# )



