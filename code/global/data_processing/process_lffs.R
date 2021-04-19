# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Apply baseflow correction to LFFS LFalls forecast
# *****************************************************************************
# INPUTS
# *****************************************************************************
# lffs.hourly.cfs.all.df0
# flows.daily.mgd.df - needed to do baseflow correction
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# lffs.daily.mgd.df - with new field, lfalls_lffs_bfc
# lffs.hourly.cfs.df - with new field, lfalls_lffs_bfc
# *****************************************************************************

# Estimate LFalls baseflow as past 30-day minimum of daily average flows
#   - rollapply computes running stats (align = "right" for past aves)
#   - work in MGD

# Add corrections to lffs daily df --------------------------------------------
print("starting process_lffs")

# Do some formatting-----------------------------------------------------------
#   - delete all data prior to 2019; create date_time and date columns
lffs.hourly.cfs.df <- lffs.hourly.cfs.all.df0 %>%
  filter(year >= year(date_today0) - 2) %>%
  dplyr::mutate(date_time = 
                  lubridate::make_datetime(year, month, 
                                           day, minute, second),
                date = lubridate:: round_date(date_time, unit = "days"),
                date = as.Date(date),
                lfalls_lffs_hourly = lfalls_lffs) %>%
  select(date_time, date, lfalls_lffs_hourly)

# Compute LFFS LFalls daily flows ---------------------------------------------
lffs.daily.cfs.df <- lffs.hourly.cfs.df %>%
  select(-date_time) %>%
  group_by(date) %>%
  # average hourlies to get dailies
  summarise(lfalls_lffs = mean(lfalls_lffs_hourly)) %>%
  # summarise(mean(lfalls_lffs), .groups = "keep") %>%
  mutate(date_time = as.Date(date)) %>%
  select(date_time, lfalls_lffs) %>%
  ungroup()

# convert units to MGD
lffs.daily.mgd.df <- lffs.daily.cfs.df %>%
  dplyr::mutate(lfalls = round(lfalls_lffs/mgd_to_cfs, 0)) %>%
  dplyr::select(date_time, lfalls)

# Create df with baseflow corrected flows--------------------------------------
lffs.daily.bfc.mgd.df0 <- 
  left_join(flows.daily.mgd.df, lffs.daily.mgd.df,
                                         by = "date_time") %>%
  # dplyr::select(date_time, lfalls_lffs, lfalls) %>%
  dplyr::select(date_time, lfalls.y, lfalls.x) %>%
  # compute 30-day trailing mins
  dplyr::mutate(min_30day_lffs = 
                  zoo::rollapply(lfalls.y, 30, min,
                                 align = "right", fill = NA),
                min_30day_usgs = 
                  zoo::rollapply(lfalls.x, 30, min,
                                 align = "right", fill = NA),
                # compute "baseflow corrections"
                lfalls_bf_correction = min_30day_usgs - min_30day_lffs)

# save today's correction - to apply to forecasts
lffs_today <- lffs.daily.bfc.mgd.df0 %>%
  filter(date_time == date_today0)
correction_today <- lffs_today$lfalls_bf_correction

lffs.daily.bfc.mgd.df <- lffs.daily.bfc.mgd.df0 %>%
  mutate(lfalls_bf_correction = case_when(
    date_time <= date_today0 ~ lfalls_bf_correction,
    date_time > date_today0 ~ correction_today,
    TRUE ~ -9999),
    lfalls_lffs_bfc = round(lfalls.y + 
                  lfalls_bf_correction, 0),
    lfalls_obs = lfalls.x,
    lfalls_lffs = lfalls.y) %>%
  dplyr::select(date_time, lfalls_obs, lfalls_lffs,
                lfalls_lffs_bfc, lfalls_bf_correction)

# Compute lfalls stats------------------------------------------------------------
# lfalls.daily.mgd.df <- 
  
# Create hourly df with daily lffs corrections -----------------------------------
lffs.daily.corrections.df <- lffs.daily.bfc.mgd.df %>%
  mutate(date = as.Date(date_time)) %>%
  select(-date_time) %>%
  select(date, lfalls_obs, lfalls_lffs,
         lfalls_lffs_bfc, lfalls_bf_correction)

lffs.hourly.mgd.df <- left_join(lffs.hourly.cfs.df,
                                 lffs.daily.corrections.df, by = "date") %>%
  mutate(lfalls_lffs_hourly_bfc = lfalls_lffs_hourly/mgd_to_cfs
         + lfalls_bf_correction)