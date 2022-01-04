# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R, so df's are globally accessible
# Applies baseflow correction to LFFS LFalls forecast
# Right now LFFS output begins with 2014, but delete 3 months of "spin-up"
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
# lffs.daily.bfc.mgd.df - with the fields:
#    - lfalls_obs
#    - lfalls_lffs
#    - lfalls_lffs_bfc
#    - lfalls_lffs_bf_correction
# lffs.hourly.mgd.df - includes the fields:
#    - lfalls_lffs_hourly
#    - lfalls_lffs_hourly_bfc
# lffs.daily.fc.mgd.df - for archiving, with the fields
#    - date
#    - lfalls, ... (mgd)
#    - date_fc
#    - length_fc (days)
# lffs.daily.bfc.fc.mgd.df - for archiving, with the fields
#    - date
#    - lfalls, ... (mgd)
#    - date_fc
#    - length_fc (days)
# *****************************************************************************

print("starting process_lffs")
# Right now LFFS output begins with 2014, but delete 3 months of "spin-up"

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Compute LFFS baseflow-corrected and apply to daily & hourly data
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#   - estimate is past 30-day minimum of daily average flows
#   - rollapply computes running stats (align = "right" for past aves)
#   - work in MGD


# First do some formatting-----------------------------------------------------
# create date_time and date columns
lffs.hourly.cfs.df <- lffs.hourly.cfs.all.df0 %>%
  dplyr::mutate(date_time = 
                  lubridate::make_datetime(year, month, 
                                           day, minute, 
                                           second, tz = "EST"),
                date = lubridate:: floor_date(date_time, unit = "days"),
                date = as.Date(date),
                lfalls_lffs_hourly = lfalls_lffs) %>%
  dplyr::filter(date_time >= as.Date("2014-04-01")) %>%
  select(date_time, date, lfalls_lffs_hourly)

# Compute LFFS LFalls daily flows ---------------------------------------------
lffs.daily.cfs.df <- lffs.hourly.cfs.df %>%
  select(-date_time) %>%
  group_by(date) %>%
  # average hourlies to get dailies
  summarise(lfalls_lffs_daily = mean(lfalls_lffs_hourly)) %>%
  mutate(date_time = as.Date(date)) %>%
  select(date_time, lfalls_lffs_daily) %>%
  ungroup()

# convert units to MGD
lffs.daily.mgd.df <- lffs.daily.cfs.df %>%
  dplyr::mutate(lfalls_lffs_daily = round(lfalls_lffs_daily/mgd_to_cfs, 0)) %>%
  dplyr::select(date_time, lfalls_lffs_daily)

# Create df with baseflow corrected flows--------------------------------------
lffs.daily.bfc.mgd.df00 <- 
  left_join(flows.daily.mgd.df, lffs.daily.mgd.df,
                                         by = "date_time") %>%
  filter(date_time <= date_today0 + 15)

# Fill in missing lfalls (obs) using interpolation 
#     or rollapply may fail when there are NA's
lffs.daily.bfc.mgd.df00$lfalls <- 
  zoo::na.approx(lffs.daily.bfc.mgd.df00$lfalls, na.rm = FALSE)

lffs.daily.bfc.mgd.df0 <- lffs.daily.bfc.mgd.df00 %>%
  dplyr::select(date_time, lfalls, lfalls_lffs_daily) %>%
  # compute 30-day trailing mins
  # lfalls.y is from lffs; lfalls.x is from USGS
  dplyr::mutate(min_30day_lffs = 
                  zoo::rollapply(lfalls_lffs_daily, 30, min,
                                 align = "right", fill = "extend"),
                min_30day_usgs = 
                  zoo::rollapply(lfalls, 30, min,
                                 align = "right", fill = "extend"),
                # compute "baseflow corrections"
                lfalls_bf_correction = min_30day_usgs - min_30day_lffs)

# save today's correction - to apply to forecasts
lffs_today <- lffs.daily.bfc.mgd.df0 %>%
  filter(date_time == date_today0)
correction_today <- lffs_today$lfalls_bf_correction[1]

lffs.daily.bfc.mgd.df <- lffs.daily.bfc.mgd.df0 %>%
  mutate(lfalls_bf_correction = case_when(
    date_time <= date_today0 ~ lfalls_bf_correction,
    date_time > date_today0 ~ correction_today,
    TRUE ~ -9999),
    lfalls_lffs_bfc = round(lfalls_lffs_daily + 
                  lfalls_bf_correction, 0),
    lfalls_obs = lfalls) %>%
  dplyr::select(date_time, lfalls_obs, lfalls_lffs_daily,
                lfalls_lffs_bfc, lfalls_bf_correction)
  
# Create hourly df with daily lffs corrections -----------------------------------
lffs.daily.corrections.df <- lffs.daily.bfc.mgd.df %>%
  mutate(date = as.Date(date_time)) %>%
  select(-date_time) %>%
  select(date, lfalls_obs, lfalls_lffs_daily,
         lfalls_lffs_bfc, lfalls_bf_correction)

lffs.hourly.mgd.df <- left_join(lffs.hourly.cfs.df,
                                 lffs.daily.corrections.df, by = "date") %>%
  mutate(lfalls_lffs_hourly_bfc = lfalls_lffs_hourly/mgd_to_cfs
         + lfalls_bf_correction)

# Create today's recent and forecasted daily flows for archiving---------------
lffs.daily.fc.mgd.df <- lffs.daily.mgd.df %>%
  dplyr::filter(date_time>=date_today0 - 30) %>%
  dplyr::rename(date = date_time,
                lfalls = lfalls_lffs_daily) %>%
  dplyr::mutate(date_fc = date_today0,
                length_fc = as.integer(date - date_fc))

lffs.daily.bfc.fc.mgd.df <- lffs.daily.bfc.mgd.df %>%
  dplyr::filter(date_time>=date_today0 - 30) %>%
  dplyr::rename(date = date_time,
                lfalls = lfalls_lffs_bfc) %>%
  dplyr::mutate(date_fc = date_today0,
                length_fc = as.integer(date - date_fc)) %>%
  dplyr::select(date, lfalls, date_fc, length_fc)