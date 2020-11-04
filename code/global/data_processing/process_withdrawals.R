# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It processes the WMA withdrawal time series data
# *****************************************************************************
# INPUTS
# *****************************************************************************
# withdrawals.hourly.mgd.df0 - table of hourly withdrawals created by import.R
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# withdr.hourly.df - WMA hourly withdrawals in MGD
# demands.daily.df - WMA daily demands in MGD
# production.daily.df - WMA daily production in MGD
# *****************************************************************************

# Get df in necessary format --------------------------------------------------

withdr.hourly.df <- withdrawals.hourly.mgd.df0 %>%
  dplyr::rename_with(tolower) %>% # switch to lowercase col names
  dplyr::rename(date_time = datetime,
                w_wssc_pot = wssc_pot,
                w_wssc_pat = wssc_pa,
                w_fw_pot = fw_pot,
                w_fw_occ = fw_oc,
                w_lw_pot = lw_pot) %>%
  filter(!is.na(date_time)) %>% # sometime these are sneaking in
  dplyr::mutate(date_time = as.POSIXct(date_time, tz = "EST"),
                date = round_date(date_time, unit = "days"),
                w_wa_pot = wa_gf + wa_lf,
                # w_fw_e = 70, w_fw_c = 20,
                # total Potomac withdrawals:
                w_pot_total = w_fw_pot + w_wssc_pot 
                + w_lw_pot + w_wa_pot - discharge_broadrun) %>%
  dplyr::select(-wa_gf, -wa_lf)

# Compute daily withdrawals ---------------------------------------------------
demands.daily.df <- withdr.hourly.df %>%
  select(-date_time) %>%
  group_by(date) %>%
  # summarise_all(mean) %>%
  summarise(across(everything(), mean), .groups = "keep") %>%
  # temporarily go back to d (demand) instead of w (withdrawal)
  rename(date_time = date) %>%
  mutate(date_time = as.Date(date_time),
         d_wa = w_wa_pot*withdr_to_demands,
         # d_fw_w, d_fw_e, d_fw_c are used elsewhere; rethink this later
         d_fw_w = w_fw_pot*withdr_to_demands,
         d_fw_e = w_fw_occ*withdr_to_demands, 
         d_fw_c = d_fw_c,
         d_fw = (w_fw_pot + w_fw_occ)*withdr_to_demands,
         d_wssc = (w_wssc_pot + w_wssc_pat)*withdr_to_demands,
         d_lw = w_lw_pot*withdr_to_demands,
         d_pot_total = w_pot_total*withdr_to_demands
  ) %>%
  ungroup()

# # Fill in df with full year of demands so that app won't break --------------
ncols <- length(demands.daily.df[1,])
data_first_date <- head(demands.daily.df$date_time, 1)
data_last_date <- tail(demands.daily.df$date_time, 1)
data_last <- tail(demands.daily.df[, 2:ncols], 1)
data_first <- head(demands.daily.df[, 2:ncols], 1)
current_year <- year(data_last_date)
days_left_in_year <- as.numeric(date_dec31 - data_last_date)
next_date <- data_last_date

for(i in 1:days_left_in_year) {
  next_date <- next_date + days(1)
  next_row <- cbind(date_time = next_date, data_last)
  demands.daily.df <- rbind(demands.daily.df, next_row)
}

# Fill in df with constant past demands so that app won't break ---------------
data_first_date <- as.Date(data_first_date)
days_prior_in_year <- as.numeric(difftime(data_first_date,
                                          date_jan1, 
                                          units = "days"))
prior_date <- data_first_date
for(i in 1:days_prior_in_year) {
  prior_date <- prior_date - days(1)
  prior_row <- cbind(date_time = prior_date, data_first)
  demands.daily.df <- rbind(demands.daily.df, prior_row)
}
demands.daily.df <- demands.daily.df %>%
  dplyr::arrange(date_time) %>%
  dplyr::mutate(date_time = round_date(date_time, unit = "days"))

