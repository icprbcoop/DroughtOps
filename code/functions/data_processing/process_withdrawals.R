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
# withdrawals.hourly.df - WMA hourly withdrawals in MGD
# demands.daily.df - WMA daily demands in MGD
# production.daily.df - WMA daily production in MGD
# *****************************************************************************

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Preliminary info-------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#   - might be temporary - FW Central SA demand - water purchased from WA
d_fw_c <- 10 # MGD

# move to parameters.R later - factor for converting withdrs to demands:
withdrawal_to_production <- 0.97 # consistent with PRRISM "production loss" rates

# Get df in necessary format --------------------------------------------------
withdrawals.hourly.df <- withdrawals.hourly.mgd.df0 %>%
  dplyr::rename_with(tolower) %>% # switch to lowercase col names
  dplyr::rename(date_time = datetime,
                w_wssc_pot = wssc_pot,
                w_wssc_pat = wssc_pa,
                w_fw_pot = fw_pot,
                w_fw_occ = fw_oc,
                w_wa_gf = wa_gf,
                w_wa_lf = wa_lf,
                w_lw_pot = lw_pot,
                disch_lw_pot = lw_br) %>%
  filter(!is.na(date_time)) %>% # sometime these are sneaking in
  dplyr::mutate(date_time = as.POSIXct(date_time, tz = "EST"),
                date = round_date(date_time, unit = "days"))

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Solve PROBLEM: LW values can be spotty and end today
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# LS Broad Run discharge-------------------------------------------------------
# First find last and first available values
lw_ts0 <- withdrawals.hourly.df %>%
  dplyr::select(date_time, disch_lw_pot)
lw_ts <- drop_na(lw_ts0, disch_lw_pot) # drop all NA's
lw_ts_last <- tail(lw_ts$disch_lw_pot,1) # paste values into 1st & last rows
lw_ts_first <- head(lw_ts$disch_lw_pot,1)

# Fill in last disch_lw_pot NA with last available value
ltemp <- length(withdrawals.hourly.df$disch_lw_pot)
withdrawals.hourly.df$disch_lw_pot[ltemp] <- lw_ts_last
withdrawals.hourly.df$disch_lw_pot[1] <- lw_ts_first

# Now interpolate Broad Run discharge data column
withdrawals.hourly.df$disch_lw_pot <- 
  zoo::na.approx.default(withdrawals.hourly.df$disch_lw_pot)

# LW Potomac withdrawal--------------------------------------------------------
# First find last and first available values
lw_ts0 <- withdrawals.hourly.df %>%
  dplyr::select(date_time, w_lw_pot)
lw_ts <- drop_na(lw_ts0, w_lw_pot) # drop all NA's
lw_ts_last <- tail(lw_ts$w_lw_pot,1) # paste values into 1st & last rows
lw_ts_first <- head(lw_ts$w_lw_pot,1)

# Fill in last disch_lw_pot NA with last available value
ltemp <- length(withdrawals.hourly.df$w_lw_pot)
withdrawals.hourly.df$w_lw_pot[ltemp] <- lw_ts_last
withdrawals.hourly.df$w_lw_pot[1] <- lw_ts_first

# Now interpolate Broad Run discharge data column
withdrawals.hourly.df$w_lw_pot <- 
  zoo::na.approx.default(withdrawals.hourly.df$w_lw_pot)

# LW FW purchase--------------------------------------------------------
# First find last and first available values
lw_ts0 <- withdrawals.hourly.df %>%
  dplyr::select(date_time, lw_fw)
lw_ts <- drop_na(lw_ts0, lw_fw) # drop all NA's
lw_ts_last <- tail(lw_ts$lw_fw,1) # paste values into 1st & last rows
lw_ts_first <- head(lw_ts$lw_fw,1)

# Fill in last disch_lw_pot NA with last available value
ltemp <- length(withdrawals.hourly.df$lw_fw)
withdrawals.hourly.df$lw_fw[ltemp] <- lw_ts_last
withdrawals.hourly.df$lw_fw[1] <- lw_ts_first

# Now interpolate Broad Run discharge data column
withdrawals.hourly.df$lw_fw <- 
  zoo::na.approx.default(withdrawals.hourly.df$lw_fw)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Compute daily withdrawals for graphing
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

withdrawals.daily.df <- withdrawals.hourly.df %>%
  select(-date_time) %>%
  group_by(date) %>%
  # summarise_all(mean) %>%
  summarise(across(everything(), mean), .groups = "keep") %>%
  rename(date_time = date) %>%
  mutate(date_time = as.Date(date_time),
         w_wa = w_wa_gf + w_wa_lf,
         w_lw_br = - disch_lw_pot,
         w_pot_total = w_wa + w_fw_pot + w_wssc_pot + w_lw_pot,
         w_pot_total_net = w_pot_total + w_lw_br) %>%
  ungroup()

# Compute daily production for graphing----------------------------------------
production.daily.df <- withdrawals.daily.df %>%
  mutate(p_wa = w_wa*withdrawal_to_production,
         p_fw_w = w_fw_pot*withdrawal_to_production,
         p_fw_e = w_fw_occ*withdrawal_to_production, 
         p_fw = p_fw_w + p_fw_e,
         p_wssc = (w_wssc_pot + w_wssc_pat)*withdrawal_to_production,
         p_wssc_pot = w_wssc_pot*withdrawal_to_production,
         p_lw = w_lw_pot*withdrawal_to_production,
         p_coop_total = p_wa + p_fw + p_wssc,
         p_wma_total = p_wa + p_fw + p_wssc + p_lw) %>%
  select(date_time, p_wa, p_fw, p_wssc, p_wssc_pot, p_lw, 
         p_fw_e, p_fw_w, p_wma_total, w_wa_gf, w_wa_lf, p_coop_total)

# Compute daily demands for legacy sim code------------------------------------
demands.daily.df <- production.daily.df %>%
  mutate(date_time = as.Date(date_time),
         d_wa = p_wa - d_fw_c,
         d_fw_w = p_fw_w,
         d_fw_e = p_fw_e, 
         d_fw_c = d_fw_c,
         d_fw = p_fw + d_fw_c,
         d_wssc = p_wssc,
         d_lw = p_lw,
         d_pot_total = d_wa + d_fw_w + p_wssc_pot + d_lw)

# # Fill in df with full year of demands so that app won't break --------------
ncols <- length(demands.daily.df[1,])
data_first_date <- head(demands.daily.df$date_time, 1)
data_last_date <- tail(demands.daily.df$date_time, 1)
data_last <- tail(demands.daily.df[, 2:ncols], 1)
data_first <- head(demands.daily.df[, 2:ncols], 1)
# current_year <- year(data_last_date)
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


