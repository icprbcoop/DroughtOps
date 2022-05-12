# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It processes the real-time North Branch reservoir storage data
# *****************************************************************************
# INPUTS
# *****************************************************************************
# storage_nbr_df - table with rt jrr and savage storages, BG
#   - jrr_total
#   - jrr_ws
#   - savage
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# storage_nbr_daily_df - jrr_total, jrr_ws, & savage storage at 7:00 AM, BG
# *****************************************************************************

# Grab value at 7 AM for daily data df
#   - rethink later
storage_nbr_daily_df <- storage_nbr_df %>%
  filter(hour == 7 & minute == 0) %>%
  select(date, jrr_total, jrr_ws, savage) %>%
  rename(date_time = date) %>%
  arrange(date_time)
