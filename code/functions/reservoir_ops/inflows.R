# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R
# It creates reservoir inflows from current daily flow time series
# *****************************************************************************
# INPUTS
# *****************************************************************************
# flows.daily.mgd.df - table with daily flows created by import_data.R
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# inflows_df
# *****************************************************************************
#
# Read these drainage areas for now, but should add to parameters_physical.R
sen_da <- 101
bennett_da <- 62.8

# Use na.approx from zoo package to replace NA's with interpolated values

flows.daily.mgd.df$bennett <- na.approx(flows.daily.mgd.df)

x <- flows.daily.mgd.df$date_time[100]
if(x < as.Date("2019-04-11")) y <- 88

inflow_df <- flows.daily.mgd.df %>%
  dplyr::mutate(date = date_time,
                inflow_sen = bennett*sen_da/bennett_da,
                inflow_occ = case_when(
                  date < as.Date("2020-11-01") ~ 1,
                  date >= as.Date("2020-11-01") & date < as.Date("2021-01-15") ~ 2,
                  date >= as.Date("2021-01-15") ~3,
                  TRUE ~ -9999)
                ) %>%
  dplyr::select(date_time, inflow_sen, inflow_occ)
