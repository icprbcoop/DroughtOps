# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# This script is run by global.R, so df's are globally accessible
# Compiles archived forecasts so they can be analyzed
#   - maybe will move these scripts elsewhere at some point
# *****************************************************************************
# INPUTS
# *****************************************************************************
# data/forecasts/lffs_bfc_daily/flows_mgd_2021-05-05.csv, ...
# *****************************************************************************
# OUTPUTS
# *****************************************************************************
# lffs_bfc_daily_fc_mgd_df
# *****************************************************************************

print("starting process_fcs")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Compile LFFS baseflow-corrected (bfc) forecasts (fcs)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# get list of lffs bfc fc files
data_dir <- "data/forecasts/lffs_bfc_daily/"
 list_lffs_bfc_daily <- fs::dir_ls(data_dir, regexp = "\\.csv$") # list csv files only
 
# read all of the files into one long df
fcs_lffs_bfc_daily_df <- list_lffs_bfc_daily %>% 
   purrr::map_dfr(read_csv) %>%
  drop_na() %>%
  dplyr::filter(length_fc > 0)



