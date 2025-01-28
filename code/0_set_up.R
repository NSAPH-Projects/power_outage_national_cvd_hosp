# This script sets up FASSE for data cleaning for the national power outages and
# cvd hospitalizations project

# Heather
# Created: September 18th, 2024
# Last updated: September 24th, 2024

# libraries
pacman::p_load(here)

# set proxies for GitHub access
Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")

# stop R from being a lil bih about small and large numbers
options(scipen = 999)

# add token so we can use GitHub
gitcreds::gitcreds_set()
gitcreds::gitcreds_get()

# create symlinks for 2018 denom and hospitalization files
# denom file
target_file_denom <-
  paste0("/n/dominici_nsaph_l3/Lab/projects/analytic/",
         "mbsf_medpar_denom/mbsf_medpar_denom_2018.parquet")
link_name_denom <-
  here("data", "mbsf_medpar_denom_2018.parquet")

# verify the symlinks
if (file.exists(link_name_denom)) {
  cat("Symbolic link created successfully.\n")
} else {
  # Create the symbolic link
  file.symlink(from = target_file_denom, to = link_name_denom)

  if (file.exists(link_name_denom)) {
    cat("Symbolic link created successfully.\n")
  }
  else {
    cat("Failed to create symbolic link.\n")
  }
}

# hospitalization file
target_file_hosp <-
  paste0(
    "/n/dominici_nsaph_l3/Lab/projects/analytic/",
    "mbsf_medpar_denom/medpar_hospitalizations_2018.parquet"
  )
link_name_hosp <-
  here("data", "medpar_hospitalizations_2018.parquet")

# verify the symlinks
if (file.exists(link_name_hosp)) {
  cat("Symbolic link created successfully.\n")
} else {
  # Create the symbolic link
  file.symlink(from = target_file_hosp, to = link_name_hosp)

  if (file.exists(link_name_hosp)) {
    cat("Symbolic link created successfully.\n")
  }
  else {
    cat("Failed to create symbolic link.\n")
  }
}


# zip to county xwalk
target_file_xwalk <-
  paste0(
    "/n/dominici_nsaph_l3/Lab/exposure/zip2county_master_xwalk/",
    "zip2county_master_xwalk_2010_2023_tot_ratio_one2one.csv"
  )
link_name_xwalk <-
  here("data", "zip2countyxwalk.csv")

# verify the symlinks
if (file.exists(link_name_xwalk)) {
  cat("Symbolic link created successfully.\n")
} else {
  # Create the symbolic link
  file.symlink(from = target_file_xwalk, to = link_name_xwalk)
  
  if (file.exists(link_name_xwalk)) {
    cat("Symbolic link created successfully.\n")
  }
  else {
    cat("Failed to create symbolic link.\n")
  }
}
