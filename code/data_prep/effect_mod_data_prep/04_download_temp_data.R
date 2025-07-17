# This script downloads temp data to calculate anom hot and cold days

# Author: Heather
# Date: Nov 6th, 2024

pacman::p_load(here, data.table, tidyverse)

# Download temp data ------------------------------------------------------

# function to download files
download_files <- function(urls, save_dir) {
  for (url in urls) {
    file_name <- basename(url)
    download.file(url, file.path(save_dir, file_name), mode = "wb")
  }
}

# make list of target file urls 
# this seems to be county-level data, so ok, that's important to know
file_urls <- paste0(
  "https://github.com/rmp15/PRISM-grids-into-FIPS-ZIP-censustract-USA/raw/", 
  "main/output/fips/tmean/weighted_area_raster_fips_tmean_daily_",
  seq(from = 1981, to = 2020, by = 1),
  ".rds"
)

# id directory to save the downloaded files
save_dir <- here("local_data", "effect_mod_data", "temp_data")

# download the files
download_files(file_urls, save_dir)
