
# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, sf)


# Load --------------------------------------------------------------------

dme_use <- readRDS(here(
  "data_for_upload",
  "effect_mod_data",
  "dme_use_by_county.RDS"
))

pov <- 
  readRDS(here('data_for_upload', "effect_mod_data", "pov_measures.RDS"))

counties <- readRDS(here('local_data', 'cotus_county_shp_w_fips.RDS'))

counties <- counties %>% left_join(dme_use)

p1 <- counties %>% ggplot() + geom_sf(aes(fill = dme_quartile))
ggsave(p1, here('figures', 'figures_output', "dme_use"))

