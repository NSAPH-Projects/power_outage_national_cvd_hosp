

pacman::p_load(ncdf4, data.table, tidyverse, here, sf, raster)


county_shp <- readRDS(here('local_data', "cotus_county_shp_w_fips.RDS"))

vs <- nc_open(here("local_data", "gridMET", 'vs_2018.nc'))
precip <- nc_open(here("local_data", "gridMET", "pr_2018.nc"))
tmax <- nc_open(here("local_data", "gridMET", "tmmx_2018.nc"))

vs <- raster::brick(here("local_data", "gridMET", 'vs_2018.nc'))
precip <- raster::brick(here("local_data", "gridMET", "pr_2018.nc"))

raster_list <- lapply(1:nlayers(raster_brick), function(i) raster(raster_brick, layer = i))



st_crs(county_shp)

county_shp <- st_transform(county_shp, crs(raster_brick[[1]]))

mean_values <- extract(vs[[1]], county_shp, fun = mean, na.rm = TRUE, df = TRUE)

i <- cbind(county_shp, mean_values)


# Intersect the NetCDF data with the shapefile
intersection <- st_intersects(nc_data, shapefile)

raster_brick <- raster::brick(here("local_data", "gridMET", 'vs_2018.nc'))

raster_list <- lapply(1:nlayers(raster_brick), function(i) raster(raster_brick, layer = i))
