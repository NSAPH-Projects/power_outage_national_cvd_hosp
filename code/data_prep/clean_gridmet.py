import netCDF4
import rasterio
import geopandas as gpd
from rasterstats import zonal_stats
import os

# Load the shapefile
county_shp = gpd.read_file(os.path.join("local_data", "cotus_county_shp_w_fips.RDS"))

# Load the NetCDF file
vs = rasterio.open(os.path.join("local_data", "gridMET", "vs_2018.nc"))

# Ensure the CRS matches
county_shp = county_shp.to_crs(vs.crs)

# Extract the mean values of the raster within each shape
mean_values = zonal_stats(county_shp, vs.read(1), stats="mean", nodata=vs.nodata)

# Convert the results to a DataFrame
mean_values_df = gpd.GeoDataFrame(mean_values)

# Combine the mean values with the shapefile data
result = county_shp.join(mean_values_df)

# Print the result
print(result)
