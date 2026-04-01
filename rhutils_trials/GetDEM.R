#### GET DEM and NHD flowlines
## maps need to make maps for CCR and HPB basins

#### packages
library(tidyverse)
library(terra)
library(sf)
library(FedData) #getting DEM
library(nhdplusTools) #getting flowlines from NHD
library(mapview) #make interactive map
library(mapedit) #add points on map to get pour point locations


#### Read in basin rasters (These were made into raster in GRASS or ArcGIS from shapefiles)
ccr_ws <- rast("./Data_sources/Watershed_rasters/ccr_watershed1.tif")
plot(ccr_ws)

# hpb_ws <- rast("./Data_sources/Watershed_rasters/hpb_watershed.tiff")
# plot(hpb_ws)


####Read in basins shapefiles and rasterize (these shapefiles are original download from streamstats)
#if converting this a more reproducible/scalable workflow; maybe read in DEM below first then can use that as the reference raster
#read in and plot shape file
# ccr_shp <- vect("Data_sources/StreamStats_shapefiles/CCR_watershed_shpfile_StreamStats/ccr_ws/layers/globalwatershed.shp")
# plot(ccr_shp)
#
# #rasterize
# ccr_shp_rast <- rasterize(ccr_shp, ccr_ws, field = 1)
# plot(ccr_shp_rast)


#### get utm zone number
#for HPB
outlet_coords_geo = c(37.3646, -79.9733) # this may be very wrong

#utm zone
ZoneNumber = floor((outlet_coords_geo[2] + 180)/6) + 1



#### set basin boundary Make box for watershed area ----
## For HPB square
# basin_bounds = c( 37.3800, 37.3540, -79.9715, -80.0071) # N S E W
## for CCR square
basin_bounds = c( 37.4600, 37.3300, -79.8900, -80.1000) # N S E W


# basin_ext = rast(crs = "EPSG:32611")
basin_ext = rast(crs = "EPSG:4326")
ext(basin_ext) = basin_bounds[c(4,3,2,1)]
values(basin_ext) = 1

plot(basin_ext)

# writeRaster(x = basin_ext, filename = "rhutils_trials/spatial_source/basin_ext.tif",overwrite =T)



### Get DEM ----
#this both reads in a DEM locally and wrties a tif file to the extraction.dir
# NED <- FedData::get_ned(template = basin_ext,
#                label="CCR",
#                extraction.dir = paste0("rhutils_trials/spatial_source/"),
#                force.redo = T)
#
# plot(NED)

#once aquiared can read in locally
NED <- rast("./rhutils_trials/spatial_source/CCR_NED_1.tif")

plot(NED)


#compare to other DEM binded in arc
# arcdem <- rast("./Data_sources/USGS_DEM/usgs_dem_ccrWSbox_UTM.tif")
#
# plot(arcdem)


#### Get NHD flowlines ----

#get basin in right format
basin_sf <- st_as_sf(as.polygons(ext(basin_ext), crs = crs(basin_ext)))


## This was trial with basic get_nhdplus but it misses all the intermittent stream classes
# # Download NHDPlus flowlines clipped to that area
# flowlines <- nhdplusTools::get_nhdplus(AOI = basin_sf, realization = "flowline")
#
# plot(flowlines$streamorde)
#
# # Rasterize flowlines to match your reference raster exactly
# flowlines_reproj <- st_transform(flowlines, crs(basin_sf))
# plot(flowlines_reproj)
#
# streams_rast     <- rasterize(vect(flowlines_reproj), basin_ext, field = 1)
#
# plot(streams_rast)
#
# plot(basin_ext)
# plot(NED)
# plot(streams_rast, add = T, col = "orange")
#
# ## looks like I'm not getting all the ephemeral streams... see whats up
# table(flowlines$ftype)
# unique(flowlines$ftype)
# unique(flowlines$fcode)


## Get expanded nhd plus data that includes intermittent streams

# #Step 1: download once (only need to do this once)
# nhdplusTools::download_nhdplushr(
#   nhd_dir = "./Data_sources/NHD_flowlines_download/",
#   hu_list = "0301"
# )

# Step 2: then read from local files
flowlines_hr <- nhdplusTools::get_nhdplushr(
  hr_dir = "./Data_sources/NHD_flowlines_download/03",
  layers = "NHDFlowline"
)$NHDFlowline

#check projections
crs(flowlines_hr)
# crs(basin_sf)

# ##Update this with DEM thats in UTM
# dem_final <- rast("./rhutils_trials/spatial_source/WBT_Trials/UTM/dem_proj.tif")
# plot(dem_final)
#
# basin_sf <- st_as_sf(as.polygons(ext(dem_final), crs = crs(dem_final)))
# plot(basin_sf)

#match projection for flowlines to basin
flowlines_hr_reproj <- st_transform(flowlines_hr, crs(dem_final))

#filter flowlines to CCR box
flowlines_fin <- flowlines_hr_reproj |>  sf::st_filter(dem_final)

#plot flowlines and check class stypes
plot(flowlines_fin)

table(flowlines_fin$FTYPE)
unique(flowlines_fin$FTYPE)
unique(flowlines_fin$FCODE)

#turn flowlines into a rater
streams_rast <- rasterize(vect(flowlines_fin), basin_ext, field = 1)

#overlay flowliens on basin and DEM
plot(basin_ext)
plot(NED)
plot(streams_rast, add = T, col = "orange")
plot(streams_rast)

#save flowlines raster
#writeRaster(streams_rast, "rhutils_trials/spatial_source/NHDflowlines_CCR.tiff", overwrite=T)



## look at intermittent v perennial
# Reclassify FType to readable labels
# https://files.hawaii.gov/dbedt/op/gis/data/NHD%20Complete%20FCode%20Attribute%20Value%20List.pdf
flowlines_fin$stream_type <- dplyr::case_when(
  flowlines_fin$FCODE == 46006 ~ "Perennial",
  flowlines_fin$FCODE == 46003 ~ "Intermittent",
  flowlines_fin$FCODE == 55800  ~ "Artifical Path",
  flowlines_fin$FCODE == 55800  ~ "Connector",
  TRUE ~ "Other"
)

# Plot
ggplot(flowlines_fin) +
  geom_sf(aes(color = stream_type)) +
  scale_color_manual(values = c("Ephemeral" = "tan",
                                "Intermittent" = "steelblue",
                                "Perennial" = "navy",
                                "Other" = "grey50"))




#### make interactive plot to pull out pour points ----
mapview(NED, alpha = 0.6) + mapview(streams_rast, col.regions = "blue")

# Click your pour points on the map, then click "Done"
pour_points <- mapedit::drawFeatures(
  mapview(NED, alpha = 0.6) + mapview(streams_rast, col.regions = "blue")
)

# Extract the coordinates as a dataframe
coords <- st_coordinates(pour_points)
print(coords)

# Optionally save as a shapefile for use in the wbt snapping step later
# st_write(pour_points, "./rhutils_trials/spatial_source/PourPoints/pour_points23.shp", delete_dsn = TRUE)


####




