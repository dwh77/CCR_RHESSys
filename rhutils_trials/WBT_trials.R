library(terra)
library(whitebox)

# look at Inputs
dem <- rast("./rhutils_trials/spatial_source/CCR_NED_1.tif")
crs(dem) #units appear to be meters
terra::expanse(dem, unit = "m")

pour_points <- vect("./rhutils_trials/spatial_source/PourPoints/pour_points23.shp")  # your ~20 tributary outlet points

plot(dem)
plot(pour_points, add = T)

library(terra)
library(whitebox)
library(tmap) #for nicer plots

wbt_version()
wbt_init()

# ---- File paths ----
# dem_path    <- "./rhutils_trials/spatial_source/CCR_NED_1.tif"
# ws_path     <- "./Data_sources/Watershed_rasters/ccr_watershed1.tif"
# nhd_path    <- "./rhutils_trials/spatial_source/NHDflowlines_CCR.tiff"
# pp_path     <- "./rhutils_trials/spatial_source/PourPoints/pour_points23.shp"
out_dir     <- "./rhutils_trials/spatial_source/WBT_Trials/UTM/"



# ---- Step 1: Set inputs data sets to UTM zone  ----
####set to UTM zone 17

# ## watershed
# ccr_ws <- rast(ws_path)
# crs(ccr_ws)
#
# ccr_ws_proj <- project(ccr_ws, "EPSG:26917")
# plot(ccr_ws_proj)
# crs(ccr_ws_proj)
#
# writeRaster(ccr_ws_proj, paste0(out_dir, "ccr_watershed_proj.tif"), overwrite = TRUE)
#
# ## dem
# dem    <- rast(dem_path)
# crs(dem)
#
# dem_proj <- project(dem, "EPSG:26917")
# plot(dem_proj)
# crs(dem_proj)
#
# writeRaster(dem_proj, paste0(out_dir, "dem_proj.tif"), overwrite = TRUE)
#
#
# ## flowlines
# nhd    <- rast(nhd_path)
# crs(nhd)
#
# nhd_proj <- project(nhd, "EPSG:26917")
# plot(nhd_proj)
# crs(nhd_proj)
#
# writeRaster(nhd_proj, paste0(out_dir, "nhd_proj.tif"), overwrite = TRUE)


# ---- Step 2: Mask DEM to watershed BEFORE any WBT processing ----

# 1. Project DEM
dem    <- rast("./rhutils_trials/spatial_source/CCR_NED_1.tif")
dem_proj <- project(dem, "EPSG:26917")
plot(dem_proj)

# 2. Project watershed raster
ccr_ws <- rast("./Data_sources/Watershed_rasters/ccr_watershed1.tif")
ccr_ws_aligned <- project(ccr_ws, dem_proj, method = "near")

# 3. Create mask
ws_mask <- ifel(ccr_ws_aligned > 0, 1, NA)

# 4. Mask DEM
dem_masked <- mask(dem_proj, ws_mask)

# 5. Convert watershed raster → polygon
ws_poly <- as.polygons(ccr_ws_aligned, dissolve = TRUE)

# 6. Crop DEM to polygon boundary
dem_final <- crop(dem_masked, ws_poly)

plot(dem_final)
# writeRaster(dem_final, paste0(out_dir, "dem_proj.tif"), overwrite = TRUE)


plot(ws_poly)

ws_raster <- rasterize(ws_poly, dem_final, field = 1, background = NA)
plot(ws_raster)
# writeRaster(ws_raster, paste0(out_dir, "watershed_proj.tif"), overwrite = TRUE)
ccr_ws <- rast(paste0(out_dir, "watershed_proj.tif"))
plot(ccr_ws)
sum(!is.na(values(ccr_ws)))

##make each cell unique ID
ccr_ws_id <- init(ccr_ws, "cell")
ccr_ws_id <- mask(ccr_ws_id, ccr_ws)
plot(ccr_ws_id)

## another patch idea
agg_factor   <- round(sqrt(global(ccr_ws, "notNA")[[1]] / 500))
ccr_agg      <- aggregate(ccr_ws, fact = agg_factor, fun = "mean", na.rm = TRUE)
ccr_agg_back <- resample(ccr_agg, ccr_ws, method = "near")
patch_map    <- patches(ccr_agg_back, directions = 4, zeroAsNA = TRUE)
patch_map    <- mask(patch_map, ccr_ws)
plot(patch_map)
global(patch_map, "max", na.rm = TRUE)

#### #burn in NHD network

dem <- rast(paste0(out_dir, "dem_proj.tif"))
streams <- rast(paste0(out_dir, "nhd_proj.tif"))   # or nhd_bin

streams_na <- ifel(streams == 0, NA, 1)

plot(dem)
plot(streams, add = T)

#nicer DEM plot
tm_shape(dem)+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE)+
  tm_scale_bar()

tm_shape(dem)+
  tm_raster(style = "cont", palette = "-Spectral", legend.show = T)+
  tm_shape(streams_na)+
  tm_raster(legend.show = TRUE, alpha = 1, style = "cat")+
  tm_scale_bar()
  # tm_shape(pp)+
  # tm_dots(col = "red")




burn_depth <- 20
dem_burned <- dem - (streams * burn_depth)

plot(dem_burned)

writeRaster(dem_burned, paste0(out_dir, "dem_burned.tif"), overwrite = TRUE)


# #get dem and waters
# dem_final <- rast(paste0(out_dir, "dem_proj.tif"))
#
# nhd    <- rast("./rhutils_trials/spatial_source/NHDflowlines_CCR.tiff")
#
# nhd_proj <- project(nhd, dem_final, method = "near")
# plot(nhd_proj)
#
# nhd_clip <- crop(nhd_proj, dem_final)
# nhd_clip <- mask(nhd_clip, dem_final)
# plot(nhd_clip)
#
# nhd_bin <- ifel(is.na(nhd_clip), 0, 1)
# plot(nhd_bin)
#
#
# writeRaster(nhd_bin, paste0(out_dir, "nhd_proj.tif"), overwrite = TRUE)
#
#
# wbt_fill_burn(
#   dem     = paste0(out_dir, "dem_proj.tif"),
#   streams = paste0(out_dir, "nhd_proj.tif"),
#   output  = paste0(out_dir, "dem_burned.tif")
# )












# ---- Step 3: Condition masked DEM ----
wbt_breach_depressions_least_cost(paste0(out_dir, "dem_burned.tif"),
                                  paste0(out_dir, "dem_breach_burned.tif"), dist = 1)
plot(rast(paste0(out_dir, "dem_breach_burned.tif")))


wbt_d8_pointer(paste0(out_dir, "dem_burned.tif"),
               paste0(out_dir, "fdir_burned.tif"))
plot(rast(paste0(out_dir, "fdir_burned.tif")))


wbt_d8_flow_accumulation(paste0(out_dir, "dem_burned.tif"),
                         paste0(out_dir, "facc_burned.tif"))
plot(rast(paste0(out_dir, "facc_burned.tif")))


# # ---- Step 3: Burn NHD streams into DEM to enforce known stream network ----
# #### fix NHD projection
# crs(rast(nhd_path))
# crs(rast(dem))
#
# nhd <- rast(nhd_path)
# plot(nhd)
# #align NHD to watershed
# nhd_reproj <- project(nhd, crs(dem))
# plot(nhd_reproj)
# #crop dem to match extend of watershed (this just does the square)
# nhd_reproj_cropped <- crop(nhd_reproj, ccr_ws)
# plot(nhd_reproj_cropped)
#
# #now need to align WS raster to DEM so we can then mask the DEM to the watershed area
# ccr_ws_aligned_nhd <- terra::resample(ccr_ws, nhd_reproj_cropped, method = "near")
# plot(ccr_ws_aligned)
# nhd_reproj_cropped_mask <- mask(nhd_reproj_cropped, ccr_ws_aligned_nhd)
# plot(nhd_reproj_cropped_mask)
#
# writeRaster(nhd_reproj_cropped_mask, paste0(out_dir, "nhd_masked.tif"), overwrite = TRUE)
#
#
# #### This carves the NHD network into the DEM so flow follows known channels
# # wbt_burn_streams_at_roads(dem    = paste0(out_dir, "dem_breach.tif"),
# #                           rivers = nhd_path,
# #                           output = paste0(out_dir, "dem_burned.tif"))
#
# wbt_fill_burn(
#   dem     = paste0(out_dir, "dem_breach.tif"),
#   streams = paste0(out_dir, "nhd_masked.tif"),
#   output  = paste0(out_dir, "dem_burned.tif")
# )
#
# crs(rast(paste0(out_dir, "dem_breach.tif")))
# crs(rast(paste0(out_dir, "nhd_masked.tif")))
#
#
# #change coords to utm to see if that works for WBT
# dem_proj <- terra::project(rast(paste0(out_dir, "dem_breach.tif")), "EPSG:26917")
# plot(dem_proj)
# writeRaster(dem_proj, paste0(out_dir, "dem_proj.tif"), overwrite = TRUE)
#
#
# # Recompute flow direction on the burned DEM
# wbt_d8_pointer(paste0(out_dir, "dem_burned.tif"),
#                paste0(out_dir, "fdir_burned.tif"))
# wbt_d8_flow_accumulation(paste0(out_dir, "dem_burned.tif"),
#                          paste0(out_dir, "facc_burned.tif"))






# ---- Step 4: Snap pour points to NHD stream raster ----
# Use NHD raster directly as stream network (already burned into DEM above)
nhd_streams_path <- paste0(out_dir, "nhd_proj.tif")

plot(rast(nhd_streams_path))

#fix projection
pp <- vect("./rhutils_trials/spatial_source/PourPoints/pour_points23.shp")
plot(pp)
pp

pp_proj <- project(pp, crs(dem_final))
plot(pp_proj)

# writeVector(pp_proj, paste0(out_dir, "pour_points23_utm.shp"), overwrite = TRUE)

#Snap points to NHD stream raster using Jenson method
wbt_jenson_snap_pour_points(pour_pts = paste0(out_dir, "pour_points23_utm.shp"),
                            streams  = nhd_streams_path,
                            output   = paste0(out_dir, "pour_snapped_jenson.shp"),
                            snap_dist = 50) # units = meters (UTM); increase if points still miss streams

plot(rast(nhd_streams_path))
plot(vect(paste0(out_dir, "pour_snapped_jenson.shp")), add = T)


# ---- Step 5: Delineate watersheds ----
wbt_watershed(d8_pntr  = paste0(out_dir, "fdir_burned.tif"),
              pour_pts = paste0(out_dir, "pour_snapped_jenson.shp"),
              output   = paste0(out_dir, "subbasins.tif"))

plot(rast(paste0(out_dir, "subbasins.tif")))


# ---- Step 6: Load, align, mask to watershed ----
subbasins        <- rast(paste0(out_dir, "subbasins.tif"))
subbasin_aligned <- resample(subbasins, ccr_ws, method = "near")
subbasins_masked <- mask(subbasin_aligned, ccr_ws)

plot(subbasins_masked)
global(subbasins_masked, "max", na.rm = TRUE)  # should match number of pour points


## try subbasins using NHD stream raster
wbt_subbasins(
  d8_pntr = paste0(out_dir, "fdir_burned.tif"),
  streams = nhd_streams_path,
  output  = paste0(out_dir, "subbasins_nhd.tif")
)

plot(rast(paste0(out_dir, "subbasins_nhd.tif")))










###################################### PREVIOUS TRIALS ########################################################


# 1. Condition DEM and compute flow direction
# wbt_breach_depressions()
wbt_breach_depressions_least_cost("./rhutils_trials/spatial_source/CCR_NED_1.tif",
                                  "./rhutils_trials/spatial_source/WBT_Trials/dem_breach.tif", dist = 5)

# dem2       <- rast("rhutils_trials/spatial_source/WBT_Trials/dem_breach.tif")
# plot(dem2)

wbt_d8_pointer("rhutils_trials/spatial_source/WBT_Trials/dem_breach.tif",
               "rhutils_trials/spatial_source/WBT_Trials/fdir.tif")

# pointer <- rast("rhutils_trials/spatial_source/WBT_Trials/fdir.tif")
# plot(pointer)

wbt_d8_flow_accumulation("rhutils_trials/spatial_source/WBT_Trials/dem_breach.tif",
                         "rhutils_trials/spatial_source/WBT_Trials/facc.tif")

# flowacc <- rast("rhutils_trials/spatial_source/WBT_Trials/facc.tif")
# plot(flowacc)

# 2. Snap pour points to nearest stream (avoids misses due to slight offsets)
#this takes like 4 min for 3 points but 40 min for 23 points
wbt_snap_pour_points("rhutils_trials/spatial_source/PourPoints/pour_points23.shp",
                     "rhutils_trials/spatial_source/WBT_Trials/facc.tif",
                     "rhutils_trials/spatial_source/WBT_Trials/pour_snapped23.shp",
                     snap_dist = 50)


## make streams
wbt_extract_streams(flow_accum = "rhutils_trials/spatial_source/WBT_Trials/facc.tif",
                    output = "rhutils_trials/spatial_source/WBT_Trials/raster_streams.tif",
                    threshold = 6000)

# wbt_streams <- rast("rhutils_trials/spatial_source/WBT_Trials/raster_streams.tif")
# plot(wbt_streams)

########## I THINK THIS IS WORKING OK,
## BUT SINCE I ONLY HAVE THREE POUR POINT SPOTS ITS MAKING THE RESERVOIR INTO 4 BOXES, TRY ADDING SOME MORE POUR POINTS


# 3. Delineate one subbasin per pour point
# wbt_unnested_watersheds("rhutils_trials/spatial_source/WBT_Trials/fdir.tif",
#                         "rhutils_trials/spatial_source/WBT_Trials/pour_snapped.shp",
#                         "rhutils_trials/spatial_source/WBT_Trials/subbasins.tif")

# Delineate all subbasins at once from multiple pour points
wbt_watershed(d8_pntr = "rhutils_trials/spatial_source/WBT_Trials/fdir.tif",
              pour_pts = "rhutils_trials/spatial_source/WBT_Trials/pour_snapped23.shp",
              output =  "rhutils_trials/spatial_source/WBT_Trials/ccr_watersheds2.tif")


wbt_subbasins(
  d8_pntr = "rhutils_trials/spatial_source/WBT_Trials/fdir.tif",
  streams = "rhutils_trials/spatial_source/NHDflowlines_CCR.tiff",
  # streams = "rhutils_trials/spatial_source/WBT_Trials/raster_streams.tif",
  output  = "rhutils_trials/spatial_source/WBT_Trials/subbasins_nhdmap.tif"
)

# 4. Load result and mask to watershed
subbasins <- rast("rhutils_trials/spatial_source/WBT_Trials/subbasins.tif")
plot(subbasins)
plot(pour_points, add = T)


ccr_ws <- rast("./Data_sources/Watershed_rasters/ccr_watershed1.tif")


subbasin_aligned <- resample(subbasins, ccr_ws, method = "near")
plot(subbasin_aligned)

subbasins1 <- crop(subbasin_aligned, ccr_ws)
subbasins1 <- mask(subbasins1, ccr_ws)
plot(subbasins1)


streams <- rast("rhutils_trials/spatial_source/WBT_Trials/raster_streams.tif")
plot(streams)


plot(subbasins)
plot(streams, add = T, col = "black")







