#### Snap DEM to CCR bathy
##DWH
## 25 March 2026



##packages
library(terra)
library(tmap) #for nicer plots

# library(whitebox)
# wbt_version()
# wbt_init()

#### Get DEM to watershed boundary ----

#### DEM

dem <- rast("./rhutils_trials/spatial_source/CCR_NED_1.tif")
plot(dem)
crs(dem) #units appear to be meters
# terra::expanse(dem, unit = "m")

# pour_points <- vect("./rhutils_trials/spatial_source/PourPoints/pour_points23.shp")  # your ~20 tributary outlet points
# plot(dem)
# plot(pour_points, add = T)

#### Watershed boundary for croping
ccr_ws <- rast("./Data_sources/Watershed_rasters/ccr_watershed1.tif")
ccr_ws_aligned <- project(ccr_ws, dem, method = "near")
plot(ccr_ws_aligned)

# 3. Create mask
ws_mask <- ifel(ccr_ws_aligned > 0, 1, NA)

# 4. Mask DEM
dem_masked <- mask(dem, ws_mask)

# 5. Convert watershed raster → polygon
ws_poly <- as.polygons(ccr_ws_aligned, dissolve = TRUE)

# 6. Crop DEM to polygon boundary
dem_final <- crop(dem_masked, ws_poly)

plot(dem_final)
terra::expanse(dem_final, unit = "km") #watershed area in km^2
crs(dem_final)



#### Read in bathy and match to DEM size and coordinates ----

bathy <- rast("./Data_sources/CCR_bathy/CCR_bathy_raster.tif")
plot(bathy)

#make 0's NAs
bathy <- ifel(bathy == 0, NA, bathy)
plot(bathy)
crs(bathy)

##match projection to dem
bathy_proj <- project(bathy, crs(dem_final))
plot(bathy_proj)
crs(bathy_proj)

##match sample size
bathy_proj_resamp <- resample(bathy_proj, dem_final, method = "bilinear")
plot(bathy_proj_resamp)



#### Append bathy to DEM ----
#note that DEM is in meters elevation and bathy is already in elevation so no need to covert raster units
summary(dem_final)
summary(bathy_proj_resamp)


# Option B: DEM has values (e.g. water surface) over the reservoir
# Use the bathy extent as a mask to blank out the reservoir in the DEM first
reservoir_mask <- !is.na(bathy_proj_resamp)
plot(reservoir_mask)

dem_noRes <- mask(dem_final, reservoir_mask, maskvalue = TRUE)
plot(dem_noRes)

combined <- merge(dem_noRes, bathy_proj_resamp)
plot(combined)

#check this really worked
plot(dem_final) #still hard to tell, try diff color pallete

tm_shape(dem_final)+
  tm_raster(style = "cont", palette = "Spectral", legend.show = T)+
  tm_scale_bar()+ tm_title("DEM only")

tm_shape(combined)+
  tm_raster(style = "cont", palette = "Spectral", legend.show = T)+
  tm_scale_bar()+ tm_title("DEM plus bathy")

#it worked! just really have to look at center of reservoir


#write tif
writeRaster(combined, "./rhutils_trials/spatial_source/DEM_bathy_combined.tif", overwrite = TRUE)


###### WBT trials on combined DEM and bathy; make this its own script ------------------------------------------

library(terra)
library(tmap) #for nicer plots
library(whitebox)
wbt_version()
wbt_init()

#set directory
out_dir     <- "./rhutils_trials/spatial_source/WBT_combined_trials/"




### combined tif
dem <- rast("./rhutils_trials/spatial_source/DEM_bathy_combined.tif")
plot(dem)
crs(dem)

##project dem to UTM 17N
dem_proj <- project(dem, "EPSG:26917")

writeRaster(dem_proj, paste0(out_dir, "dem_proj.tif"), overwrite = TRUE)
plot(rast( paste0(out_dir, "dem_proj.tif")))


#### Fill in gaps and set flow directions

wbt_breach_depressions_least_cost(
  dem    = paste0(out_dir, "dem_proj.tif"),
  output = paste0(out_dir, "dem_conditioned.tif"),
  dist   = 10,   # large enough to span the reservoir basin
  fill   = F    # breach + fill in one step
)

b <- rast(paste0(out_dir, "dem_conditioned.tif"))
summary(b)   # check values are still sensible
plot(b, main = "Conditioned DEM")

zz <- ifel(b == Inf, 1, NA)
plot(zz)

zz <- ifel(b == Inf, NA, b)
plot(zz)
summary(zz)

writeRaster(zz, paste0(out_dir, "dem_conditioned2.tif"), overwrite = TRUE)


wbt_fill_depressions_wang_and_liu(
  dem = paste0(out_dir, "dem_conditioned2.tif"),
  output = paste0(out_dir, "dem_proj_filled_breached.tif")
)

plot(rast( paste0(out_dir, "dem_proj_filled_breached.tif")))

# wbt_breach_depressions_least_cost(
#   dem = paste0(out_dir, "dem_proj.tif"),
#   output = paste0(out_dir, "dem_proj_breached.tif"),
#   dist = 10,
#   fill = F)
#
# plot(rast( paste0(out_dir, "dem_proj_breached.tif")))
#
#
#
# wbt_fill_depressions_wang_and_liu(
#   dem = paste0(out_dir, "dem_proj_breached.tif"),
#   output = paste0(out_dir, "dem_proj_filled_breached.tif")
# )
#
# plot(rast( paste0(out_dir, "dem_proj_filled_breached.tif")))































































#### Flow acc and pointer, skipping breach for now
wbt_d8_flow_accumulation(input = paste0(out_dir, "dem_proj.tif"),
                         output = paste0(out_dir, "flowacc.tif"))

plot(rast( paste0(out_dir, "flowacc.tif")))


wbt_d8_pointer(dem = paste0(out_dir, "dem_proj.tif"),
               output = paste0(out_dir, "d8pointer.tif"))

plot(rast( paste0(out_dir, "d8pointer.tif")))


#### extract streams

wbt_extract_streams(flow_accum = paste0(out_dir, "flowacc.tif"),
                    output = paste0(out_dir, "raster_streams.tif"),
                    threshold = 20)

plot(rast( paste0(out_dir, "raster_streams.tif")))




