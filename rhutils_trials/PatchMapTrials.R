#### DWH trying plotting GRASS maps in R


library(sf)
library(terra)

getwd()
### Read in watershed boundary from GRASS,
#will use this to change projections and size from soil and veg maps
watershed <- rast("HPB_files_clean/spatial_data/hpb_watershed.tiff")
crs(watershed)
plot(watershed)

### SO HERES WHAT I WANT
#1- maps of soil and veg that match the region and projection of waterhsed
#2 patch map that makes the groups shown in veg, then functionally patches move like veg areas


#### Format soil data ----
soils_map <-  rast("Polaris_SoilData/Processed/ccr_soils_texture.tif")
plot(soils_map)
crs(soils_map)

soils_reproj <- terra::project(soils_map, crs(watershed), method = "near") #match projection of watershed and near keeps discrete values
crs(soils_reproj)
plot(soils_reproj)

compareGeom(watershed, soils_reproj)


##THIS WORKS!!!!!
soil_aligned <- resample(soils_reproj, watershed, method="near")
soil_crop <- crop(soil_aligned, watershed)
plot(soil_crop)
ext(soil_crop) == ext(watershed)
soil_ws <- mask(soil_crop, watershed)
plot(soil_ws)

getwd()
writeRaster(soil_ws, "HPB_files_NEW/spatial_data/hpb_soil_ID.tiff", overwrite=T)


#### Format veg data ----
veg_map <-  rast("rhutils_trials/NLCD/NLCD_veg_cover.tif")
plot(veg_map)
crs(veg_map)

veg_reproj <- terra::project(veg_map, crs(watershed), method = "near") #match projection of watershed and near keeps discrete values
crs(veg_reproj)
plot(veg_reproj)

veg_aligned <- resample(veg_reproj, watershed, method="near")
veg_crop <- crop(veg_aligned, watershed)
ext(veg_crop) == ext(watershed)
veg_ws <- mask(veg_crop, watershed)
plot(veg_ws)

getwd()
writeRaster(veg_ws, "HPB_files_NEW/spatial_data/hpb_veg_ID.tiff", overwrite=T)


#
# #### try to use veg to reproject patches
# veg <- veg_reproj_crop
#
# #trim vveg_reproj_crop#trim veg to watershed
# compareGeom(veg, watershed)
#
# # 1. Resample veg to EXACT watershed grid
# veg_rs <- resample(veg, watershed, method = "near")
#
# # 2. Mask using watershed
# veg_ws <- mask(veg_rs, watershed)
#
# # 3. (Optional) remove outer NA rows/cols if needed
# veg_ws <- trim(veg_ws)
#
# plot(veg_ws)



###### PATCH EXPLORATION ----------------
##Pull in soil ksat data
ksat_3738_8081 <- rast("Polaris_SoilData/POLARISOut/mean/ksat/0_5/lat3738_lon-81-80.tif")
ksat_3738_7980 <- rast("Polaris_SoilData/POLARISOut/mean/ksat/0_5/lat3738_lon-80-79.tif")

ksat_merge <- merge(ksat_3738_7980, ksat_3738_8081)
plot(ksat_merge)
#check projection
st_crs(ksat_merge)

ksat_merge_crop <- terra::mask(project(ksat_merge, watershed), watershed)
plot(ksat_merge_crop)

# 1. Resample veg to EXACT watershed grid
ksat_rs <- resample(ksat_merge_crop, watershed, method = "near")

summary(ksat_rs)

## fresh chat instructions

# Use 5 classes based on quantiles, NA ignored
breaks <- quantile(values(ksat_rs), probs = seq(0,1,by=0.2), na.rm=TRUE)

# Slight tweak: include min and max explicitly
breaks[1] <- min(values(ksat_rs), na.rm=TRUE)
breaks[length(breaks)] <- max(values(ksat_rs), na.rm=TRUE)

# Create discrete raster (classes 1-5)
ksat_class <- classify(ksat_rs, cbind(breaks[-length(breaks)], breaks[-1], 1:5))

# Check unique values
unique(values(ksat_class))

plot(ksat_class)

ksat_patch <- patches(ksat_class, directions = 4)

# Optional: mask to watershed if needed
ksat_patch <- mask(ksat_patch, watershed)

# Optional: inspect
freq(ksat_patch)
plot(ksat_patch)




#

























############### EVERYTHING BELOW IS A MESS, but has some usefull code tidbits

#data sets from GRASS inputs
getwd()
dem <- rast("HPB_files_clean/spatial_data/hpb_dem.tiff")
#get total cells in map
z <- ncell(dem)
print(z)

watershed <- rast("HPB_files_clean/spatial_data/hpb_watershed.tiff")
crs(watershed)

streams <- rast("HPB_files_clean/spatial_data/hpb_streams1.tiff")

plot(dem)
plot(streams, add = T)

basins <- rast("HPB_files_clean/spatial_data/hpb_basins_5000.tiff")
plot(basins)
unique(basins)


#look at patches
patch_basins <- rast("HPB_files_clean/spatial_data/hpb_patches_basin1000_fill.tiff")
plot(patch_basins)
unique(patch_basins)

patch_clump <- rast("HPB_files_clean/spatial_data/hpb_patchcl_fill.tiff")
plot(patch_clump)
unique(patch_clump)


#check projection
crs(dem)
crs(streams)

### maps from soil and veg ------
#make watershed projection match from croping
watershed_wgs84 <- terra::project(watershed, "EPSG:4326", method = "near") #need method near to not get decimals; normal method is bilinear
watershed_proj <- terra::project(watershed, crs(soils_map)) #could try this
crs(watershed_proj)
plot(watershed_proj)



#soils
soils_map <-  rast("Polaris_SoilData/Processed/ccr_soils_texture.tif")
plot(soils_map)
crs(soils_map)
#get total cells in map
z <- ncell(soils_map)
print(z)

soils_unique <- soils_map
values(soils_unique) <- seq_len(ncell(soils_map))
plot(soils_unique)

soils_hpb <- crop(soils_map, watershed_wgs84)
plot(soils_hpb)
# plot(watershed_wgs84)

#
# ws_ext <- ext(watershed_wgs84)
#
# soils_crop <- terra::crop(soils_map, ws_ext)
# soils_mask <- terra::mask(soils_crop, ws_ext)
#
# plot(soils_mask)
#
#
# soils_hpb <- soils_map |>
#   terra::crop(watershed_proj) |>
#   terra::mask(watershed_proj)
#
#
# soils_hpb <- terra::crop(project(soils_map, watershed_wgs84), watershed_wgs84)
# soils_hpb <- terra::mask(soils_map, watershed_wgs84)
# plot(soils_hpb)


#veg RHESSys groups from NLCD
veg_map <-  rast("rhutils_trials/NLCD/NLCD_veg_cover.tif")
plot(veg_map)
crs(veg_map)
#get total cells in map
z <- ncell(veg_map)
print(z)

veg_hpb <- crop(veg_map, watershed_wgs84)
plot(veg_hpb)
plot(watershed_wgs84)






#### ----------
#trying soil patch from rhutils 1.3
mask_map = dem
mask_vect = as.polygons(mask_map)
# writeVector(mask_vect, "preprocessing/spatial_source/basin_vect90m.shp")

p_map = dem
names(p_map) = "patches"
# resample to get unique patches 1 per cell
values(p_map)[!is.nan(values(p_map))] = seq_along(values(p_map)[!is.nan(values(p_map))])
# writeRaster(p_map, "preprocessing/whitebox/patches.tif", overwrite=T)
#writeRaster(p_map, "preprocessing/spatial180m/patches.tif", overwrite=T)

plot(p_map)


## bring in soils data
soils_map = rast("Polaris_SoilData/Processed/ccr_soils_texture.tif")
plot(soils_map)

crs(soils_map)

#match projection
soils_map_nad83 <- terra::project(soils_map, "EPSG:4269", method = "near") #need method near to not get decimals; normal method is bilinear
plot(soils_map_nad83)
crs(soils_map_nad83)













