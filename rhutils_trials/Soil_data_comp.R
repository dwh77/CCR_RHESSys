## get soils from rhutils 1.1

output_dir = "Polaris_SoilData/Processed"


## set a mask for general ccr watershed area
## For ~ HPB
# basin_bounds = c( 37.3800, 37.3540, -79.9715, -80.0071) # N S E W
## for CCR square
basin_bounds = c( 37.4600, 37.3300, -79.8900, -80.1000) # N S E W

#### Make basin
basin_ext = rast(crs = "EPSG:4326") #WGS to match soil below
writeRaster(basin_ext, file.path(output_dir, "ccr_mask.tif"),overwrite=T)
ext(basin_ext) = basin_bounds[c(4,3,2,1)]
values(basin_ext) = 1



# ------------------------------ GET SOILS MAPS ------------------------------
# devtools::install_github("lhmrosso/XPolaris")
library("XPolaris")

#This is a box that covers all of the CCR watershed plus more
bbox_df = data.frame(ID = c("a","b","c","d"),
                     lat = c(37.0, 37.0, 37.9, 37.9),
                     long = c(-79.1, -81.0, -79.1, -81.0)
                     # lat = ext(basin_ext)[c(2,2,4,4)],
                     # long = ext(basin_ext)[c(1,3,1,3)]
)

xplot(locations = bbox_df)

getwd()

df_ximages <- ximages(locations = bbox_df,
                      statistics = c('mean'),
                      variables = c('clay','sand','ksat'),
                      layersdepths = c('0_5'),
                      localPath = "Polaris_SoilData/")

# xsoil(ximages_output = df_ximages, localPath = "preprocessing/spatial_source/")
# clay = rast('./download/POLARISOut/mean/clay/0_5/lat3839_lon-121-120.tif')
# sand = rast('./download/POLARISOut/mean/sand/0_5/lat3839_lon-121-120.tif')
# ksat =  rast('./download/POLARISOut/mean/ksat/0_5/lat3839_lon-121-120.tif')
# clay.crop = crop(clay, dem)
# sand.crop = crop(sand, dem)

#### Take these generated files; read them back in, merge and trim to region of interest
##Clay
clay_3738_8081 <- rast("Polaris_SoilData/POLARISOut/mean/clay/0_5/lat3738_lon-81-80.tif")
clay_3738_7980 <- rast("Polaris_SoilData/POLARISOut/mean/clay/0_5/lat3738_lon-80-79.tif")
plot(clay_3738_8081)

clay_merge <- merge(clay_3738_7980, clay_3738_8081)
plot(clay_merge)
#check projection
st_crs(clay_merge)

clay_merge_mask <- terra::crop(project(clay_merge, basin_ext), basin_ext)
clay_merge_mask <- terra::mask(clay_merge_mask, basin_ext)
plot(clay_merge_mask)
writeRaster(clay_merge_mask, file.path(output_dir, "ccr_clay_map.tif"),overwrite=T)


##Sand
sand_3738_8081 <- rast("Polaris_SoilData/POLARISOut/mean/sand/0_5/lat3738_lon-81-80.tif")
sand_3738_7980 <- rast("Polaris_SoilData/POLARISOut/mean/sand/0_5/lat3738_lon-80-79.tif")

sand_merge <- merge(sand_3738_7980, sand_3738_8081)
plot(sand_merge)
#check projection
st_crs(sand_merge)

sand_merge_mask <- terra::crop(project(sand_merge, basin_ext), basin_ext)
sand_merge_mask <- terra::mask(sand_merge_mask, basin_ext)
plot(sand_merge_mask)
writeRaster(sand_merge_mask, file.path(output_dir, "ccr_sand_map.tif"),overwrite=T)


##make soil texture
library(rhutils)


soil_texture = polaris2texture(
  basin = "Polaris_SoilData/Processed/ccr_mask.tif", # maps_out[["basin"]],
  sand = "Polaris_SoilData/Processed/ccr_sand_map.tif",
  clay = "Polaris_SoilData/Processed/ccr_clay_map.tif",
  plot_out = file.path(output_dir, "SoilTextures_baseline.pdf")
)

#
# 1 clay
# 2 silty-clay
# 3 silty-clay-loam
# 4 sandy-clay
# 5 sandy-clay-loam
# 6 clay-loam
# 7 silt
# 8 silt-loam
# 9 loam
# 10 sand
# 11 loamy-sand
# 12 sandy-loam

plot(soil_texture)

writeRaster(soil_texture, file.path(output_dir, "ccr_soils_texture.tif"),overwrite=T)

zz <- rast("Polaris_SoilData/Processed/ccr_soils_texture.tif")
plot(zz)
