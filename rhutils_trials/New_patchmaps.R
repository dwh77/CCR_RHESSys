### Working on figuring out patch sizes

library(terra)

## current watershed and patches for HPB
watershed <- rast("HPB_files_NEW/spatial_data/hpb_watershed.tiff")
plot((watershed), main = "watershed map")

terra::expanse(watershed, unit = "m")


patches <- rast("HPB_files_NEW/spatial_data/hpb_patches_basin1000_fill_feb26.tiff")
plot(patches)
unique(patches)

patches_small <- rast("HPB_files_NEW/spatial_data/hpb_patch_clump_diag.tiff")
plot(patches_small)
unique(patches_small) #so my small patches has ~30,000 patches

#get area in km2
terra::expanse(patches, unit = "km")



#### example data info
test_watershed <- rast("Testing_rhessys_ExampleFiles/spatial_data/subbasins50.tiff")
plot(test_watershed)
terra::expanse(test_watershed, unit = "km")


test_patches <- rast("Testing_rhessys_ExampleFiles/spatial_data/pch30mh50.tiff")
plot(test_patches)
unique(test_patches)
terra::expanse(test_patches, unit = "km")

ncell(test_patches)


##### Ok so testing area is 0.25 km2; and has 247 patches
4.29/0.26
### so HPB is ~16.5 times larger so
247*16.5
#so should have ~4100 patches


### Lets make a smaller patch grid thats ~4K
hpb_dem <- rast("HPB_files_NEW/spatial_data/hpb_dem.tiff")
plot(hpb_dem)
unique(hpb_dem)
terra::expanse(hpb_dem, unit = "km")


crs(hpb_dem)


##try clumps and kmeans
xy <- as.data.frame(hpb_dem, xy = TRUE, na.rm = TRUE)
set.seed(42)
#did 2000 initially
km <- kmeans(scale(xy), centers = 500, nstart = 3, iter.max = 50)
patch_map <- hpb_dem
patch_map[!is.na(hpb_dem)] <- km$cluster

plot(patch_map)
plot(patch_map, type = "classes")
unique(patch_map)

expanse(patch_map, unit="km")

writeRaster(patch_map, "HPB_files_NEW/spatial_data/hpb_patches_kmeans_500.tiff", overwrite=T)

kmeans <- rast("HPB_files_NEW/spatial_data/hpb_patches_kmeans_500.tiff")
plot(kmeans)
unique(kmeans)

