# set wd to project folder 
setwd('C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project')

# load required packages
library(raster)
library(sf)
library(tidyverse)

# tree canopy cover 
nlcd_tree <- raster("nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif")

# impervious surface product
nlcd_imp <- raster("Annual_NLCD_FctImp_2021_CU_C1V0.tif")

# Read NYC shapefile
nyc_boundary <- st_read("C:/Users/steve/Downloads/NYS_Civil_Boundaries.shp/Borough Boundaries_20250411/geo_export_2d503d80-6699-4811-bc7b-2f1d04ca9b31.shp")
nyc_boundary %>% mapview::mapview()
crs(nyc_boundary)

# View attributes and filter to exclude Staten Island (less urban)
#View(nyc_boundary)
nyc_minus_SI <- nyc_boundary %>% filter(borocode %in% c(1,2,3,4))
nyc_minus_SI %>% mapview::mapview()


# transform shp crs to match TCC and IMP
tcc_crs <- crs(nlcd_tree)
imp_crs <- crs(nlcd_imp)

nyc_minus_SI_tree <- st_transform(nyc_minus_SI, crs=tcc_crs)
nyc_minus_SI_tree %>% mapview::mapview()

nyc_minus_SI_imp <- st_transform(nyc_minus_SI, crs=imp_crs)
nyc_minus_SI_imp %>% mapview::mapview()

# crop both rasters to study area 

nlcd_tree_nyc <- crop(nlcd_tree, nyc_minus_SI_tree)
nlcd_imp_nyc <- crop(nlcd_imp, nyc_minus_SI_imp)

# crop just changes extent based on extent of shp 
# mask to select nyc boroughs only, not rectangular shape 
nlcd_tree_nyc <- mask(nlcd_tree_nyc, nyc_minus_SI_tree)
nlcd_imp_nyc <- mask(nlcd_imp_nyc, nyc_minus_SI_imp)



# check that rasters have the same extent, resolution, and projection
# they should given theyre both derived from landsat/NLCD
res(nlcd_tree_nyc)
res(nlcd_imp_nyc)
extent(nlcd_tree_nyc)
extent(nlcd_imp_nyc)
crs(nlcd_tree_nyc)
crs(nlcd_imp_nyc)

# different crs, so resmaple in order to compare
nlcd_tree_nyc <- resample(nlcd_tree_nyc, nlcd_imp_nyc)

View(nlcd_tree_nyc)
# identify tree canopy pixels (where tree canopy cover > 30) to make simplified polygons
tree_mask <- nlcd_tree_nyc > 30

tree_mask %>% mapview::mapview()



# create masks for the two vegetation categories 
# based on impervious surface percentage in same pixel

# urban vegetation: tcc with >= 50% imp
urban_trees <- tree_mask & (nlcd_imp_nyc >= 50)

# Park vegetation: tcc with < 50% imp
park_trees <- tree_mask & (nlcd_imp_nyc < 50)

# view to make sure park trees contain major parks and green areas (cemeteries, etc.)
park_trees %>% mapview::mapview()
urban_trees %>% mapview::mapview()

# rural comparision area 
rural_extent <- as(raster::extent(-73.15451, -73.13527, 40.75146, 40.77536), "SpatialPolygons")
proj4string(rural_extent) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
rural_extent <- st_as_sf(rural_extent)

rural_extent %>% mapview::mapview()


# repeat for selecting tcc area 
rural_extent <- st_transform(rural_extent, crs=tcc_crs)

tcc_rural <- crop(nlcd_tree, rural_extent)
#tcc_rural <- mask(tcc_rural, rural_extent)

rural_tree_mask <- tcc_rural > 30


rural_tree_mask %>% mapview::mapview()


# raster to polygons masks for input to LandsatTS
#urban_trees_poly <- rasterToPolygons(urban_trees, dissolve=TRUE)
#park_trees_poly <- rasterToPolygons(park_trees, dissolve=TRUE)
#rural_poly <- rasterToPolygons(rural_tree_mask, dissolve=TRUE)


plot(urban_trees_poly)
# convert to sf objects
urban_trees_sf <- st_as_sf(urban_trees_poly)
park_trees_sf <- st_as_sf(park_trees_poly)
rural_trees_sf <- st_as_sf(rural_poly)


# save as shp files
st_write(urban_trees_sf, "urban_trees.shp")
st_write(park_trees_sf, "park_trees.shp")
st_write(rural_trees_sf, "rural_trees.shp", append=FALSE)


# load LandsatTS package
install.packages("tinytex")
tinytex::install_tinytex()

devtools::install_github("logan-berner/LandsatTS")



# Load required packages

library(LandsatTS)
library(sf)
library(dplyr)
library(data.table)
library(rgee)

# Initialize Earth Engine
ee_install_upgrade()
ee_clean_pyenv()

rgee::ee_install()
# ERROR w/ rgee and python envrionment, not sure what is wrong, i have tried to fix using the error prompts
ee_Initialize()
ee_install()
