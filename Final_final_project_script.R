# set wd to project folder 
setwd('C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project') 

# load required packages 
library(raster) 
library(tidyverse) 

# tree canopy cover 
nlcd_tree <- raster("nlcd_tcc_CONUS_2021_v2021-4/nlcd_tcc_conus_2021_v2021-4.tif") 

# impervious surface product 
nlcd_imp <- raster("Annual_NLCD_FctImp_2021_CU_C1V0.tif") 

# Read NYC shapefile 
nyc_boundary <- st_read("C:/Users/steve/Downloads/NYS_Civil_Boundaries.shp/Borough Boundaries_20250411/geo_export_2d503d80-6699-4811-bc7b-2f1d04ca9b31.shp") 

# check shapefile
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

# crop just changes extent based on extent of shp # mask to select nyc boroughs only, not rectangular shape 
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
#View(nlcd_tree_nyc) 

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
# no need to mask, keep study area rectangular since it is one big preserve 

rural_tree_mask <- tcc_rural > 30 
rural_tree_mask %>% mapview::mapview() 

# get CRS of resampled rasters for creation of SF points objects 
UT_crs <- crs(urban_trees) 
PT_crs <- crs(park_trees) 

# raster to point masks for input to LandsatTS 
urban_trees_pt <- rasterToPoints(urban_trees) 
park_trees_pt <- rasterToPoints(park_trees) 
rural_trees_pt <- rasterToPoints(rural_tree_mask) 

urban_trees_sf <- as.data.frame(urban_trees_pt) %>% filter(layer == 1 )%>% st_as_sf(coords = c("x", "y"), crs = UT_crs) 
park_trees_sf <- as.data.frame(park_trees_pt) %>% filter(layer == 1 )%>% st_as_sf(coords = c("x", "y"), crs = UT_crs) 
rural_trees_sf <- as.data.frame(rural_trees_pt) %>% filter(layer == 1 )%>% st_as_sf(coords = c("x", "y"), crs = UT_crs) 


rural_trees_sf <- st_read("C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project/rural_trees.shp")
park_trees_sf <- st_read("C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project/park_trees.shp")
urban_trees_sf <- st_read("C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project/urban_trees.shp")

ggplot() + 
  geom_sf(data = nyc_minus_SI) +
  geom_sf(data = park_trees_sf, aes(color = "Park Trees"), size = 0.001, alpha = 0.7) + 
  geom_sf(data = urban_trees_sf, aes(color = "Urban Trees"),size = 0.001, alpha = 0.7) +
  scale_color_manual(values = c("Urban Trees" = "blue", "Park Trees" = "green")) + 
  labs(title = "NYC Urban vs. Park Trees", color = "Legend") + theme_minimal() 

rural_trees_sf %>% mapview::mapview() 

# save as shp files 
st_write(urban_trees_sf, "urban_trees.shp", append = FALSE) 
st_write(park_trees_sf, "park_trees.shp", append = FALSE) 
st_write(rural_trees_sf, "rural_trees.shp", append=FALSE) 

# load LandsatTS package 
install.packages("tinytex") 
tinytex::install_tinytex() 
devtools::install_github("logan-berner/LandsatTS") 

# load rgee - need python environment
library(rgee)


# find Python path
py_path <- "C:/Users/steve/anaconda3/envs/geocomp/python.exe"


# set this environment for rgee
ee_install_set_pyenv(
  py_path = py_path,
  py_env = "rgee", 
  confirm = TRUE
)


# load required sessions
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(rgee)
library(LandsatTS)


# set working directory
setwd('C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project')


# initialize Google Earth Engine (GEE) API
ee_Authenticate()
ee_Initialize(project='reuprecipitationmodel')




# load point shapefiles
rural_trees_sf <- st_read("rural_trees.shp")
park_trees_sf <- st_read("park_trees.shp")
urban_trees_sf <- st_read("urban_trees.shp")


# transform to WGS84 (EPSG:4326), needed for GEE
rural_trees_sf <- st_transform(rural_trees_sf, 4326)
park_trees_sf <- st_transform(park_trees_sf, 4326)
urban_trees_sf <- st_transform(urban_trees_sf, 4326)


# add an identifier column to each shapefile
rural_trees_sf$region <- "rural"
park_trees_sf$region <- "park"
urban_trees_sf$region <- "urban"


# add unique sample_id to each point
# needed for lsat_export_ts() which requires unique identifier for each point
rural_trees_sf$sample_id <- paste0("rural_", 1:nrow(rural_trees_sf))
park_trees_sf$sample_id <- paste0("park_", 1:nrow(park_trees_sf))
urban_trees_sf$sample_id <- paste0("urban_", 1:nrow(urban_trees_sf))


# combine all points into a single sf
all_points_sf <- rbind(rural_trees_sf, park_trees_sf, urban_trees_sf)


# Step 2: Export Landsat time series for all points for 2021
# simpler than working with polygons bc no need to calculate polygon pixel center
# The lsat_export_ts() function works with point feature collections
task_list <- lsat_export_ts(all_points_sf,
                            sample_id_from = "sample_id", # Specify which column has the unique IDs
                            start_date = "2021-01-01",
                            end_date = "2021-12-31",
                            drive_export_dir = "landsat_2021",
                            file_prefix = "landsat_2021")
# Monitor tasks to completion
# Comment out if running in a non-interactive session
# This will wait for each task to complete before proceeding
# map(task_list, ee_monitoring)


# Alternative approach to start tasks
# Convert to Python object first and then call the start method
for(i in 1:length(task_list)) {
  # Convert the R representation to a proper Python object
  python_task <- reticulate::r_to_py(task_list[[i]])
  # Call the start method
  python_task$start()
  # Print progress
  cat("Started task", i, "of", length(task_list), "\n")
}


ee_monitoring()



# call all the landsat files exported to drive, downloaded manually and put into one folder
files <- list.files("C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project/landsat_2021", pattern = ".csv", full.names = TRUE)
files
# read and combine all files into a single data table
lsat_data <- do.call("rbind", lapply(files, fread))






# lsat_format_data
# this extracts a landsat time-series from EE
# accepts any sf object w/ point feature collection
# requires unique identifier (sample_id)
# parses columns, renames bands, scales values
lsat_data <- lsat_format_data(lsat_data)




# clean the data
# filter for max cloud cover (80% due to limited revisits)
# filter for snow and water as well
lsat_data <- lsat_clean_data(lsat_data,
                             geom.max = 15,
                             cloud.max = 80,
                             sza.max = 60,
                             filter.cfmask.snow = TRUE,
                             filter.cfmask.water = TRUE,
                             filter.jrc.water = TRUE)


#[1] "removed 1260628 of 2009103 observations (62.75%)"






# calculate NDVI
lsat_data <- lsat_calc_spectral_index(lsat_data, si = 'ndvi')




# working with multiple Landsat sensors
# need to calibrate NDVI using Random Forest ML
# all parameters are kept as in the LandsatTS readme tutorial
lsat_data <- lsat_calibrate_rf(lsat_data,
                               band.or.si = 'ndvi',
                               doy.rng = 151:242,
                               min.obs = 5,
                               frac.train = 0.75,
                               overwrite.col = TRUE,
                               write.output = FALSE,
                               train.with.highlat.data = TRUE)


# band.or.si sat uncal.bias uncal.bias.pcnt rf.r2 rf.rmse rf.n xval.r2 xval.rmse
# 1 ndvi LANDSAT_8 0.032 5 0.97 0.033 5927 0.969 0.033
# xval.n xval.bias xval.bias.pcnt
# 1 1976 -0.001 -0.2


# fit phenological curves to the NDVI time series
# determine phenology for each site
# iteratively fits cubic splines to observations pooled over specifed # of years
# then determine deviation of each observation from curve-fit maximum summer greenness
# ^ definition from landsatTS
lsat_pheno <- lsat_fit_phenological_curves(lsat_data,
                                           si = 'ndvi',
                                           window.yrs = 1, # one year of data only
                                           window.min.obs = 5,
                                           spl.fit.outfile = FALSE,
                                           progress = TRUE)


# "compute mean, median, 90th percentile of NDVI using observations from growing season
# estimate annual maximum greeness using phenology modeling" - LandsatTS
lsat_gs <- lsat_summarize_growing_seasons(lsat_pheno,
                                          si = 'ndvi',
                                          min.frac.of.max = 0.75)


str(lsat_gs)




# estimate maximum greenes vs number of landsat available scenes
# shows how siginificant these estimates are based on number of observations
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat_pheno, si = 'ndvi', min.obs = 10, reps = 5, min.frac.of.max = 0.75, outdir = NA)


# extract region info from sample.id
lsat_pheno$region <- gsub("_.*", "", lsat_pheno$sample.id)


lsat_pheno$region




# change names for figures
# region defines study classes
lsat_pheno <- lsat_pheno %>%
  mutate(region = case_when(
    region == "rural" ~ "Rural",
    region == "park" ~ "Park",
    region == "urban" ~ "Urban",
    TRUE ~ region
  ))




# plot phenological curves by region
ggplot(lsat_pheno, aes(x = doy, y = ndvi, color = region)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(data = subset(lsat_pheno, region == "Rural"),
              method = "gam", formula = y ~ s(x, bs = "cs", k = 6), se = TRUE) +
  geom_smooth(data = subset(lsat_pheno, region %in% c("Park", "Urban")),
              method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
  labs(title = "Phenological Curves by Tree Type (2021)",
       x = "Day of Year",
       y = "NDVI",
       color = "Region Type") +
  scale_color_manual(values = c("Rural" = "darkgreen",
                                "Park" = "lightgreen",
                                "Urban" = "orange")) +
  theme_minimal()


View(lsat_pheno)


# number of observations by region
# number of revist days
region_summary <- lsat_pheno %>%
  group_by(region) %>%
  summarize(
    n_observations = n(),
    n_unique_sites = n_distinct(sample.id),
    n_unique_days = n_distinct(doy)
  )


region_summary


#region n_observations n_unique_sites n_unique_days
#<chr> <int> <int> <int>
# 1 Park 661129 62721 14
# 2 Rural 18444 3675 6
# 3 Urban 50163 4862 14


# Calculate summary statistics
# lsat_gs has sample.id with each sample in format "region_observation#" i.e. "park_1"
# gsub splits the str in the region column on the underscore and replaces the remainder after the underscore
# with blank to get comprehensive summary
# summarize mean ndvi max and ndvi max standard deviation along with
# mean doy max and sd and count of observations
phenology_summary <- lsat_gs %>%
  mutate(region = gsub("_.*", "", sample.id)) %>%
  group_by(region) %>%
  summarize(
    mean_ndvi_max = mean(ndvi.max, na.rm = TRUE),
    sd_ndvi_max = sd(ndvi.max, na.rm = TRUE),
    mean_doy_max = mean(ndvi.max.doy, na.rm = TRUE),
    sd_doy_max = sd(ndvi.max.doy, na.rm = TRUE),
    n_samples = n() # Count number of samples per region
  )


phenology_summary


# region mean_ndvi_max sd_ndvi_max mean_doy_max sd_doy_max n_samples
# <chr> <dbl> <dbl> <dbl> <dbl> <int>
# 1 park 0.810 0.0845 183. 34.4 62721
# 2 rural 0.892 0.0257 234. 17.3 3675
# 3 urban 0.674 0.0989 180. 34.7 4862




# add region column
lsat_gs_with_region <- lsat_gs %>%
  mutate(region = gsub("_.*", "", sample.id))


# set min size to minumum observations per region - rural
min_size <- 3675


# create balanced dataset by sampling randomly
set.seed(123) # set seed to reproduce


# group by region and sample min size then ungroup
balanced_data <- lsat_gs_with_region %>%
  group_by(region) %>%
  sample_n(min_size) %>%
  ungroup()


# repeat for pheno
balanced_pheno <- lsat_pheno %>%
  group_by(region) %>%
  sample_n(min_size) %>%
  ungroup()


# check counts
table(balanced_data$region)
table(balanced_pheno$region)




# Create boxplots of key phenological metrics with correct column names
p1 <- ggplot(balanced_data %>% mutate(region = gsub("_.*", "", sample.id)),
             aes(x = region, y = ndvi.max, fill = region)) +
  geom_boxplot() +
  labs(title = "Maximum NDVI by Region",
       x = "Region Type",
       y = "Maximum NDVI") +
  scale_fill_manual(values = c("rural" = "darkgreen",
                               "park" = "lightgreen",
                               "urban" = "orange")) +
  theme_minimal()


# Use ndvi.gs.avg as an alternative if season length isn't available
p2 <- ggplot(balanced_data %>% mutate(region = gsub("_.*", "", sample.id)),
             aes(x = region, y = ndvi.gs.avg, fill = region)) +
  geom_boxplot() +
  labs(title = "Mean Growing Season NDVI by Region",
       x = "Region Type",
       y = "Mean NDVI") +
  scale_fill_manual(values = c("rural" = "darkgreen",
                               "park" = "lightgreen",
                               "urban" = "orange")) +
  theme_minimal()


# Display plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)




# ANOVA test for differences in phenology among regions
anova_max <- aov(ndvi.max ~ region, data = balanced_data %>%
                   mutate(region = gsub("_.*", "", sample.id)))
summary(anova_max)


#Df Sum Sq Mean Sq F value Pr(>F)
#region 2 89.22 44.61 7691 <2e-16 ***
# Residuals 11022 63.94 0.01
#---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


anova_doy <- aov(ndvi.max.doy ~ region, data = balanced_data %>%
                   mutate(region = gsub("_.*", "", sample.id)))
summary(anova_doy)


# Df Sum Sq Mean Sq F value Pr(>F)
# region 2 6622393 3311196 3666 <2e-16 ***
# Residuals 11022 9955627 903
# ---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1






anova_gsavg <- aov(ndvi.gs.avg ~ region, data = balanced_data %>%
                     mutate(region = gsub("_.*", "", sample.id)))
summary(anova_gsavg)


#Df Sum Sq Mean Sq F value Pr(>F)
#region 2 108.27 54.14 9156 <2e-16 ***
# Residuals 11022 65.17 0.01
#---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1










names(balanced_data)
names(balanced_pheno)




View(balanced_data)










# calculate SOS, EOS, and DUR using phenological data
# use full data and not balanced because there arent enough data values to accurately calculate
lsat_gs_with_length <- lsat_gs %>%
  # add the region column
  mutate(region = gsub("_.*", "", sample.id)) %>%
  # join with lsat_pheno to access time series data
  left_join(lsat_pheno, by = "sample.id") %>%
  # group by sample ID to calculate values for each point
  group_by(sample.id) %>%
  # calculate Start of Season (SOS), End of Season (EOS), and Growing Season Length (GSL)
  summarize(
    # add longitude and latitude of sample point for later comparision with IMP percent
    longitude = first(longitude.x),
    latitude = first(latitude.x),
    region = first(region.x),
    
    # Start of Season Time (SOS)
    # "Day of year identified as having a consistent upward trend in time series NDVI" - USGS
    # find minumum doy where ndvi value exceeds 20% of NDVI range + minumum NDVI
    SOS = min(doy[which(ndvi >= (min(ndvi, na.rm = TRUE) +
                                   0.2 * (max(ndvi, na.rm = TRUE) - min(ndvi, na.rm = TRUE))))],
              na.rm = TRUE),
    
    # get first NDVI at SOS day
    SOSN = ndvi[which.min(abs(doy - SOS))][1],
    
    # End of Season Time (EOS)
    # "Day of year identified at the end of a consistent downward trend in time series NDVI" - USGS
    # find max or last doy that exceeds the 20% of NDVI range + minimum NDVI
    EOS = max(doy[which(ndvi >= (min(ndvi, na.rm = TRUE) +
                                   0.2 * (max(ndvi, na.rm = TRUE) - min(ndvi, na.rm = TRUE))))],
              na.rm = TRUE),
    
    # End of Season NDVI (EOSN)
    # NDVI value at EOS
    EOSN = ndvi[which.min(abs(doy - EOS))][1],
    
    # retain existing metrics
    ndvi.max = first(ndvi.max),
    ndvi.gs.avg = first(ndvi.gs.avg),
    ndvi.max.doy = first(ndvi.max.doy)
  ) %>%
  # calculate growing season length as EOS - SOS
  mutate(growing_season_length = EOS - SOS) %>%
  ungroup()




# Compare with impervious surface percentage
library(raster)

# load the impervious surface raster
imp_raster <- raster("C:/Users/steve/Downloads/G385_R_Data_Viz/R_Spatial/Final_project/Annual_NLCD_FctImp_2021_CU_C1V0.tif")




# spatial points from tree locations in lsat_gs_with_length
tree_points <- lsat_gs_with_length %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# transform to match imp_raster crs
tree_points <- st_transform(tree_points, crs(imp_raster))


# extract impervious surface values
imp_values <- extract(imp_raster, st_coordinates(tree_points))


# add impervious surface values a df
tree_data <- tree_points %>%
  st_drop_geometry() %>%
  mutate(percent_impervious = imp_values)


# plot GSL v imp
ggplot(tree_data, aes(x = percent_impervious, y = growing_season_length, color = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = paste0(region, "_line")), method = "lm", se = TRUE) +
  labs(title = "Growing Season Length vs. Percent Impervious Surface",
       y = "Growing Season Length (days)",
       x = 'Percent Impervious Surface',
       color = "Region Type") +
  scale_color_manual(values = c("rural" = "darkgreen",
                                "park" = "lightgreen",
                                "urban" = "orange",
                                "rural_line" = "darkgreen",
                                "park_line" = "chartreuse4",
                                "urban_line" = "red")) +
  theme_minimal()




# SOS v IMP
p1 <- ggplot(tree_data, aes(x = percent_impervious, y = SOS, color = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = paste0(region, "_line")), method = "lm", se = TRUE) +
  labs(title = "SOS vs. Percent Impervious Surface",
       y = "Start of Season (day)",
       color = "Region Type") +
  scale_color_manual(values = c("rural" = "darkgreen",
                                "park" = "lightgreen",
                                "urban" = "orange",
                                "rural_line" = "darkgreen",
                                "park_line" = "chartreuse4",
                                "urban_line" = "red")) +
  theme_minimal() +
  theme(legend.position = "none") + # remove legend because both plots share legend
  theme(axis.title.x = element_blank()) # remove x-axis because both plots share same x


# EOS v IMP
p2 <- ggplot(tree_data, aes(x = percent_impervious, y = EOS, color = region)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(color = paste0(region, "_line")), method = "lm", se = TRUE) +
  labs(title = "EOS vs. Percent Impervious Surface",
       y = "End of Season (day)",
       color = "Region Type") +
  scale_color_manual(values = c("rural" = "darkgreen",
                                "park" = "lightgreen",
                                "urban" = "orange",
                                "rural_line" = "darkgreen",
                                "park_line" = "chartreuse4",
                                "urban_line" = "red")) +
  theme_minimal() +
  theme(axis.title.x = element_blank()) # Remove x-axis title but keep tick labels


# shared x-axis title
shared_x_title <- "Percent Impervious Surface"



# show both plots with shared x axis tile on bottom
grid.arrange(p1, p2, ncol = 2, bottom = shared_x_title)


# calculate summary statistics for growing season length by region
gs_length_summary <- lsat_gs_with_length %>%
  group_by(region) %>%
  summarize(
    mean_length = mean(growing_season_length, na.rm = TRUE),
    median_length = median(growing_season_length, na.rm = TRUE),
    sd_length = sd(growing_season_length, na.rm = TRUE),
    n_samples = sum(!is.na(growing_season_length))
  )


gs_length_summary


#region mean_length median_length sd_length n_samples
#<chr> <dbl> <dbl> <dbl> <int>
# 1 park 74.6 80 10.0 62721
#2 rural 69.4 72 5.67 3675
#3 urban 75.3 80 11.0 4862




# ANOVA to test for significant differences in GSL
anova_length <- aov(growing_season_length ~ region,
                    data = lsat_gs_with_length)
summary(anova_length)


#Df Sum Sq Mean Sq F value Pr(>F)
#region 2 96845 48423 493.3 <2e-16 ***
# Residuals 71255 6995034 98
#---
# Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


