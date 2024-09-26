#############################################################################################
# Loading the required libraries
#############################################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(readxl)


#############################################################################################
# Loading the radar rainfall data and converting it to sf format
#############################################################################################
rain_df <- read.csv("Ranalyses/radar rainfall/data/rainfall_radar.csv")
head(rain_df)

rain_sf <- st_as_sf(rain_df, coords = c("x_coord","y_coord"), crs="EPSG:27700")
plot(rain_sf$geometry)


#############################################################################################
# Loading the monitoring sites data and plotting it with radar rainfall sites
#############################################################################################
monitor <- read_excel("Data/YWS study/YWS Monitoring Locations with RH edits.xlsx")

monitor_sf <- st_as_sf(monitor, coords = c("Longitude","Latitude"), crs="EPSG:4326")
monitor_sf_b <- st_transform(monitor_sf,crs="EPSG:27700")

plot(monitor_sf_b$geometry, col="red",add=TRUE)


#############################################################################################
# Filtering out the flow sites
#############################################################################################
flow_sites = monitor_sf_b %>%
  filter(`Monitoring type` == "Water flow")

plot(flow_sites$geometry, add=TRUE, col="blue")

quality_sites = monitor_sf_b %>%
  filter(`Monitoring type` == "Water quality")

#############################################################################################
# Getting the radar rainfall sites data
#############################################################################################
unique_sites <- rain_sf %>%
  distinct(geometry)

plot(unique_sites$geometry)
write_sf(unique_sites, "Ranalyses/radar rainfall/data/unique_sites.shp")


#############################################################################################
# Function to find the nearest radar rainfall site to each flow monitoring site
#############################################################################################
find_nearest_points <- function(flow, sites) {
  # Calculate distances between flow sites and unique sites
  distances <- st_distance(flow, sites)
  
  # For each point in unique_sites, find the index of the nearest point in flow_sites
  nearest_indices <- apply(distances, 1, which.min)
  
  # Extract unique row indices for the nearest points in unique_sites
  unique_nearest_indices <- unique(nearest_indices)
  
  # If unique_sites has repeated points (same geometry), keep all rows with these geometries
  unique_geometries <- st_geometry(sites[unique_nearest_indices, ])
  near_sites <- sites %>% filter(st_geometry(sites) %in% unique_geometries)
  
  return(near_sites)
}


find_nearest_four_sites <- function(flow, sites) {
  # Calculate distances between the flow site and all site geometries
  distances <- st_distance(flow, sites)
  
  # Order the sites by distance and select the indices of the four nearest
  nearest_indices <- order(distances)[1:4]
  
  # Extract the four nearest sites based on the indices
  nearest_sites <- sites[nearest_indices, ]
  
  return(nearest_sites)
}


#############################################################################################
# Computing the radar rainfall data for each flow monitoring site
#############################################################################################
f1_r = find_nearest_four_sites(flow_sites[1,], unique_sites)
rain_f1 <- rain_sf %>%
  filter(geometry %in% f1_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


f2_r = find_nearest_four_sites(flow_sites[2,], unique_sites)
rain_f2 <- rain_sf %>%
  filter(geometry %in% f2_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')

f3_r = find_nearest_four_sites(flow_sites[3,], unique_sites)
rain_f3 <- rain_sf %>%
  filter(geometry %in% f3_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


f4_r = find_nearest_four_sites(flow_sites[4,], unique_sites)
rain_f4 <- rain_sf %>%
  filter(geometry %in% f4_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


f5_r = find_nearest_four_sites(flow_sites[5,], unique_sites)
rain_f5 <- rain_sf %>%
  filter(geometry %in% f5_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


f6_r = find_nearest_four_sites(flow_sites[6,], unique_sites)
rain_f6 <- rain_sf %>%
  filter(geometry %in% f6_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


write.csv(rain_f1, "Ranalyses/radar rainfall/data/rain_f1.csv")
write.csv(rain_f2, "Ranalyses/radar rainfall/data/rain_f2.csv")
write.csv(rain_f3, "Ranalyses/radar rainfall/data/rain_f3.csv")
write.csv(rain_f4, "Ranalyses/radar rainfall/data/rain_f4.csv")
write.csv(rain_f5, "Ranalyses/radar rainfall/data/rain_f5.csv")
write.csv(rain_f6, "Ranalyses/radar rainfall/data/rain_f6.csv")


#############################################################################################
# Computing the radar rainfall data for each quality monitoring site
#############################################################################################
q1_r = find_nearest_four_sites(quality_sites[1,], unique_sites)
rain_q1 <- rain_sf %>%
  filter(geometry %in% q1_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')

q2_r = find_nearest_four_sites(quality_sites[2,], unique_sites)
rain_q2 <- rain_sf %>%
  filter(geometry %in% q2_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')

q3_r = find_nearest_four_sites(quality_sites[3,], unique_sites)
rain_q3 <- rain_sf %>%
  filter(geometry %in% q3_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


q4_r = find_nearest_four_sites(quality_sites[4,], unique_sites)
rain_q4 <- rain_sf %>%
  filter(geometry %in% q4_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


q5_r = find_nearest_four_sites(quality_sites[5,], unique_sites)
rain_q5 <- rain_sf %>%
  filter(geometry %in% q5_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


q6_r = find_nearest_four_sites(quality_sites[6,], unique_sites)
rain_q6 <- rain_sf %>%
  filter(geometry %in% q6_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


q7_r = find_nearest_four_sites(quality_sites[7,], unique_sites)
rain_q7 <- rain_sf %>%
  filter(geometry %in% q7_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')


q8_r = find_nearest_four_sites(quality_sites[8,], unique_sites)
rain_q8 <- rain_sf %>%
  filter(geometry %in% q8_r$geometry) %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarize(rainfall = mean(rainfall, na.rm = TRUE), .groups = 'drop')



write.csv(rain_q1, "Ranalyses/radar rainfall/data/rain_q1.csv")
write.csv(rain_q2, "Ranalyses/radar rainfall/data/rain_q2.csv")
write.csv(rain_q3, "Ranalyses/radar rainfall/data/rain_q3.csv")
write.csv(rain_q4, "Ranalyses/radar rainfall/data/rain_q4.csv")
write.csv(rain_q5, "Ranalyses/radar rainfall/data/rain_q5.csv")
write.csv(rain_q6, "Ranalyses/radar rainfall/data/rain_q6.csv")
write.csv(rain_q7, "Ranalyses/radar rainfall/data/rain_q7.csv")
write.csv(rain_q8, "Ranalyses/radar rainfall/data/rain_q8.csv")

