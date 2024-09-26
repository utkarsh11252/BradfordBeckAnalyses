#############################################################################################
# Loading the required libraries
#############################################################################################
#install.packages("ncdf4")
#install.packages("RNetCDF")
#install.packages("purrr")

library(data.table)
library(reshape2)
library(ncdf4)
library(sf)
library(purrr)
library(dplyr)


#############################################################################################
# Function to convert NetCDF file to R
#############################################################################################
process_nc_data <- function(nc_data) {
  # Extract variables from NetCDF data
  rainfall <- ncvar_get(nc_data, "rainfall")
  time <- ncvar_get(nc_data, "time")
  x_coord <- ncvar_get(nc_data, "projection_x_coordinate")
  y_coord <- ncvar_get(nc_data, "projection_y_coordinate")
  latitude <- ncvar_get(nc_data, "latitude")
  longitude <- ncvar_get(nc_data, "longitude")
  
  # Convert time to POSIXct format
  origin <- as.POSIXct("1800-01-01 00:00:00", tz = "UTC")
  time <- origin + time * 3600  # Convert hours since origin to POSIXct format
  
  # Reshape the rainfall data into a long format
  rainfall_long <- melt(rainfall, varnames = c("x", "y", "time"), value.name = "rainfall")
  rainfall_long$x_coord <- x_coord[rainfall_long$x]
  rainfall_long$y_coord <- y_coord[rainfall_long$y]
  rainfall_long$time <- time[rainfall_long$time]
  rainfall_long$latitude <- latitude[cbind(rainfall_long$x, rainfall_long$y)]
  rainfall_long$longitude <- longitude[cbind(rainfall_long$x, rainfall_long$y)]
  
  # Create final data frame
  rainfall_df <- data.frame(
    time = rainfall_long$time,
    x_coord = rainfall_long$x_coord,
    y_coord = rainfall_long$y_coord,
    latitude = rainfall_long$latitude,
    longitude = rainfall_long$longitude,
    rainfall = rainfall_long$rainfall
  )
  
  # Close the NetCDF file
  nc_close(nc_data)
  
  # Convert to data table
  rainfall_dt <- as.data.table(rainfall_df)
  
  # Filter data for Bradford region
  bradford_x_min <- 407666.9
  bradford_x_max <- 419121.1
  bradford_y_min <- 429921
  bradford_y_max <- 437893.5
  
  bradford_data <- rainfall_dt[x_coord >= bradford_x_min & x_coord <= bradford_x_max &
                                 y_coord >= bradford_y_min & y_coord <= bradford_y_max]
  
  return(bradford_data)
}


#############################################################################################
# Opening the .nc files and converting them to appropriate format
#############################################################################################
nc_1 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20190901-20190930.nc")
nc_2 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20191001-20191031.nc")
nc_3 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20191101-20191130.nc")
nc_4 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20191201-20191231.nc")
nc_5 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200101-20200131.nc")
nc_6 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200201-20200229.nc")
nc_7 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200301-20200331.nc")
nc_8 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200401-20200430.nc")
nc_9 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200501-20200531.nc")
nc_10 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200601-20200630.nc")
nc_11 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200701-20200731.nc")
nc_12 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200801-20200831.nc")
nc_13 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20200901-20200930.nc")
nc_14 <- nc_open("Ranalyses/rainfall data raw/rainfall_hadukgrid_uk_1km_day_20201001-20201031.nc")


bd1 <- process_nc_data(nc_1)
bd2 <- process_nc_data(nc_2)
bd3 <- process_nc_data(nc_3)
bd4 <- process_nc_data(nc_4)
bd5 <- process_nc_data(nc_5)
bd6 <- process_nc_data(nc_6)
bd7 <- process_nc_data(nc_7)
bd8 <- process_nc_data(nc_8)
bd9 <- process_nc_data(nc_9)
bd10 <- process_nc_data(nc_10)
bd11 <- process_nc_data(nc_11)
bd12 <- process_nc_data(nc_12)
bd13 <- process_nc_data(nc_13)
bd14 <- process_nc_data(nc_14)


dfs <- list(bd1,bd2,bd3,bd4,bd5,bd6,bd7,bd8,bd9,bd10,bd11,bd12,bd13,bd14)
rain_df = bind_rows(dfs)

write.csv(rain_df,"Ranalyses/radar rainfall/data/rainfall_radar.csv", row.names=FALSE)


