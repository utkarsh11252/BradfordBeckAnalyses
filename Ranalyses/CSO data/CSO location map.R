library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

cso = read.csv("Ranalyses/CSO data/Water_Company_CSO_Data_2019_6875022873306555620.csv")
cso_sf = cso %>%
        filter (DISTRICT_COUNCIL == "BRADFORD") %>%
        st_as_sf(coords = c("x","y"), crs = "EPSG:27700")

write_sf(cso_sf,"Ranalyses/CSO data/CSO.shp")
