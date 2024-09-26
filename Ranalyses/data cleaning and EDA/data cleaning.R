#############################################################################################
# importing the dependencies 
#############################################################################################
library(sf)
library(tmap)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(ggmice)
library(lubridate)
library(tidyr)
library(imputeTS)
library(mice)
library(rlang)
library(VIM)
library(naniar)
library(caret)


#############################################################################################
# getting locations of the flow, quality and rainfall monitoring sites and visualizing it
#############################################################################################
loc = read.xlsx("Data/YWS study/YWS Monitoring Locations with RH edits.xlsx")
loc

loc_sf = st_as_sf(loc, coords = c("Longitude","Latitude"), crs = "EPSG:4326")
plot(loc_sf$geometry)
loc_sf = st_transform(loc_sf,crs="EPSG:4326")

tmap_mode("view")  

tm_shape(loc_sf) +
  tm_basemap("OpenStreetMap") +  # Use OSM as basemap
  tm_symbols(col = "red", size = 0.5, border.lwd = NA) +   # Add labels
  tm_layout(title = "Points on OSM Basemap",
            legend.outside = TRUE)

write_sf(loc_sf,"Data/loc_sf.shp")

rainfall = read.csv("Data/YWS study/rainfall.csv")
rainfall_sf = st_as_sf(rainfall, coords = c("Long","Lat"), crs="EPSG:4326")
plot(rainfall_sf$geometry, col="red")

write_sf(rainfall_sf, "Data/rainfall_sf.shp")


#############################################################################################
# Read in flow data
#############################################################################################
f1 = read.csv("Data/YWS study/F0010.csv", na.strings = "")
f1 = f1[-c(1:2), ]

f2 = read.csv("Data/YWS study/F0012.csv", na.strings = "")
f2 = f2[-c(1:2), ]

f3 = read.csv("Data/YWS study/F0014.csv", na.strings = "")
f3 = f3[-c(1:2), ]

f4 = read.csv("Data/YWS study/F0016.csv", na.strings = "")
f4 = f4[-c(1:2), ]

f5 = read.csv("Data/YWS study/F0022.csv", na.strings = "")
f5 = f5[-c(1:2), ]

f6 = read.csv("Data/YWS study/F0101.csv", na.strings = "")
f6 = f6[-c(1:2), ]


#############################################################################################
# Pre processing the flow data to correct data formats and renaming columns
#############################################################################################
f1 = f1 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0010",
         "f_velocity" = "F0010.1",
         "f_volume" = "F0010.2") 

f2 = f2 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0012",
         "f_velocity" = "F0012.1",
         "f_volume" = "F0012.2") 

f3 = f3 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0014",
         "f_velocity" = "F0014.1",
         "f_volume" = "F0014.2") 

f4 = f4 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0016",
         "f_velocity" = "F0016.1",
         "f_volume" = "F0016.2")

f5 = f5 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0022",
         "f_velocity" = "F0022.1",
         "f_volume" = "F0022.2") 

f6 = f6 %>%
  #mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric) %>%
  rename("Depth" = "F0101",
         "f_velocity" = "F0101.1",
         "f_volume" = "F0101.2") 


#############################################################################################
# Analyze missing value patterns in flow data
#############################################################################################
png("Ranalyses/data cleaning and EDA/f1.png");
f1_m <- DataExplorer::plot_missing(f1)

f1_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f1)

png("Ranalyses/data cleaning and EDA/f2.png");
f2_m <- DataExplorer::plot_missing(f2)

f2_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f2)

png("Ranalyses/data cleaning and EDA/f3.png");
f3_m <- DataExplorer::plot_missing(f3)

f3_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f3)

png("Ranalyses/data cleaning and EDA/f4.png");
f4_m <- DataExplorer::plot_missing(f4)

f4_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f4)

png("Ranalyses/data cleaning and EDA/f5.png");
f5_m <- DataExplorer::plot_missing(f5)

f5_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f5)

png("Ranalyses/data cleaning and EDA/f6.png");
f6_m <- DataExplorer::plot_missing(f6)

f6_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()
plot_pattern(f6)


#############################################################################################
# Read in water quality data, pre prcoessing the data and analyzing missing values 
#############################################################################################
q1 = read.csv("Data/YWS study/1941_S0010_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q1 = q1 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)


png("Ranalyses/data cleaning and EDA/q1.png");
q1_m <- DataExplorer::plot_missing(q1)

q1_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q1))/nrow(q1)*100
plot_pattern(q1)

q2 = read.csv("Data/YWS study/1941_S0014_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q2 = q2 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q2.png");
q2_m <- DataExplorer::plot_missing(q2)

q2_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q2))/nrow(q2)*100
plot_pattern(q2)

q3 = read.csv("Data/YWS study/1941_S0015_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_093019_112000.csv", 
              na.strings = "",header = FALSE)
q3 = q3 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q3.png");
q3_m <- DataExplorer::plot_missing(q3)

q3_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q3))/nrow(q3)*100
plot_pattern(q3)

q4 = read.csv("Data/YWS study/1941_S0016_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q4 = q4 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q4.png");
q4_m <- DataExplorer::plot_missing(q4)

q4_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q4))/nrow(q4)*100
plot_pattern(q4)

q5 = read.csv("Data/YWS study/1941_S0022_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_104500.csv", 
              na.strings = "",header = FALSE)
q5 = q5 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q5.png");
q5_m <- DataExplorer::plot_missing(q5)

q5_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q5))/nrow(q5)*100
plot_pattern(q5)

q6 = read.csv("Data/YWS study/1941_S0024_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q6 = q6 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q6.png");
q6_m <- DataExplorer::plot_missing(q6)

q6_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q6))/nrow(q6)*100
plot_pattern(q6)

q7 = read.csv("Data/YWS study/1941_S0027_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q7 = q7 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q7.png");
q7_m <- DataExplorer::plot_missing(q7)

q7_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q7))/nrow(q7)*100
plot_pattern(q7)

q8 = read.csv("Data/YWS study/1941_S0101_WQ_DO_WQ_NH4_WQ_PH_WQ_TEMP_092319_000000.csv", 
              na.strings = "",header = FALSE)
q8 = q8 %>% select(-c("V6","V7")) %>% 
  rename("date_time"="V1",
         "DO"="V2",
         "NH4"="V3",
         "PH"="V4",
         "Temp"="V5") %>% 
  slice(-(1:3)) #%>%
  #mutate(date_time = as.POSIXct(date_time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  #mutate_if(is.character, as.numeric)

png("Ranalyses/data cleaning and EDA/q8.png");
q8_m <- DataExplorer::plot_missing(q8)

q8_m + theme(
  axis.title.x = element_text(size = 18),  
  axis.title.y = element_text(size = 18),  
  axis.text.x = element_text(size = 18),    
  axis.text.y = element_text(size = 18),
  text = element_text(size = 18)     
)
dev.off()

sum(is.na(q8))/nrow(q8)*100
plot_pattern(q8)


#############################################################################################
# Plots of proportion and pattern of missing values in the data
#############################################################################################

sum(is.na(f1))/nrow(f1)*100
png("Ranalyses/data cleaning and EDA/vim_f1.png", width=1200, height=800);
vim_f1 = aggr(f1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f1), cex.axis=0.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

sum(is.na(f2))/nrow(f2)*100
png("Ranalyses/data cleaning and EDA/vim_f2.png", width=1200, height=800);
vim_f2 = aggr(f2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f2), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

sum(is.na(f3))/nrow(f3)*100
png("Ranalyses/data cleaning and EDA/vim_f3.png", width=1200, height=800);
vim_f3 = aggr(f3, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f3), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

sum(is.na(f4))/nrow(f4)*100
png("Ranalyses/data cleaning and EDA/vim_f4.png", width=1200, height=800);
vim_f4 = aggr(f4, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f4), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

sum(is.na(f5))/nrow(f5)*100
png("Ranalyses/data cleaning and EDA/vim_f5.png", width=1200, height=800);
vim_f5 = aggr(f5, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f5), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

sum(is.na(f6))/nrow(f6)*100
png("Ranalyses/data cleaning and EDA/vim_f6.png", width=1200, height=800);
vim_f6 = aggr(f6, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(f6), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
legend("bottom", legend=c("Not Missing", "Missing"), fill=c("navyblue", "red"), cex=1.4)
dev.off()

#Quality data
png("Ranalyses/data cleaning and EDA/vim_q1.png");
vim_q1 = aggr(q1, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q1), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q2.png");
vim_q2 = aggr(q2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q2), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q3.png");
vim_q3 = aggr(q3, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q3), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q4.png");
vim_q4 = aggr(q4, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q4), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q5.png");
vim_q5 = aggr(q5, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q5), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q6.png");
vim_q6 = aggr(q6, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q6), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q7.png");
vim_q7 = aggr(q7, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q7), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()

png("Ranalyses/data cleaning and EDA/vim_q8.png");
vim_q8 = aggr(q8, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(q8), cex.axis=.9, gap=3, ylab=c("Proportion of missing data","Pattern of missing data"))
dev.off()


#############################################################################################
# Little's MCAR test to check if the values are missing at random (null hyp: values are MCAR)
#############################################################################################
mcar_test(f1)
mcar_test(f2)
mcar_test(f3)
mcar_test(f4)
mcar_test(f5)
mcar_test(f6)

mcar_test(q1)
mcar_test(q2)
mcar_test(q3)
mcar_test(q4)
mcar_test(q5)
mcar_test(q6)
mcar_test(q7)
mcar_test(q8)


#############################################################################################
# Loading rainfall data (YWS) and pre processing it. 
#############################################################################################
r1 = read.csv("Data/YWS study/212R0017.csv")
r1 = r1 %>%
  select(c(1,2)) %>% 
  slice(-(1:3)) %>% 
  rename("Amount" = "X212R0017") %>% 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  mutate_if(is.character, as.numeric)

r2 = read.csv("Data/YWS study/215R0046.csv")
r2 = r2 %>%
  select(c(1,2)) %>% 
  slice(-(1:3)) %>% 
  rename("Amount" = "X215R0046") %>% 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  mutate_if(is.character, as.numeric)

r3 = read.csv("Data/YWS study/215R0052.csv")
r3 = r3 %>%
  select(c(1,2)) %>% 
  slice(-(1:3)) %>% 
  rename("Amount" = "X215R0052") %>% 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  mutate_if(is.character, as.numeric)

r4 = read.csv("Data/YWS study/217R0042.csv")
r4 = r4 %>%
  select(c(1,2)) %>% 
  slice(-(1:3)) %>% 
  rename("Amount" = "X217R0042") %>% 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  mutate_if(is.character, as.numeric)

r5 = read.csv("Data/YWS study/219R0034.csv")
r5 = r5 %>%
  select(c(1,2)) %>% 
  slice(-(1:3)) %>% 
  rename("Amount" = "X219R0034") %>% 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M", tz="GMT")) %>%
  mutate_if(is.character, as.numeric)


#############################################################################################
# Rainfall Data EDA plots 
#############################################################################################
r1_p = ggplot(r1, aes(x = Time, y = Amount)) +
  geom_line(color = "black") +
  geom_point(data = subset(r1, Amount > 0), aes(x = Time, y = Amount), color = "red", size = 0.001) +
  labs(title = "", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()


r2_p = ggplot(r2, aes(x = Time, y = Amount)) +
  geom_line(color = "black") +
  geom_point(data = subset(r2, Amount > 0), aes(x = Time, y = Amount), color = "red", size = 0.001) +
  labs(title = "", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

r3_p = ggplot(r3, aes(x = Time, y = Amount)) +
  geom_line(color = "black") +
  geom_point(data = subset(r3, Amount > 0), aes(x = Time, y = Amount), color = "red", size = 0.001) +
  labs(title = "", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

r4_p = ggplot(r4, aes(x = Time, y = Amount)) +
  geom_line(color = "black") +
  geom_point(data = subset(r4, Amount > 0), aes(x = Time, y = Amount), color = "red", size = 0.001) +
  labs(title = "", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

r5_p = ggplot(r5, aes(x = Time, y = Amount)) +
  geom_line(color = "black") +
  geom_point(data = subset(r5, Amount > 0), aes(x = Time, y = Amount), color = "red", size = 0.001) +
  labs(title = "", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

ggsave("Ranalyses/data cleaning and EDA/r1_p.png",r1_p)
ggsave("Ranalyses/data cleaning and EDA/r2_p.png",r2_p)
ggsave("Ranalyses/data cleaning and EDA/r3_p.png",r3_p)
ggsave("Ranalyses/data cleaning and EDA/r4_p.png",r4_p)
ggsave("Ranalyses/data cleaning and EDA/r5_p.png",r5_p)


#############################################################################################
# Saving the pre processed flow and quality datasets for imputation
#############################################################################################
write.csv(f1,"Pythonanalyses/KNN imputation/data/f1_miss.csv", row.names = FALSE)
write.csv(f2,"Pythonanalyses/KNN imputation/data/f2_miss.csv", row.names = FALSE)
write.csv(f3,"Pythonanalyses/KNN imputation/data/f3_miss.csv", row.names = FALSE)
write.csv(f4,"Pythonanalyses/KNN imputation/data/f4_miss.csv", row.names = FALSE)
write.csv(f5,"Pythonanalyses/KNN imputation/data/f5_miss.csv", row.names = FALSE)
write.csv(f6,"Pythonanalyses/KNN imputation/data/f6_miss.csv", row.names = FALSE)

write.csv(q1,"Pythonanalyses/KNN imputation/data/q1_miss.csv", row.names = FALSE)
write.csv(q2,"Pythonanalyses/KNN imputation/data/q2_miss.csv", row.names = FALSE)
write.csv(q3,"Pythonanalyses/KNN imputation/data/q3_miss.csv", row.names = FALSE)
write.csv(q4,"Pythonanalyses/KNN imputation/data/q4_miss.csv", row.names = FALSE)
write.csv(q5,"Pythonanalyses/KNN imputation/data/q5_miss.csv", row.names = FALSE)
write.csv(q6,"Pythonanalyses/KNN imputation/data/q6_miss.csv", row.names = FALSE)
write.csv(q7,"Pythonanalyses/KNN imputation/data/q7_miss.csv", row.names = FALSE)
write.csv(q8,"Pythonanalyses/KNN imputation/data/q8_miss.csv", row.names = FALSE)




#############################################################################################
# Missing value imputation and EDA plots for flow and quality data are 
# carried out in Python due to computation time issues in R.
#############################################################################################

