#############################################################################################
# Loading the required libraries
#############################################################################################
library(ggplot2)
library(sf)
library(dplyr)
library(gridExtra)
library(ggrepel)
library(openxlsx)


#############################################################################################
# Loading the water quality datasets, converting to date time and labelling the datasets
#############################################################################################
q1 <- read.csv("Pythonanalyses/KNN imputation/data/q1_knn.csv")
q2 <- read.csv("Pythonanalyses/KNN imputation/data/q2_knn.csv")
q3 <- read.csv("Pythonanalyses/KNN imputation/data/q3_knn.csv")
q4 <- read.csv("Pythonanalyses/KNN imputation/data/q4_knn.csv")
q5 <- read.csv("Pythonanalyses/KNN imputation/data/q5_knn.csv")
q6 <- read.csv("Pythonanalyses/KNN imputation/data/q6_knn.csv")
q7 <- read.csv("Pythonanalyses/KNN imputation/data/q7_knn.csv")
q8 <- read.csv("Pythonanalyses/KNN imputation/data/q8_knn.csv")

q1$date_time <- as.POSIXct(q1$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q2$date_time <- as.POSIXct(q2$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q3$date_time <- as.POSIXct(q3$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q4$date_time <- as.POSIXct(q4$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q5$date_time <- as.POSIXct(q5$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q6$date_time <- as.POSIXct(q6$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q7$date_time <- as.POSIXct(q7$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q8$date_time <- as.POSIXct(q8$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

q1$dataset <- "q1"
q2$dataset <- "q2"
q3$dataset <- "q3"
q4$dataset <- "q4"
q5$dataset <- "q5"
q6$dataset <- "q6"
q7$dataset <- "q7"
q8$dataset <- "q8"

f3 = read.csv("Pythonanalyses/KNN imputation/data/f3_knn.csv")
f4 = read.csv("Pythonanalyses/KNN imputation/data/f4_knn.csv")
f5 = read.csv("Pythonanalyses/KNN imputation/data/f5_knn.csv")
f6 = read.csv("Pythonanalyses/KNN imputation/data/f6_knn.csv")


f3$Time <- as.POSIXct(f3$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f4$Time <- as.POSIXct(f4$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f5$Time <- as.POSIXct(f5$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f6$Time <- as.POSIXct(f6$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


f3_rn = read.csv("Ranalyses/radar rainfall/data/rain_f3.csv")
f4_rn = read.csv("Ranalyses/radar rainfall/data/rain_f4.csv")
f5_rn = read.csv("Ranalyses/radar rainfall/data/rain_f5.csv")
f6_rn = read.csv("Ranalyses/radar rainfall/data/rain_f6.csv")


f3_rn$time <- as.POSIXct(f3_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f4_rn$time <- as.POSIXct(f4_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f5_rn$time <- as.POSIXct(f5_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f6_rn$time <- as.POSIXct(f6_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


#############################################################################################
# Converting TAN to NH3
#############################################################################################
nh3_from_TAN <- function(TAN, temp, pH){
    T = 273.15+temp
    pKa = 0.09018 + 2727.92/T
    f = 1/(10^(pKa-pH)+1)
    return (f*TAN)
}

#############################################################################################
# getting locations of the flow, quality and rainfall monitoring sites and visualizing it
#############################################################################################
loc = read.xlsx("Data/YWS study/YWS Monitoring Locations with RH edits.xlsx")

loc_sf = st_as_sf(loc, coords = c("Longitude","Latitude"), crs = "EPSG:4326")
loc_sf = st_transform(loc_sf,crs="EPSG:4326")
loc_sf = loc_sf %>%
            filter(Monitoring.type=="Water quality" & IETG.code %in% c("S0014","S0015","S0016","S0022"))


#############################################################################################
# Parameters to be defined
#############################################################################################
th = 0.75     #threshold
e_date = c("2020-02-02")   #quality event dates


#############################################################################################
# Filtering the events for the selected date(s)
#############################################################################################
evq1 <- q1 %>% 
   filter(as.Date(date_time) %in% e_date)

evq2 <- q2 %>% 
   filter(as.Date(date_time) %in% e_date)

evq3 <- q3 %>% 
   filter(as.Date(date_time) %in% e_date)

evq4 <- q4 %>% 
   filter(as.Date(date_time) %in% e_date)

evq5 <- q5 %>% 
   filter(as.Date(date_time) %in% e_date )

evq6 <- q6 %>% 
   filter(as.Date(date_time) %in% e_date )

evq7 <- q7 %>% 
   filter(as.Date(date_time) %in% e_date)

evq8 <- q8 %>% 
   filter(as.Date(date_time) %in% e_date )


#############################################################################################
# Frequency of quality events on the given date(s) for each site
#############################################################################################
combined_data <- bind_rows(evq1, evq2, evq3, evq4, evq5, evq6, evq7, evq8)

freq_quality = combined_data%>%
filter(NH4>th) %>%
group_by(dataset)%>%
summarize(n=n())

print(freq_quality)
write.csv(freq_quality, "Ranalyses/event analysis/freq_events.csv", row.names = FALSE)

sum(q2$NH4>th)/nrow(q2)
sum(q3$NH4>th)/nrow(q3)
sum(q4$NH4>th)/nrow(q4)
sum(q5$NH4>th)/nrow(q5)

'''# Plot all datasets in separate facets
plt = ggplot(combined_data, aes(x = date_time, y = NH4)) +
  geom_line(size = 1) +
  labs(title = "TAN Over Time",
       x = "Date Time",
       y = "TAN Concentration")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~dataset, scales = "free_x")

ggsave("wq_event.png", plot = plt, width = 10, height = 6, dpi = 600)'''


#############################################################################################
# plots of wq events for each monitoring site
#############################################################################################
plot_single_wq <- function(data) {
  p <- ggplot(data, aes(x = date_time, y = NH4)) +
    geom_line(color = "black", size = 1) +
    geom_hline(aes(yintercept = th, color = "threshold"), linetype = "dashed", size = 1) +  # Add horizontal dashed red line
    labs(
      title = '',   # Title of the plot
      x = "Date Time",
      y = "TAN Concentration",
      color = "Legend"  # Legend title
    ) +
    scale_color_manual(values = c(threshold = "red")) +  # Set color for the threshold line
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),   # Adjust x-axis text size
      axis.text.y = element_text(size = 16),                           # Adjust y-axis text size
      axis.title.x = element_text(size = 18),                         # Adjust x-axis title size
      axis.title.y = element_text(size = 18),                         # Adjust y-axis title size
      plot.title = element_text(size = 20)                            # Adjust plot title size
    )

  return(p)  # Return the plot
}

pevq1 = plot_single_wq(evq1)
pevq2 = plot_single_wq(evq2)
pevq3 = plot_single_wq(evq3)
pevq4 = plot_single_wq(evq4)
pevq5 = plot_single_wq(evq5)
pevq6 = plot_single_wq(evq6)
pevq7 = plot_single_wq(evq7)
pevq8 = plot_single_wq(evq8)

ggsave("Ranalyses/event analysis/plots/evq1.png", plot = pevq1, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq2.png", plot = pevq2, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq3.png", plot = pevq3, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq4.png", plot = pevq4, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq5.png", plot = pevq5, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq6.png", plot = pevq6, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq7.png", plot = pevq7, width = 10, height = 6, dpi = 600)
ggsave("Ranalyses/event analysis/plots/evq8.png", plot = pevq8, width = 10, height = 6, dpi = 600)

# Display all eight plots in a 2x4 grid
png("Ranalyses/event analysis/plots/combined_event.png");
com = grid.arrange(pevq1, pevq2, pevq3, pevq4, pevq5, pevq6, pevq7, pevq8, nrow = 2, ncol = 4)
dev.off()
plot

#############################################################################################
# Loading the dataset containing the lengths of segments between each monitoring site
#############################################################################################
lengths = read.csv("QGIS/consolidated map/lengths.csv")
dist = read_sf("QGIS/consolidated map/dist_final.gpkg")



#temp
e_date = c("2020-05-09","2020-05-10")   #quality event dates

evq1 <- q1 %>% 
   filter(as.Date(date_time) %in% e_date)

evq2 <- q2 %>% 
   filter(as.Date(date_time) %in% e_date)

evq3 <- q3 %>% 
   filter(as.Date(date_time) %in% e_date)

evq4 <- q4 %>% 
   filter(as.Date(date_time) %in% e_date)

evq5 <- q5 %>% 
   filter(as.Date(date_time) %in% e_date )

evq6 <- q6 %>% 
   filter(as.Date(date_time) %in% e_date )

evq7 <- q7 %>% 
   filter(as.Date(date_time) %in% e_date)

evq8 <- q8 %>% 
   filter(as.Date(date_time) %in% e_date )


#############################################################################################
# Calculating travel time of the quality peak for each monitoring site
# q1 - S0010
# q2 - S0014
# q3 - S0015
# q4 - S0016
# q5 - S0022
# q6 - S0024
# q7 - S0027
# q8 - S0101
#############################################################################################
peak_q1 = evq1$date_time[which.max(evq1$NH4)]
peak_q8 = evq8$date_time[which.max(evq8$NH4)]
peak_q2 = evq2$date_time[which.max(evq2$NH4)]
peak_q3 = evq3$date_time[which.max(evq3$NH4)]
peak_q4 = evq4$date_time[which.max(evq4$NH4)]
peak_q5 = evq5$date_time[which.max(evq5$NH4)]
peak_q6 = evq6$date_time[which.max(evq6$NH4)]
peak_q7 = evq7$date_time[which.max(evq7$NH4)]

#loc_sf$peak_time = list(as.character(peak_q2),as.character(peak_q3),as.character(peak_q4),as.character(peak_q5))

t_1_8 = as.integer(difftime(peak_q8,peak_q1, units="sec"))
t_8_2 = as.integer(difftime(peak_q2,peak_q8, units="sec"))
t_2_3 = as.integer(difftime(peak_q3,peak_q2, units="sec"))
t_3_4 = as.integer(difftime(peak_q4,peak_q3, units="sec"))
t_4_5 = as.integer(difftime(peak_q5,peak_q4, units="sec"))
t_5_6 = as.integer(difftime(peak_q6,peak_q5, units="sec"))
t_7_6 = as.integer(difftime(peak_q6,peak_q7, units="sec"))

lengths$transit_time = list(t_2_3,t_3_4,t_4_5,t_5_6)

lengths = as.data.frame(lengths)
lengths$Length..in.m. <- as.numeric(lengths$Length..in.m.)
lengths$transit_time <- as.numeric(lengths$transit_time)

write.csv(lengths, "Ranalyses/event analysis/lengths_with_transit_times_and_velocity.csv")

#############################################################################################
# Visualizing transit time, lengths and velocity on a map
#############################################################################################
dist$centroid <- st_centroid(dist$geom)

linestring_df <- st_coordinates(dist$centroid) %>%
  as.data.frame() %>%
  cbind(dist)  

point_df <- st_coordinates(loc_sf) %>%
  as.data.frame() %>%
  cbind(loc_sf)  

sum_plot <- ggplot() +
  geom_sf(data = dist, aes(geometry = geom), color = "blue", size = 1) +  # Plot linestrings
  geom_label_repel(
    data = linestring_df, 
    aes(
      X, Y, 
      label = paste("Sites:", ID, "\nLength:", Length, "\nTime:", transit_time, "\nVelocity:", velocity),
      nudge_x = ifelse(ID %in% c(1, 2, 3), -0.1, 0)  # Conditional nudge left for IDs 1, 2, 3
    ), 
    size = 1.5, color = "black", fill = "white",  # Use geom_label_repel for boxed text
    box.padding = 0.5,   # Increased padding for better placement
    point.padding = 0.2, # Adjusted padding to ensure lines touch geometries
    nudge_y = 0.02,      # General nudge up
    max.overlaps = Inf,   
    segment.size = 0.5,  
    force = 10,           # Significantly increased force to spread out labels
    min.segment.length = 0.3 # Minimum segment length before it disappears
  ) +  
  geom_sf(data = loc_sf, aes(geometry = geometry), color = "red", size = 1) +  # Plot points
  geom_label_repel(
    data = point_df, 
    aes(X, Y, label = paste("Site", IETG.code, "\nPeak time:", peak_time)), 
    color = "red", fill = "white", size = 1.5,  # Use geom_label_repel for boxed text
    box.padding = 0.5,    
    point.padding = 0.2,  
    nudge_y = 0.02,       
    nudge_x = 0.02,       
    max.overlaps = Inf,   
    segment.size = 0.5,   
    force = 10,           # Significantly increased force to spread out labels
    min.segment.length = 0.3 # Minimum segment length before it disappears
  ) +
  labs(title = "Summary of lengths, transit times, and velocity.")
print(sum_plot)

ggsave("Ranalyses/event analysis/plots/summary_plot.png", plot = sum_plot, width = 10, height = 6, dpi = 600)


#############################################################################################
# Rainfall and flow for event day
#############################################################################################
dt = "2020-06-03"

f3_rn[as.Date(f3_rn$time) == as.Date(dt), ]$rainfall
f4_rn[as.Date(f4_rn$time) == as.Date(dt), ]$rainfall
f5_rn[as.Date(f5_rn$time) == as.Date(dt), ]$rainfall

mean(f3[as.Date(f3$Time) == as.Date(dt), ]$f_volume)
mean(f4[as.Date(f4$Time) == as.Date(dt), ]$f_volume)
mean(f5[as.Date(f5$Time) == as.Date(dt), ]$f_volume)

mean(q2[as.Date(q2$date_time) == as.Date(dt), ]$NH3)
mean(q3[as.Date(q3$date_time) == as.Date(dt), ]$NH3)
mean(q4[as.Date(q4$date_time) == as.Date(dt), ]$NH3)
mean(q5[as.Date(q5$date_time) == as.Date(dt), ]$NH3)

