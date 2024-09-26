#############################################################################################
# Loading the required libraries
#############################################################################################
library(dplyr)
library(ggplot2)
library(patchwork)


#############################################################################################
# Importing the flow and rainfall datasets
#############################################################################################
f1 = read.csv("Pythonanalyses/KNN imputation/data/f1_knn.csv")
f2 = read.csv("Pythonanalyses/KNN imputation/data/f2_knn.csv")
f3 = read.csv("Pythonanalyses/KNN imputation/data/f3_knn.csv")
f4 = read.csv("Pythonanalyses/KNN imputation/data/f4_knn.csv")
f5 = read.csv("Pythonanalyses/KNN imputation/data/f5_knn.csv")
f6 = read.csv("Pythonanalyses/KNN imputation/data/f6_knn.csv")

f1$Time <- as.POSIXct(f1$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f2$Time <- as.POSIXct(f2$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f3$Time <- as.POSIXct(f3$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f4$Time <- as.POSIXct(f4$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f5$Time <- as.POSIXct(f5$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f6$Time <- as.POSIXct(f6$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

f1_rn = read.csv("Ranalyses/radar rainfall/data/rain_f1.csv")
f2_rn = read.csv("Ranalyses/radar rainfall/data/rain_f2.csv")
f3_rn = read.csv("Ranalyses/radar rainfall/data/rain_f3.csv")
f4_rn = read.csv("Ranalyses/radar rainfall/data/rain_f4.csv")
f5_rn = read.csv("Ranalyses/radar rainfall/data/rain_f5.csv")
f6_rn = read.csv("Ranalyses/radar rainfall/data/rain_f6.csv")


#############################################################################################
# function ot generate hydrograph by extrapolating rainfall
#############################################################################################
generate_hydrograph <- function(f1, f1_rn) {
  
  # Define fixed start and end dates
  start_date <- as.POSIXct("2019-09-23 00:00:00", tz = "UTC")
  end_date <- as.POSIXct("2020-10-09 23:59:59", tz = "UTC")
  
  # Process rainfall data
  f1_rain <- f1_rn %>%
    select(c("time", "rainfall")) %>%
    mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    filter(time >= start_date & time <= end_date)
  
  # Process flow data
  f1_t <- f1 %>%
    select(c("Time", "f_volume")) %>%
    rename("datetime" = "Time")
  
  # Generate a sequence of 15-minute intervals for the entire period
  time_seq <- seq(from = min(f1$Time), to = max(f1$Time), by = "15 mins")
  
  # Create an empty data frame for 15-minute interval rainfall data
  rainfall_15min <- data.frame(datetime = time_seq, rainfall = 0)
  
  # Function to distribute daily rainfall over 15-minute intervals
  distribute_rainfall <- function(date, amount) {
    # Generate a sequence of 15-minute intervals for the day
    intervals <- seq(from = as.POSIXct(paste(date, "00:00:00")), by = "15 mins", length.out = 96)
    
    # Calculate the rainfall per 15-minute interval
    per_interval <- amount / length(intervals)
    
    # Return a data frame with 15-minute intervals and corresponding rainfall
    return(data.frame(datetime = intervals, rainfall = rep(per_interval, length(intervals))))
  }
  
  # Apply the function to each day in the rainfall data
  rainfall_15min <- do.call(rbind, lapply(1:nrow(f1_rain), function(i) {
    distribute_rainfall(f1_rain$time[i], f1_rain$rainfall[i])
  }))
  
  # Merge the rainfall data with the existing time sequence
  rainfall_15min <- merge(data.frame(datetime = time_seq), rainfall_15min, by = "datetime", all.x = TRUE)
  rainfall_15min$rainfall[is.na(rainfall_15min$rainfall)] <- 0
  
  # Merge the datasets based on datetime
  hydro_data <- merge(f1_t, rainfall_15min, by = "datetime", all = TRUE)
  
  # Replace NA values with 0 in rainfall
  hydro_data$rainfall[is.na(hydro_data$rainfall)] <- 0
 
  # Plot the hydrograph with specified colors
  p <- ggplot(data = hydro_data, aes(x = datetime)) +
  
  # Add the flow line first (so it appears behind the bars)
  geom_line(aes(y = f_volume, color = "Flow"), size = 0.5) +
  
  # Add the inverted rainfall bars
  geom_bar(aes(y = -rainfall * 8000, fill = "Rainfall"), stat = "identity", alpha = 0.5) +
  
  # Scale y-axis for flow and create secondary axis for rainfall
  scale_y_continuous(
    name = "Flow (cumecs)",
    sec.axis = sec_axis(~./-8000, name = "Rainfall (mm)", breaks = seq(0, 45, by = 5))
  ) +
  
  # Duplicate x-axis to top
  scale_x_datetime(
    name = "Date",
    sec.axis = dup_axis(name = "Date")
  ) +
  
  # Set colors for the flow line and rainfall bars
  scale_color_manual(values = c("Flow" = "#ff0800")) +
  scale_fill_manual(values = c("Rainfall" = "#01010d")) +
  
  # Labels and themes
  labs(title = "", x = "Date", color = "", fill = "") +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.text.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.x.top = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    axis.title.x.bottom = element_text(color = "black"),
    axis.text.x.bottom = element_text(color = "black"),
    legend.position = "bottom"
  )

  return(p)
}

hydro_f1 = generate_hydrograph(f1,f1_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f1.png", plot = hydro_f1, width = 10, height = 6, dpi = 600)

hydro_f2 = generate_hydrograph(f2,f2_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f2.png", plot = hydro_f2, width = 10, height = 6, dpi = 600)

hydro_f3 = generate_hydrograph(f3,f3_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f3.png", plot = hydro_f3, width = 10, height = 6, dpi = 600)

hydro_f4 = generate_hydrograph(f4,f4_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f4.png", plot = hydro_f4, width = 10, height = 6, dpi = 600)

hydro_f5 = generate_hydrograph(f5,f5_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f5.png", plot = hydro_f5, width = 10, height = 6, dpi = 600)

hydro_f6 = generate_hydrograph(f6,f6_rn)
ggsave("Ranalyses/flow accumulation/plots/hydro_f6.png", plot = hydro_f6, width = 10, height = 6, dpi = 600)


#############################################################################################
# function to plot hydrograph by taking average daily flow
#############################################################################################
hydrograph_plot <- function(flow_data, rain_data) {
  # Process daily flow data
  daily_flow_data <- flow_data %>%
    # Extract date from datetime
    mutate(date = as.Date(Time)) %>%
    # Group by the extracted date
    group_by(date) %>%
    # Calculate daily average flow
    summarize(daily_avg_flow = mean(f_volume, na.rm = TRUE)) %>%
    # Optional: Arrange by date
    arrange(date)
  
  # Process rainfall data
  rain_data$date <- as.Date(rain_data$time)
  
  # Merge flow and rainfall data
  hydro_data <- merge(rain_data, daily_flow_data, by = "date", all = TRUE)
  
pp <- ggplot(data = hydro_data, aes(x = date)) +
  
  # Inverted rainfall bars, so they appear at the top
  geom_bar(aes(y = -rainfall * 20, fill = "Rainfall"), stat = "identity", alpha = 0.5) +
  
  # Flow line, ensuring it is behind the bars
  geom_line(aes(y = daily_avg_flow, color = "Flow"), size = 1) +
  
  # Scale y-axis for flow and create secondary axis for rainfall
  scale_y_continuous(
    name = "Flow (cumecs)",
    sec.axis = sec_axis(~., name = "Rainfall (mm)", breaks = seq(-45, 0, by = 5), labels = abs)
  ) +
  
  # Duplicate x-axis to top
  scale_x_date(
    name = "Date",
    sec.axis = dup_axis(name = "Date")
  ) +
  
  # Set colors for the plots
  scale_color_manual(values = c("Flow" = "darkorange")) +
  scale_fill_manual(values = c("Rainfall" = "blue")) +
  
  # Labels and themes
  labs(title = "Hydrograph", color = "", fill = "") +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.text.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.x.top = element_text(color = "black"),
    axis.text.x.top = element_text(color = "black"),
    axis.title.x.bottom = element_text(color = "black"),
    axis.text.x.bottom = element_text(color = "black"),
    legend.position = "bottom"
  )

  return(pp)
}

h_f1 = hydrograph_plot(f1,f1_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f1.png", plot = h_f1, width = 10, height = 6, dpi = 600)

h_f2 = hydrograph_plot(f2,f2_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f2.png", plot = h_f2, width = 10, height = 6, dpi = 600)

h_f3 = hydrograph_plot(f3,f3_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f3.png", plot = h_f3, width = 10, height = 6, dpi = 600)

h_f4 = hydrograph_plot(f4,f4_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f4.png", plot = h_f4, width = 10, height = 6, dpi = 600)

h_f5 = hydrograph_plot(f5,f5_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f5.png", plot = h_f5, width = 10, height = 6, dpi = 600)

h_f6 = hydrograph_plot(f6,f6_rn)
ggsave("Ranalyses/flow accumulation/plots/h_f6.png", plot = h_f6, width = 10, height = 6, dpi = 600)


