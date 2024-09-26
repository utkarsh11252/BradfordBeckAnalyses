#############################################################################################
# Loading the required libraries
#############################################################################################
library(ggplot2)
library(dplyr)
library(purrr)
library(lubridate)
library(viridis)

#############################################################################################
# Parameters to be defined
#############################################################################################
th = 0.75     #threshold


#############################################################################################
# Loading the water quality datasets
#############################################################################################
q1 <- read.csv("Pythonanalyses/KNN imputation/data/q1_knn.csv")
q2 <- read.csv("Pythonanalyses/KNN imputation/data/q2_knn.csv")
q3 <- read.csv("Pythonanalyses/KNN imputation/data/q3_knn.csv")
q4 <- read.csv("Pythonanalyses/KNN imputation/data/q4_knn.csv")
q5 <- read.csv("Pythonanalyses/KNN imputation/data/q5_knn.csv")
q6 <- read.csv("Pythonanalyses/KNN imputation/data/q6_knn.csv")
q7 <- read.csv("Pythonanalyses/KNN imputation/data/q7_knn.csv")
q8 <- read.csv("Pythonanalyses/KNN imputation/data/q8_knn.csv")


#############################################################################################
# Plotting TAN with the required threshold value
#############################################################################################
plot_NH4_with_threshold <- function(data, datetime_col, nh4_col, threshold = th) {
  
  data[[datetime_col]] <- as.POSIXct(data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Plot using ggplot2
  ggplot(data, aes_string(x = datetime_col, y = nh4_col)) +
    geom_line(color = "black") +  # Line plot for NH4 values
    geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +  # Threshold line
    labs(title = paste("NH4 Concentration Over Time (Threshold:", threshold, ")"),
         x = "Date Time",
         y = "NH4 Concentration (mg/L)") +
    theme_minimal()
}

plot_NH4_with_threshold(q1,"date_time","NH4")


#############################################################################################
# Converting TAN to NH3
#############################################################################################
nh3_from_TAN <- function(TAN, temp, pH){
    T = 273.15+temp
    pKa = 0.09018 + 2727.92/T
    f = 1/(10^(pKa-pH)+1)
    return (f*TAN)
}

q1$NH3 = mapply(nh3_from_TAN, TAN = q1$NH4,temp= q1$Temp,pH = q1$PH)
plot_NH4_with_threshold(q1,"date_time","NH3", threshold = 0.04)



#############################################################################################
# Proportion of points greater than the threshold
#############################################################################################
nrow(q1[q1$NH4 > 0.75, ])/nrow(q1)
nrow(q2[q2$NH3 > 0.04, ])/nrow(q2)


#############################################################################################
# Converting to date time format and labelling the datasets
#############################################################################################
q1$date_time <- as.POSIXct(q1$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q2$date_time <- as.POSIXct(q2$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q3$date_time <- as.POSIXct(q3$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q4$date_time <- as.POSIXct(q4$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q5$date_time <- as.POSIXct(q5$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q6$date_time <- as.POSIXct(q6$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q7$date_time <- as.POSIXct(q7$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
q8$date_time <- as.POSIXct(q8$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


#############################################################################################
# Filtering water quality events based on threshold and grouping them by day
#############################################################################################
wq_events <- function(q, threshold = th) {
  # Filter for NH4 values greater than 0.75
  filtered_data <- subset(q, NH4 > threshold)
  
  # Extract the date part and group by date
  filtered_data <- filtered_data %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarize(count = n())
  
  # Create the plot

plot <- ggplot(filtered_data, aes(x = date, y = count)) +
  geom_col(fill = "blue", color = "black") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", 
               limits = as.Date(c("2019-09-23", "2020-10-07"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 19),
        axis.text.y = element_text(size = 19),
        axis.title.x = element_text(size = 21),
        axis.title.y = element_text(size = 21),
        plot.title = element_text(size = 23)) +
  labs(title = "Occurrences of NH3 > 0.04 Per Day", 
       x = "Date", 
       y = "Count")
  # Return the filtered data and the plot
  return(list(filtered_data = filtered_data, plot = plot))
}

e1 = wq_events(q1)$filtered_data
e8 = wq_events(q8)$filtered_data

e2 = wq_events(q2)$filtered_data
e3 = wq_events(q3)$filtered_data

e4 = wq_events(q4)$filtered_data

e5 = wq_events(q5)$filtered_data

e6 = wq_events(q6)$filtered_data

e7 = wq_events(q7)$filtered_data


#############################################################################################
# Finding occurences on each date
#############################################################################################
# Function to count the occurrences of each date across multiple data frames
count_date_occurrences <- function(dfs) {
  combined_df = bind_rows(dfs)
  combined_df %>%
    group_by(date) %>%
    summarize(n=n())
}

counts = count_date_occurrences(list(e1,e2,e3,e4,e5,e6,e7,e8))
head(counts)

# Create a month and day columns
counts$month <- month(counts$date)
counts$day <- day(counts$date)

# Heatmap with ggplot2
cc = ggplot(counts, aes(x = day, y = month, fill = n)) +
  geom_tile() +
  scale_fill_gradient(
    low = "lightblue",       # Color for the lowest value
    high = "black",          # Color for the highest value
    n.breaks = 7             # Number of discrete color breaks to ensure a gradual transition
  ) +
  labs(title = "Number of sites for which threshold was exceeded",
       x = "Day of Month",
       y = "Month",
       fill = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size =22, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 17),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
cc  
ggsave("Ranalyses/event analysis/plots/count_sites.png", plot = cc, width = 14, height = 10, dpi = 600)



#############################################################################################
# Saving the plots for significant quality events by day
#############################################################################################
ggsave("Ranalyses/event analysis/plots/e1.png", plot = wq_events(q1)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e2.png", plot = wq_events(q2)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e3.png", plot = wq_events(q3)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e4.png", plot = wq_events(q4)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e5.png", plot = wq_events(q5)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e6.png", plot = wq_events(q6)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e7.png", plot = wq_events(q7)$plot, width = 14, height = 10, dpi = 600)
ggsave("Ranalyses/event analysis/plots/e8.png", plot = wq_events(q8)$plot, width = 14, height = 10, dpi = 600)


#############################################################################################
# Function to find common events for the given monitoring sites
#############################################################################################
find_common_events <- function(data_frames) {
  # Check if the input is a list
  if (!is.list(data_frames) || length(data_frames) < 2) {
    stop("Input should be a list of at least two data frames.")
  }
  
  # Start with the first data frame in the list
  common_events <- data_frames[[1]]
  
  # Sequentially merge with each subsequent data frame
  for (i in 2:length(data_frames)) {
    common_events <- merge(common_events, data_frames[[i]], by = "date")
  }
  
  # Return the resulting data frame with common dates
  return(common_events)
}

find_common_events(list(e2,e3,e4,e5,e6))

find_common_events(list(e3,e2,e4))





