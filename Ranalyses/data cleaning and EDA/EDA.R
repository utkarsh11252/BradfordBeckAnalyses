#############################################################################################
# Loading the required libraries
#############################################################################################
library(ggplot2)
library(reshape2)
library(sf)
library(dplyr)
library(gridExtra)
library(RColorBrewer)

#############################################################################################
# Loading the datasets
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


#############################################################################################
# Rainfall vs flow
#############################################################################################
plot_volume_vs_rainfall <- function(f1_list, f1_rn_list, site_names) {
  start_date <- as.POSIXct("2019-09-23 00:00:00", tz = "UTC")
  end_date <- as.POSIXct("2020-10-08 23:59:59", tz = "UTC")

  # Ensure that the length of site_names matches the length of f1_list and f1_rn_list
  if (length(f1_list) != length(f1_rn_list) || length(f1_list) != length(site_names)) {
    stop("Length of f1_list, f1_rn_list, and site_names must be the same")
  }
  
  # Initialize an empty list to store combined datasets
  combined_data_list <- list()
  
  # Process each dataset and combine
  for (i in seq_along(f1_list)) {
    # Prepare the rainfall data with the correct time format and date range
    f1_rain <- f1_rn_list[[i]] %>%
      select(c("time", "rainfall")) %>%
      mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
      filter(time >= start_date & time <= end_date) %>%
      mutate(date = as.Date(time))
    
    # Calculate the daily average of f_volume from f1
    daily_avg <- f1_list[[i]] %>%
      mutate(date = as.Date(Time)) %>%
      group_by(date) %>%
      summarise(avg_volume = mean(f_volume, na.rm = TRUE))
    
    # Merge the f1_rain and daily_avg dataframes on the date column
    merged_data <- merge(f1_rain, daily_avg, by = "date")

    # Add a site identifier with custom names
    merged_data$site <- site_names[i]
    
    # Append to combined data list
    combined_data_list[[i]] <- merged_data
  }
  
  # Combine all sites into one dataframe
  all_combined_data <- bind_rows(combined_data_list)
  
  # Generate a color palette with a unique color for each site
  unique_sites <- unique(all_combined_data$site)
  color_palette <- brewer.pal(length(unique_sites), "Set1")
  names(color_palette) <- unique_sites
  
  # Create the scatter plot with a fitted line
  plot <- ggplot(all_combined_data, aes(x = rainfall, y = avg_volume)) +
    geom_point(aes(color = site), size = 1.5, alpha = 0.7) +  # Scatter plot with color for site
    geom_smooth(method = "lm", aes(color = site), se = FALSE) +  # Linear fit line with color for site
    facet_wrap(~ site, ncol = 3) +  # Facet by site, 3 columns
    scale_color_manual(values = color_palette) +  # Apply custom color palette
    labs(title = "Plot of Daily Average Flow vs. Rainfall",
         x = "Rainfall Depth (mm)",
         y = "Average Daily Flow (L/s)",
         color = "Site") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, face = "bold"),
      panel.grid.major = element_line(size = 0.5, color = "grey80"),
      panel.grid.minor = element_line(size = 0.25, color = "grey90")
    )
  
  # Display the plot
  print(plot)
  return(plot)
}

f_list <- list(f1,f2,f3,f4,f5,f6)
f_rn_list <- list(f1_rn,f2_rn,f3_rn,f4_rn,f5_rn,f6_rn)
site_names = c("F0010","F0012","F0014","F0016","F0022","F0101")
fr_plot = plot_volume_vs_rainfall(f_list, f_rn_list, site_names)

ggsave("Ranalyses/data cleaning and EDA/plots/fr_plot.png", plot = fr_plot, width = 10, height = 6, dpi = 600)


#############################################################################################
# Annual daily NH4
#############################################################################################
plot_daily_avg_tan <- function(data, time_col, tan_col) {
  
  # Ensure the time column is in POSIXct format
  data <- data %>%
    mutate(!!time_col := as.POSIXct(.data[[time_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  # Calculate daily averages of TAN values
  daily_avg_tan <- data %>%
    mutate(date = as.Date(.data[[time_col]])) %>%
    group_by(date) %>%
    summarise(avg_tan = mean(.data[[tan_col]], na.rm = TRUE))
  
  # Create a data frame for the horizontal lines to include in the legend
  hline_data <- data.frame(
    yintercept = c(0.3, 0.75),
    linetype = c("WFD good status", "WFD moderate status"),
    color = c("darkgreen", "darkorange")
  )
  
  # Create the scatter plot with a line connecting points and horizontal lines with updated legend names
  plot = ggplot(daily_avg_tan, aes(x = date, y = avg_tan)) +
    geom_point(color = "blue", size = 3, alpha = 0.6, aes(color = "Daily average")) +  # Scatter plot
    geom_line(color = "blue", alpha = 0.6, aes(color = "Daily average")) +  # Line connecting points
    geom_smooth(method = "lm", color = "red", se = FALSE, aes(linetype = "Trend in TAN")) +  # Linear fit line
    geom_hline(data = hline_data, aes(yintercept = yintercept, color = linetype), linetype = "dashed", size = 1) +
    scale_color_manual(name = NULL, # No title for color legend
                       values = c("Daily average" = "blue", "WFD good status" = "darkgreen", "WFD moderate status" = "darkorange"),
                       breaks = c("Daily average", "WFD good status", "WFD moderate status"),
                       labels = c("Daily average", "WFD good status", "WFD moderate status")) +
    scale_linetype_manual(name = NULL, # No title for linetype legend
                          values = c("Trend in TAN" = "solid"),
                          breaks = c("Trend in TAN"),
                          labels = c("Trend in TAN")) +
    labs(title = "TAN profile",
         x = "Date",
         y = "Average daily TAN (mg/L)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 17),
      plot.title = element_text(size = 19, face = "bold"),
      panel.grid.major = element_line(size = 0.5, color = "grey80"),
      panel.grid.minor = element_line(size = 0.25, color = "grey90"),
      legend.position = "bottom",
      legend.text = element_text(size = 17), # Increase legend text size
      legend.title = element_blank() # Remove legend title
    ) +
    guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))

  return(plot)
}

q1_tan = plot_daily_avg_tan(q1,"date_time","NH4")
ggsave("Ranalyses/data cleaning and EDA/plots/q1_tan.png", plot = q1_tan, width = 10, height = 6, dpi = 600)

q4_tan = plot_daily_avg_tan(q4,"date_time","NH4")
ggsave("Ranalyses/data cleaning and EDA/plots/q4_tan.png", plot = q4_tan, width = 10, height = 6, dpi = 600)

q5_tan = plot_daily_avg_tan(q5,"date_time","NH4")
ggsave("Ranalyses/data cleaning and EDA/plots/q5_tan.png", plot = q5_tan, width = 10, height = 6, dpi = 600)


#############################################################################################
# Correlation heatmap
#############################################################################################
plot_correlation_heatmap <- function(df, custom_title) {
  
  # 1. Calculate the correlation matrix
  cor_matrix <- cor(df, use = "complete.obs")
  
  # 2. Mask the upper triangle of the correlation matrix
  cor_matrix_upper <- cor_matrix
  cor_matrix_upper[upper.tri(cor_matrix_upper, diag = FALSE)] <- NA
  
  # 3. Melt the correlation matrix into long format
  melted_cor_matrix <- melt(cor_matrix_upper, na.rm = TRUE)
  
  # 4. Plot the correlation heatmap with custom color palette and correlation values
  plot <- ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +  # Create the heatmap tiles
    scale_fill_gradientn(colors = c("darkblue", "white", "darkred"),   # Custom color palette
                         limits = c(-1, 1),                            # Limit color scale to -1 to 1
                         name = "Correlation") +                       # Legend title
    geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 10) +  # Display correlation values
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 27),  # X-axis label size
          axis.text.y = element_text(size = 27),                                   # Y-axis label size
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.title = element_text(size = 29, face = "bold", hjust = 0.5),      # Title size, style, and alignment
          legend.text = element_text(size = 23),                                  # Legend text size
          legend.title = element_text(size = 23)) +                               # Legend title size
    coord_fixed() +                                                     # Ensure tiles are square
    labs(title = custom_title) +                                     # Add custom title
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis text for better readability

  return(plot)
}


q1f1 =  merge(f1, q1, by.x = "Time", by.y = "date_time")
q1_c_f1 = plot_correlation_heatmap(q1f1[,-1],"F0010 and S0010")
ggsave("Ranalyses/data cleaning and EDA/plots/q1_f1.png", plot = q1_c_f1, width = 16, height = 12, dpi = 600)


q2f3 = merge(f3, q2, by.x = "Time", by.y = "date_time")
q2_c_f3 = plot_correlation_heatmap(q2f3[,-1], "F0014 and S0014")
ggsave("Ranalyses/data cleaning and EDA/plots/q2_f3.png", plot = q2_c_f3, width = 16, height = 12, dpi = 600)


q4f4 = merge(f4, q4, by.x = "Time", by.y = "date_time")
q4_c_f4 = plot_correlation_heatmap(q4f4[,-1], "F0016 and S0016")
ggsave("Ranalyses/data cleaning and EDA/plots/q4_f4.png", plot = q4_c_f4, width = 16, height = 12, dpi = 600)


q5f5 = merge(f5, q5, by.x = "Time", by.y = "date_time")
q5_c_f5 = plot_correlation_heatmap(q5f5[,-1], "F0022 and S0022")
ggsave("Ranalyses/data cleaning and EDA/plots/q5_f5.png", plot = q5_c_f5, width = 16, height = 12, dpi = 600)


#############################################################################################
# Rainfall and TAN
#############################################################################################
plot_tan_rainfall_boxplot <- function(q1, f1_rn, custom_title) {
  
  # Define date range
  start_date <- as.POSIXct("2019-09-23 00:00:00", tz = "UTC")
  end_date <- as.POSIXct("2020-10-08 23:59:59", tz = "UTC")
  
  # Process TAN data: Calculate daily average of TAN values
  df_tan <- q1 %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarise(avg_TAN = mean(NH4, na.rm = TRUE))
  
  # Process rainfall data: Convert datetime to date and filter by date range
  df_rain <- f1_rn %>%
    select(c("time", "rainfall")) %>%
    mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    filter(time >= start_date & time <= end_date) %>%
    mutate(date = as.Date(time))
  
  # Merge TAN and rainfall data on date
  merged_data <- merge(df_tan, df_rain, by = "date")
  
  # Define rainfall bins and labels as intervals
  bin_edges <- quantile(merged_data$rainfall, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  bin_labels <- paste0("[", sprintf("%.2f", bin_edges[-length(bin_edges)]), ", ", sprintf("%.2f", bin_edges[-1]), "]")
  
  # Categorize rainfall into bins with interval labels
  merged_data <- merged_data %>%
    mutate(rainfall_category = cut(rainfall, 
                                   breaks = bin_edges,
                                   labels = bin_labels,
                                   include.lowest = TRUE))
  
  # Create the boxplot
  plot <- ggplot(merged_data, aes(x = rainfall_category, y = avg_TAN)) +
    geom_boxplot(aes(fill = rainfall_category), 
                 color = "black", 
                 outlier.colour = "red", 
                 outlier.size = 6, 
                 outlier.shape = 16) +
    scale_fill_brewer(palette = "Set3") + # Use a color palette for better aesthetics
    labs(title = custom_title,
         x = "Rainfall Intervals (mm)",
         y = "Average daily TAN (mg/L)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 31),
          axis.text.y = element_text(size = 31),
          axis.title.x = element_text(size = 31),
          axis.title.y = element_text(size = 31),
          plot.title = element_text(size = 34, hjust = 0.5),
          legend.position = "none") # Hide the legend for this plot
  
  return(plot)
}

q1_rn = plot_tan_rainfall_boxplot(q1,f1_rn,"S0010")
ggsave("Ranalyses/data cleaning and EDA/plots/q1_rn.png", plot = q1_rn, width = 18, height = 14, dpi = 600)

q2_rn = plot_tan_rainfall_boxplot(q2,f3_rn,"S0014")
ggsave("Ranalyses/data cleaning and EDA/plots/q2_rn.png", plot = q2_rn, width = 18, height = 14, dpi = 600)

q4_rn = plot_tan_rainfall_boxplot(q4,f4_rn,"S0016")
ggsave("Ranalyses/data cleaning and EDA/plots/q4_rn.png", plot = q4_rn, width = 18, height = 14, dpi = 600)

q5_rn = plot_tan_rainfall_boxplot(q5,f5_rn,"S0022")
ggsave("Ranalyses/data cleaning and EDA/plots/q5_rn.png", plot = q5_rn, width = 18, height = 14, dpi = 600)


#############################################################################################
# monthly flow plot
#############################################################################################
plot_monthly_f_volume <- function(df,tit) {
  
  # 1. Convert the 'Time' column to Date-Time format
  df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S")
  
  # 2. Extract Year and Month from the 'Time' column
  df <- df %>%
    mutate(Year = year(Time),
           Month = month(Time, label = TRUE, abbr = TRUE))  # Convert month to abbreviated name
  
  # 3. Calculate monthly average and standard deviation of 'f_volume'
  monthly_stats <- df %>%
    group_by(Year, Month) %>%
    summarize(
      avg_volume = mean(f_volume, na.rm = TRUE),
      sd_volume = sd(f_volume, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # 4. Create the plot
  plot <- ggplot(monthly_stats, aes(x = interaction(Year, Month), y = avg_volume)) +
    geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot with a single color
    geom_errorbar(aes(ymin = avg_volume - sd_volume, ymax = avg_volume + sd_volume), width = 0.5) +  # Error bars
    scale_x_discrete(labels = function(x) gsub("^[0-9]+\\.", "", x)) +  # Clean x-axis labels
    labs(
      x = "Month",
      y = "Average flow volume (L/s)",
      title = tit
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 30, angle = 45, hjust = 1),  # Increase size and rotate x-axis labels
      axis.text.y = element_text(size = 30),  # Increase size of y-axis labels
      axis.title.x = element_text(size = 32),  # Increase size of x-axis title
      axis.title.y = element_text(size = 32),  # Increase size of y-axis title
      plot.title = element_text(size = 35, face = "bold", hjust = 0.5)  # Increase size and bold the title
    )
  
  return(plot)
}

f1_mon = plot_monthly_f_volume(f1,"F0010")
ggsave("Ranalyses/data cleaning and EDA/plots/f1_mon.png", plot = f1_mon, width = 20, height = 16, dpi = 600)

f2_mon = plot_monthly_f_volume(f2,"F0012")
ggsave("Ranalyses/data cleaning and EDA/plots/f2_mon.png", plot = f2_mon, width = 20, height = 16, dpi = 600)

f3_mon = plot_monthly_f_volume(f3,"F0014")
ggsave("Ranalyses/data cleaning and EDA/plots/f3_mon.png", plot = f3_mon, width = 20, height = 16, dpi = 600)

f4_mon = plot_monthly_f_volume(f4,"F0016")
ggsave("Ranalyses/data cleaning and EDA/plots/f4_mon.png", plot = f4_mon, width = 20, height = 16, dpi = 600)

f5_mon = plot_monthly_f_volume(f5,"F0022")
ggsave("Ranalyses/data cleaning and EDA/plots/f5_mon.png", plot = f5_mon, width = 20, height = 16, dpi = 600)

f6_mon = plot_monthly_f_volume(f6,"F0101")
ggsave("Ranalyses/data cleaning and EDA/plots/f6_mon.png", plot = f6_mon, width = 18, height = 14, dpi = 600)


#############################################################################################
# Q-C plot
#############################################################################################
plot_qc <- function(flow_df, quality_df) {
  
  # 1. Convert 'Time' and 'date_time' columns to POSIXct format
  flow_df$Time <- as.POSIXct(flow_df$Time, format = "%Y-%m-%d %H:%M:%S")
  quality_df$date_time <- as.POSIXct(quality_df$date_time, format = "%Y-%m-%d %H:%M:%S")
  
  # 2. Merge datasets on the time column
  merged_df <- flow_df %>%
    rename(Time = Time) %>%
    inner_join(quality_df %>% rename(Time = date_time), by = "Time")
  
  # 3. Calculate daily averages of f_volume and NH4
  daily_avg_df <- merged_df %>%
    mutate(Date = as.Date(Time)) %>%  # Extract the date portion
    group_by(Date) %>%  # Group by date
    summarise(
      avg_f_volume = mean(f_volume, na.rm = TRUE),  # Calculate daily average of f_volume
      avg_NH4 = mean(NH4, na.rm = TRUE)  # Calculate daily average of NH4
    )
  
  # 4. Create the Q-C plot with a trendline and legend
  plot <- ggplot(daily_avg_df, aes(x = avg_f_volume, y = avg_NH4)) +
    geom_point(color = "blue") +  # Scatter plot
    geom_smooth(method = "loess", color = "red", se = FALSE, aes(linetype = "Trendline")) +  # Trendline with legend
    labs(
      x = "Discharge (f_volume)",
      y = "NH4 Concentration",
      title = "Q-C (Discharge-Concentration) Plot",
      linetype = "Legend"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "top"  # Position the legend at the top
    )
  
  return(plot)
}
plot_qc(f4,q4)
