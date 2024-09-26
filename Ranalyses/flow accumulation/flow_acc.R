#############################################################################################
# importing libraries
#############################################################################################
library(dplyr)
library(ggplot2)
library(lubridate)


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

f1_rn$Time <- as.POSIXct(f1_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f2_rn$Time <- as.POSIXct(f2_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f3_rn$Time <- as.POSIXct(f3_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f4_rn$Time <- as.POSIXct(f4_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f5_rn$Time <- as.POSIXct(f5_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
f6_rn$Time <- as.POSIXct(f6_rn$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


#############################################################################################
# catchment area of each gauge
#############################################################################################
f1_ar = 19.67
f2_ar = 4.54
f3_ar = 6.15
f4_ar = 12.46
f5_ar = 9.5
f6_ar = 19.67


#############################################################################################
# Flow duration curves
#############################################################################################

plot_fdc_ggplot2 <- function(df1, df2, df3, df4, df5, df6) {
  
  # Function to calculate exceedance probability
  calc_fdc <- function(df, site_name) {
    df <- df %>%
      arrange(desc(f_volume)) %>%
      mutate(exceedance_probability = 1:n() / (n() + 1),
             site = site_name)
    return(df)
  }
  
  # Apply the FDC calculation to all dataframes
  fdc1 <- calc_fdc(df1, "F0010")
  fdc2 <- calc_fdc(df2, "F0012")
  fdc3 <- calc_fdc(df3, "F0014")
  fdc4 <- calc_fdc(df4, "F0016")
  fdc5 <- calc_fdc(df5, "F0022")
  fdc6 <- calc_fdc(df6, "F0101")
  
  # Combine the dataframes into a single dataframe
  combined_df <- bind_rows(fdc1, fdc2, fdc3, fdc4, fdc5, fdc6)
  print(max(combined_df$f_volume))  
  
  # Scale the flow values
  combined_df$f_volume <- combined_df$f_volume / max(combined_df$f_volume)
  
  # Create the ggplot2 plot
  p = ggplot(combined_df, aes(x = exceedance_probability, y = f_volume, color = site)) +
    geom_line(size = 0.5, linetype = "dashed") +  # Set line size and linetype here
    geom_point(size = 1) +  # Smaller point size to match thinner lines
    scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown")) +
    labs(title = "Flow Duration Curves for 6 flow sites", 
         x = "Exceedance Probability", 
         y = "Scaled Flow Volume") +
    theme_minimal(base_size = 19) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 22),    # Increase size of legend text
      legend.position = "right",
      axis.title.x = element_text(size = 26),   # Increase size of x-axis title
      axis.title.y = element_text(size = 26),   # Increase size of y-axis title
      axis.text.x = element_text(size = 22),    # Increase size of x-axis labels
      axis.text.y = element_text(size = 22),    # Increase size of y-axis labels
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5)  # Increase size and boldness of plot title
    )
  
  return(p)
}

p = plot_fdc_ggplot2(f1,f2,f3,f4,f5,f6)
ggsave("Ranalyses/flow accumulation/plots/temp.png", plot = p, width = 13, height = 9, dpi = 600)
plot(p)


#############################################################################################
# cumulative flow for each site
#############################################################################################
calculate_cumulative_flow <- function(f1, f2, f3, f4, f5, f6, flow_column = "f_volume", datetime_column = "Time") {
  
  # Initialize cumulative flow for each site
  cumulative_flow_f1 <- f1[[flow_column]]  # Starting point for f1
  cumulative_flow_f2 <- f2[[flow_column]]  # Tributary f2 has its original flow
  
  # Calculate cumulative flow for f6 as f1 + f6
  cumulative_flow_f6 <- cumulative_flow_f1 + f6[[flow_column]]
  
  # Calculate cumulative flow for f3 as f1 + f6 + f2 + f3
  cumulative_flow_f3 <- cumulative_flow_f6 + cumulative_flow_f2 + f3[[flow_column]]
  
  # Calculate cumulative flow for f4 as f1 + f6 + f2 + f3 + f4
  cumulative_flow_f4 <- cumulative_flow_f3 + f4[[flow_column]]
  
  # Calculate cumulative flow for f5 as f1 + f6 + f2 + f3 + f4 + f5
  cumulative_flow_f5 <- cumulative_flow_f4 + f5[[flow_column]]
  
  # Create data frames with datetime and cumulative flow for each site
  cumulative_flow_dfs <- list(
    cumulative_flow_site1 = data.frame(
      datetime = f1[[datetime_column]],
      Site = "Site1",
      CumulativeFlow = cumulative_flow_f1
    ),
    cumulative_flow_site2 = data.frame(
      datetime = f2[[datetime_column]],
      Site = "Site2",
      CumulativeFlow = cumulative_flow_f2
    ),
    cumulative_flow_site6 = data.frame(
      datetime = f6[[datetime_column]],
      Site = "Site6",
      CumulativeFlow = cumulative_flow_f6
    ),
    cumulative_flow_site3 = data.frame(
      datetime = f3[[datetime_column]],
      Site = "Site3",
      CumulativeFlow = cumulative_flow_f3
    ),
    cumulative_flow_site4 = data.frame(
      datetime = f4[[datetime_column]],
      Site = "Site4",
      CumulativeFlow = cumulative_flow_f4
    ),
    cumulative_flow_site5 = data.frame(
      datetime = f5[[datetime_column]],
      Site = "Site5",
      CumulativeFlow = cumulative_flow_f5
    )
  )
  
  # Return the list of cumulative flow data frames
  return(cumulative_flow_dfs)
}

cumulative_flows <- calculate_cumulative_flow(f1, f2, f3, f4, f5, f6)

# Accessing each cumulative flow data frame
cum_f1 = cumulative_flows$cumulative_flow_site1
cum_f2 = cumulative_flows$cumulative_flow_site2  # Tributary
cum_f3 = cumulative_flows$cumulative_flow_site3
cum_f4 = cumulative_flows$cumulative_flow_site4
cum_f5 = cumulative_flows$cumulative_flow_site5
cum_f6 = cumulative_flows$cumulative_flow_site6

plot_cumulative_flow_multiple_sites <- function(df_list) {
  # Ensure df_list is a list of data frames
  if (!all(sapply(df_list, is.data.frame))) {
    stop("All elements in df_list must be data frames")
  }
  
  # Define custom legend labels inside the function
  site_labels <- c("F0010", "F0012", "F0014", "F0016", "F0022", "F0101")
  
  # Check that the number of labels matches the number of data frames
  if (length(site_labels) != length(df_list)) {
    stop("The number of custom legend labels must match the number of data frames")
  }
  
  # Add a Site identifier to each data frame and combine them
  combined_df <- do.call(rbind, lapply(seq_along(df_list), function(i) {
    df <- df_list[[i]]
    df$Site <- site_labels[i]  # Use the custom Site identifier
    return(df)
  }))
  
  # Ensure datetime is in POSIXct format
  combined_df$datetime <- as.POSIXct(combined_df$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  # Plot using ggplot2
  p <- ggplot(combined_df, aes(x = datetime, y = CumulativeFlow, color = Site)) +
    geom_line(size = 1) +  # Add lines for each site
    labs(
      title = "Cumulative Flow Over Time for all Sites",
      x = "Date",
      y = "Cumulative Flow (m3/s)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",  # Position legend on the right
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  # Increase x-axis label size and rotate
      axis.text.y = element_text(size = 15),  # Increase y-axis label size
      axis.title.x = element_text(size = 17),  # Increase x-axis title size
      axis.title.y = element_text(size = 17),  # Increase y-axis title size
      legend.text = element_text(size = 15),  # Increase legend text size
      legend.title = element_text(size = 17),  # Increase legend title size
      plot.title = element_text(size = 19, face = "bold"),  # Increase plot title size and make it bold
      panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    scale_color_manual(
      values = rainbow(length(site_labels)),  # Distinct colors for each site
      labels = site_labels  # Custom labels for the legend
    )
  
  return(p)
}
cum_flow_plot = plot_cumulative_flow_multiple_sites(list(cum_f1,cum_f2,cum_f3,cum_f4,cum_f5,cum_f6))
ggsave("Ranalyses/flow accumulation/plots/cum_flow_plot.png", plot = cum_flow_plot, width = 12, height = 8, dpi = 600)

head(cum_f1_dry)


plot_stacked_bar_proportion <- function(..., time_interval = "1 month", custom_colors = NULL, custom_labels = NULL) {
  
  # Combine all input dataframes into one dataframe
  combined_df <- bind_rows(...) %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    group_by(Site, period = floor_date(datetime, time_interval)) %>%
    summarize(MonthlyFlow = sum(CumulativeFlow, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  
  # Calculate the total monthly flow for each period
  total_flow <- combined_df %>%
    group_by(period) %>%
    summarize(TotalFlow = sum(MonthlyFlow, na.rm = TRUE), .groups = "drop")
  
  # Join total monthly flow back to combined dataframe to calculate proportions
  combined_df <- combined_df %>%
    left_join(total_flow, by = "period") %>%
    mutate(Proportion = MonthlyFlow / TotalFlow * 100)
  
  # Calculate average proportion for each site
  avg_proportion <- combined_df %>%
    group_by(Site) %>%
    summarize(AvgProportion = mean(Proportion, na.rm = TRUE), .groups = "drop") %>%
    arrange(AvgProportion)
  
  # Order Site factor levels based on the average proportion, ascending
  combined_df$Site <- factor(combined_df$Site, levels = avg_proportion$Site)
  
  # Default color scheme if not provided
  if (is.null(custom_colors)) {
    custom_colors <- scales::hue_pal()(length(unique(combined_df$Site)))
  }
  
  # Use custom labels if provided
  if (is.null(custom_labels)) {
    custom_labels <- levels(combined_df$Site)
  }
  
  # Ensure that the custom labels match the Site factor levels
  custom_labels <- setNames(custom_labels, levels(combined_df$Site))
  
  # Plotting the stacked bar plot of flow proportions
  p <- ggplot(combined_df, aes(x = period, y = Proportion, fill = Site)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = custom_colors, labels = custom_labels) +
    labs(title = "Proportion of flow contribution by each Site",
         x = "Month",
         y = "Proportion of Total Flow (%)") +
    scale_x_datetime(
      date_breaks = "1 month",
      labels = function(x) {
        months <- format(x, "%b %Y")
        even_months <- seq(2, length(months), by = 2)
        months[even_months] <- ""
        return(months)
      }
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 20),
      plot.title = element_text(size = 27, face = "bold"),
      axis.title.x = element_text(size = 25, margin = margin(t = 10)),
      axis.title.y = element_text(size = 25, margin = margin(r = 10)),
      axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 20)
    )
  
  return(p)
}




custom_colors <- c(
  "Site1" = "#1f77b4",
  "Site2" = "#ff7f0e",
  "Site3" = "#2ca02c",
  "Site4" = "#d62728",
  "Site5" = "#9467bd",
  "Site6" = "#8c564b"
)

# Define custom labels
custom_labels <- c(
  "Site1" = "F0010",
  "Site2" = "F0012",
  "Site3" = "F0014",
  "Site4" = "F0016",
  "Site5" = "F0022",
  "Site6" = "F0101"
)

# Plot with custom colors and labels
cum_bar = plot_stacked_bar_proportion(
  cum_f1, cum_f2, cum_f3, cum_f4, cum_f5, cum_f6,
  custom_colors = custom_colors,
  custom_labels = custom_labels
)

ggsave("Ranalyses/flow accumulation/plots/cum_flow_bar.png", plot = cum_bar, width = 13, height = 9, dpi = 600)




#############################################################################################
# cumulative flow percentage for each site
#############################################################################################
calculate_percentage_contribution <- function(f1, f2, f3, f4, f5, f6, flow_column = "f_volume") {
  
  # Sum the flow volumes for each site
  total_flow_site1 <- sum(f1[[flow_column]], na.rm = TRUE)
  total_flow_site2 <- sum(f2[[flow_column]], na.rm = TRUE)
  total_flow_site3 <- sum(f3[[flow_column]], na.rm = TRUE)
  total_flow_site4 <- sum(f4[[flow_column]], na.rm = TRUE)
  total_flow_site5 <- sum(f5[[flow_column]], na.rm = TRUE)
  total_flow_site6 <- sum(f6[[flow_column]], na.rm = TRUE)
  
  # Calculate the percentage contribution of each site to the total flow at site 6
  percentage_site1 <- (total_flow_site1 / total_flow_site5) * 100
  percentage_site2 <- (total_flow_site2 / total_flow_site5) * 100
  percentage_site3 <- (total_flow_site3 / total_flow_site5) * 100
  percentage_site4 <- (total_flow_site4 / total_flow_site5) * 100
  percentage_site5 <- (total_flow_site5 / total_flow_site5) * 100
  percentage_site6 <- (total_flow_site6 / total_flow_site5) * 100  # This will be 100%
  
  # Store the results in a data frame
  contribution_df <- data.frame(
    Site = c("Site1", "Site2", "Site3", "Site4", "Site5", "Site6"),
    PercentageContribution = c(percentage_site1, percentage_site2, percentage_site3, percentage_site4, percentage_site5, percentage_site6)
  )
  
  return(contribution_df)
}

percentage_contribution_df <- calculate_percentage_contribution(cum_f1, cum_f2, cum_f3, cum_f4, cum_f5, cum_f6, flow_column = "CumulativeFlow")

# View the percentage contribution of each site
print(percentage_contribution_df)

library(dplyr)

# Function to compute average cumulative flow and percentages, then sort by percentage
compute_avg_and_percentages <- function(df_list) {
  # Ensure df_list is a list of data frames
  if (!all(sapply(df_list, is.data.frame))) {
    stop("All elements in df_list must be data frames")
  }
  
  # Define site names internally
  site_names <- c("F0010", "F0012", "F0014", "F0016", "F0022", "F0101")
  
  # Check if the length of site_names matches the number of data frames
  if (length(site_names) != length(df_list)) {
    stop("The number of site names must match the number of data frames")
  }
  
  # Calculate average cumulative flow for each site
  avg_cum_flow <- sapply(df_list, function(df) {
    mean(df$CumulativeFlow, na.rm = TRUE)
  })
  
  # Round average cumulative flow values to 2 decimal places
  avg_cum_flow <- round(avg_cum_flow, 2)
  
  # Extract average cumulative flow for site 5 (df5)
  avg_cum_flow_site5 <- avg_cum_flow[5]
  
  # Calculate proportions and convert to percentages
  proportions <- avg_cum_flow / avg_cum_flow_site5
  percentages <- proportions * 100
  
  # Round percentages to 2 decimal places
  percentages <- round(percentages, 2)
  
  # Create a data frame with site names, average cumulative flow values, and percentages
  result_df <- data.frame(
    Site = site_names,
    Avg_Cum_Flow = avg_cum_flow,
    Percentage = percentages
  )
  
  # Arrange the data frame in increasing order of percentages
  result_df <- result_df %>% arrange(Percentage)
  
  return(result_df)
}

flow_acc = compute_avg_and_percentages(list(cum_f1,cum_f2,cum_f3,cum_f4,cum_f5,cum_f6))
write.csv(flow_acc,"Ranalyses/flow accumulation/flow_acc.csv")

#############################################################################################
# getting flow for dry and wet days
#############################################################################################
classify_and_separate_flow <- function(rainfall_data, flow_data, rainfall_threshold = 1.0) {
  
  # Convert the date columns to Date format in rainfall_data
  rainfall_data <- rainfall_data %>%
    mutate(time = as.Date(time))
  
  # Ensure the flow_data Time column is already in POSIXct format
  # Create a date column for merging
  flow_data <- flow_data %>%
    mutate(date = as.Date(Time))
  
  # Classify days based on rainfall
  rainfall_classification <- rainfall_data %>%
    mutate(day_type = ifelse(rainfall >= rainfall_threshold, 'Wet', 'Dry'))
  
  # Join the classification with flow data
  flow_with_classification <- flow_data %>%
    left_join(rainfall_classification %>% select(time, day_type, rainfall), by = c('date' = 'time'))
  
  # Separate into dry and wet day DataFrames
  dry_days_flow_df <- flow_with_classification %>%
    filter(day_type == 'Dry') %>%
    select(Time, Depth, f_volume,rainfall)
  
  wet_days_flow_df <- flow_with_classification %>%
    filter(day_type == 'Wet') %>%
    select(Time, Depth, f_volume,rainfall)
  
  # Return the data frames as a list
  return(list(dry_days_flow_df = dry_days_flow_df, wet_days_flow_df = wet_days_flow_df))
}


avg_rain <- bind_rows(f1_rn, f2_rn, f3_rn, f4_rn, f5_rn, f6_rn) %>%
  group_by(time) %>%
  summarise(rainfall = mean(rainfall, na.rm = TRUE))

nrow(avg_rain %>% filter(time >= "2019-09-23" & time <= "2020-10-08" & rainfall>= 1))


t = avg_rain %>% filter(time >= "2019-09-23" & time <= "2020-10-07")

f1_dry = classify_and_separate_flow(avg_rain,f1)$dry_days_flow_df
f1_wet = classify_and_separate_flow(avg_rain,f1)$wet_days_flow_df

f2_dry = classify_and_separate_flow(avg_rain,f2)$dry_days_flow_df
f2_wet = classify_and_separate_flow(avg_rain,f2)$wet_days_flow_df

f3_dry = classify_and_separate_flow(avg_rain,f3)$dry_days_flow_df
f3_wet = classify_and_separate_flow(avg_rain,f3)$wet_days_flow_df

f4_dry = classify_and_separate_flow(avg_rain,f4)$dry_days_flow_df
f4_wet = classify_and_separate_flow(avg_rain,f4)$wet_days_flow_df

f5_dry = classify_and_separate_flow(avg_rain,f5)$dry_days_flow_df
f5_wet = classify_and_separate_flow(avg_rain,f5)$wet_days_flow_df

f6_dry = classify_and_separate_flow(avg_rain,f6)$dry_days_flow_df
f6_wet = classify_and_separate_flow(avg_rain,f6)$wet_days_flow_df



#############################################################################################
# cumulative flow for dry and wet days
#############################################################################################
cumulative_flow_dry <- calculate_cumulative_flow(f1_dry, f2_dry, f3_dry, f4_dry, f5_dry, f6_dry,datetime_column = "Time")

# Accessing each cumulative flow data frame
cum_f1_dry = cumulative_flow_dry$cumulative_flow_site1
cum_f2_dry = cumulative_flow_dry$cumulative_flow_site2  # Tributary
cum_f3_dry = cumulative_flow_dry$cumulative_flow_site3
cum_f4_dry = cumulative_flow_dry$cumulative_flow_site4
cum_f5_dry = cumulative_flow_dry$cumulative_flow_site5
cum_f6_dry = cumulative_flow_dry$cumulative_flow_site6

percentage_contribution_df_dry <- calculate_percentage_contribution(cum_f1_dry, cum_f2_dry, cum_f3_dry, cum_f4_dry, cum_f5_dry, cum_f6_dry, flow_column = "CumulativeFlow")

# View the percentage contribution of each site
print(percentage_contribution_df_dry)


cumulative_flow_wet <- calculate_cumulative_flow(f1_wet, f2_wet, f3_wet, f4_wet, f5_wet, f6_wet,datetime_column = "Time")

# Accessing each cumulative flow data frame
cum_f1_wet = cumulative_flow_wet$cumulative_flow_site1
cum_f2_wet = cumulative_flow_wet$cumulative_flow_site2  # Tributary
cum_f3_wet = cumulative_flow_wet$cumulative_flow_site3
cum_f4_wet = cumulative_flow_wet$cumulative_flow_site4
cum_f5_wet = cumulative_flow_wet$cumulative_flow_site5
cum_f6_wet = cumulative_flow_wet$cumulative_flow_site6

percentage_contribution_df_wet <- calculate_percentage_contribution(cum_f1_wet, cum_f2_wet, cum_f3_wet, cum_f4_wet, cum_f5_wet, cum_f6_wet, flow_column = "CumulativeFlow")

# View the percentage contribution of each site
print(percentage_contribution_df_wet)

cum_flow_plot_dry = plot_cumulative_flow_multiple_sites(list(cum_f1_dry, cum_f2_dry, cum_f3_dry, cum_f4_dry, cum_f5_dry, cum_f6_dry))
ggsave("Ranalyses/flow accumulation/plots/cum_flow_plot_dry.png", plot = cum_flow_plot_dry, width = 14, height = 10, dpi = 600)

cum_flow_plot_wet = plot_cumulative_flow_multiple_sites(list(cum_f1_wet, cum_f2_wet, cum_f3_wet, cum_f4_wet, cum_f5_wet, cum_f6_wet))
ggsave("Ranalyses/flow accumulation/plots/cum_flow_plot_wet.png", plot = cum_flow_plot_wet, width = 14, height = 10, dpi = 600)


flow_acc_dry = compute_avg_and_percentages(list(cum_f1_dry, cum_f2_dry, cum_f3_dry, cum_f4_dry, cum_f5_dry, cum_f6_dry))
write.csv(flow_acc_dry,"Ranalyses/flow accumulation/flow_acc_dry.csv")

flow_acc_wet = compute_avg_and_percentages(list(cum_f1_wet, cum_f2_wet, cum_f3_wet, cum_f4_wet, cum_f5_wet, cum_f6_wet))
write.csv(flow_acc_wet,"Ranalyses/flow accumulation/flow_acc_wet.csv")






plot_stacked_bar_proportion_with_conditions <- function(dry_weather_data, wet_weather_data, time_interval = "1 month", custom_colors = NULL, custom_labels = NULL) {
  
  # Add a Condition column to differentiate between dry and wet weather
  dry_weather_data <- dry_weather_data %>% mutate(Condition = "Dry")
  wet_weather_data <- wet_weather_data %>% mutate(Condition = "Wet")
  
  # Combine dry and wet weather dataframes into one
  combined_df <- bind_rows(dry_weather_data, wet_weather_data) %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    group_by(Site, Condition, period = floor_date(datetime, time_interval)) %>%
    summarize(MonthlyFlow = sum(CumulativeFlow, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  
  # Calculate the total monthly flow for each period and condition
  total_flow <- combined_df %>%
    group_by(period, Condition) %>%
    summarize(TotalFlow = sum(MonthlyFlow, na.rm = TRUE), .groups = "drop")
  
  # Join total monthly flow back to combined dataframe to calculate proportions
  combined_df <- combined_df %>%
    left_join(total_flow, by = c("period", "Condition")) %>%
    mutate(Proportion = MonthlyFlow / TotalFlow * 100)
  
  # Calculate average proportion for each site
  avg_proportion <- combined_df %>%
    group_by(Site) %>%
    summarize(AvgProportion = mean(Proportion, na.rm = TRUE), .groups = "drop") %>%
    arrange(AvgProportion)
  
  # Order Site factor levels based on the average proportion, ascending
  combined_df$Site <- factor(combined_df$Site, levels = avg_proportion$Site)
  
  # Default color scheme if not provided
  if (is.null(custom_colors)) {
    custom_colors <- scales::hue_pal()(length(unique(combined_df$Site)))
  }
  
  # Use custom labels if provided
  if (is.null(custom_labels)) {
    custom_labels <- levels(combined_df$Site)
  }
  
  # Ensure that the custom labels match the Site factor levels
  custom_labels <- setNames(custom_labels, levels(combined_df$Site))
  
  # Plotting the stacked bar plot of flow proportions with separate bars for Dry and Wet conditions
  p <- ggplot(combined_df, aes(x = period, y = Proportion, fill = Site)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~Condition, nrow = 1, strip.position = "top") +
    scale_fill_manual(values = custom_colors, labels = custom_labels) +
    labs(title = "Flow accumulation in dry and wet weather",
         x = "Month",
         y = "Proportion of Total Flow (%)") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 20),
      plot.title = element_text(size = 27, face = "bold"),
      axis.title.x = element_text(size = 25, margin = margin(t = 10)),
      axis.title.y = element_text(size = 25, margin = margin(r = 10)),
      axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 22)
    ) +
    scale_x_datetime(
      labels = function(x) ifelse(as.numeric(format(x, "%m")) %% 2 == 1, format(x, "%b %Y"), ""),
      date_breaks = "1 month"
    )
  
  return(p)
}





dry_weather_data <- list(cum_f1_dry, cum_f2_dry, cum_f3_dry, cum_f4_dry, cum_f5_dry, cum_f6_dry)
wet_weather_data <- list(cum_f1_wet, cum_f2_wet, cum_f3_wet, cum_f4_wet, cum_f5_wet, cum_f6_wet)

# Call the function
p <- plot_stacked_bar_proportion_with_conditions(
  dry_weather_data = bind_rows(dry_weather_data),
  wet_weather_data = bind_rows(wet_weather_data),
  time_interval = "1 month",
  custom_colors = custom_colors ,
  custom_labels = custom_labels 
)
plot(p)
ggsave("Ranalyses/flow accumulation/plots/cum_bar_plot_drywet.png", plot = p, width = 13, height = 9, dpi = 600)


q2[q2$date_time=="2020-06-03 10:00:00",]
dt = as.POSIXct("2020-06-03 12:10:00", tz = "UTC")
f5[f5$Time>="2020-06-03 13:00:00" & f5$Time<="2020-06-03 15:00:00",]

q3[as.Date(q3$date_time) == "2020-06-03" & q3$NH4 >= 3.6 & q3$NH4 <= 4,]


