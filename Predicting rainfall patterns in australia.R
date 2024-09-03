# Load libraries
library(dplyr)
library(ggplot2)
library(forecast)
library(reshape2)
library(tidyr)
library(naniar)
library(zoo) 
library(caret) 
library(rpart) 
library(randomForest) 
library(vars) 
library(tibble)
library(rpart.plot)

# Set working directory
setwd("E:/MSC/MSC NEW 2")

# Load the data into a data frame
weather_data <- read.csv("weatherAUS.csv")

# Section 1- Data Cleaning

weather_data$Date <- as.Date(weather_data$Date, format = "%Y-%m-%d")
# Identify character columns
char_cols <- sapply(weather_data, is.character)

# Before cleaning
print("Before cleaning:")
print(head(weather_data[char_cols]))

# Remove whitespace from character columns
char_cols <- sapply(weather_data, is.character)
for (col in names(weather_data[, char_cols])) {
  weather_data[[col]] <- trimws(weather_data[[col]])
}
# After cleaning
print("After cleaning:")
print(head(weather_data[char_cols]))

# Handle Missing Values
missing_values <- sapply(weather_data, function(x) sum(is.na(x)))
total_rows <- nrow(weather_data)
missing_percentage <- (missing_values / total_rows) * 100

# Combine the missing counts and percentages into a data frame
missing_summary <- data.frame(
  Variable = names(missing_values),
  MissingCount = missing_values,
  MissingPercentage = round(missing_percentage, 2)
)

# Save the summary
write.csv(missing_summary, "missing_values_summary.csv", row.names = FALSE)

# Print the summary as a table
print(missing_summary)

# Impute missing values for numeric columns with the mean
numeric_cols <- sapply(weather_data, is.numeric)
weather_data[numeric_cols] <- lapply(weather_data[numeric_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# check missing values for categorical columns
categorical_cols <- sapply(weather_data, is.factor)
mode_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
weather_data[categorical_cols] <- lapply(weather_data[categorical_cols], function(x) {
  ifelse(is.na(x), mode_value(x), x)
})

# Check duplicate rows
duplicate_rows <- weather_data[duplicated(weather_data), ]

if(nrow(duplicate_rows) == 0) {
  cat("No duplicate rows found.\n")
} else {
  cat("Duplicate rows found. Saving to 'duplicate_rows.csv'.\n")
  print(duplicate_rows)
  write.csv(duplicate_rows, "duplicate_rows.csv", row.names = FALSE)
}

# Outlier Detection
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}
numeric_cols <- sapply(weather_data, is.numeric)
outliers <- lapply(weather_data[, numeric_cols], detect_outliers)

# Save outliers summary
outliers_summary <- list()
for (col in names(outliers)) {
  if (length(outliers[[col]]) > 0) {
    outliers_summary[[col]] <- outliers[[col]]
  }
}
capture.output(outliers_summary, file = "outliers_summary.txt")

# Display outliers summary
outliers_summary <- list()
for (col in names(outliers)) {
  if (length(outliers[[col]]) > 0) {
    outliers_summary[[col]] <- outliers[[col]]
    cat("Outliers detected in column:", col, "\n")
    print(outliers[[col]])
  }
}


# Consistency in Categorical Columns
categorical_cols <- sapply(weather_data, is.factor)
for (col in names(weather_data[, categorical_cols])) {
  weather_data[[col]] <- trimws(tolower(weather_data[[col]]))
}
unique_vals <- sapply(weather_data[, categorical_cols], unique)
capture.output(unique_vals, file = "unique_values.txt")

# Validity Checks
invalid_rainfall <- which(weather_data$Rainfall < 0)
if(length(invalid_rainfall) > 0) {
  write.csv(weather_data[invalid_rainfall, ], "invalid_rainfall.csv", row.names = FALSE)
  print("Invalid rainfall values found and saved to invalid_rainfall.csv")
  print(weather_data[invalid_rainfall, ])  # Display invalid rows in the console
} else {
  write("No invalid rainfall values found.", file = "invalid_rainfall.txt")
  print("No invalid rainfall values found.")
}


# Cross-Field Validation
invalid_temp <- which(weather_data$MaxTemp < weather_data$MinTemp)
if(length(invalid_temp) > 0) {
  write.csv(weather_data[invalid_temp, ], "invalid_temp.csv", row.names = FALSE)
  print("Inconsistent temperature values found where MaxTemp is less than MinTemp. These have been saved to invalid_temp.csv.")
  print(weather_data[invalid_temp, ])  # Display invalid rows in the console
} else {
  write("No inconsistent temperature values found.", file = "invalid_temp.txt")
  print("No inconsistent temperature values found where MaxTemp is less than MinTemp.")
}

# Save cleaned data
write.csv(weather_data, "cleaned_weather_data.csv", row.names = FALSE)

#Section 2-  Feature Engineering

# Load the cleaned data
cleaned_weather_data <- read.csv("cleaned_weather_data.csv")

# Normalization Function
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Ensure that the data is loaded correctly
weather_data <- read.csv("cleaned_weather_data.csv")

# Normalize relevant columns
normalized_data <- cleaned_weather_data %>%
  mutate(
    Rainfall = normalize(Rainfall),
    Temp3pm = normalize(Temp3pm),
    WindSpeed9am = normalize(WindSpeed9am)
  )

# Save normalized data
write.csv(normalized_data, "normalized_weather_data.csv", row.names = FALSE)

# 2.1 Moving Averages

# Calculate Moving Averages
normalized_data <- normalized_data %>%
  arrange(Date) %>%
  mutate(
    MovingAvg7 = rollmean(Rainfall, k = 7, fill = NA, align = 'right'),
    MovingAvg30 = rollmean(Rainfall, k = 30, fill = NA, align = 'right')
  )

# Moving Average Plot
png("moving_avg_plot.png", width = 800, height = 600)
try({
  ggplot(normalized_data, aes(x = Date)) +
    geom_line(aes(y = Rainfall, color = "Original"), size = 1) +
    geom_line(aes(y = MovingAvg7, color = "7-Day Moving Avg"), size = 1) +
    geom_line(aes(y = MovingAvg30, color = "30-Day Moving Avg"), size = 1) +
    labs(title = "Rainfall with Moving Averages", x = "Date", y = "Normalized Rainfall") +
    theme_minimal() +
    scale_color_manual(values = c("Original" = "blue", "7-Day Moving Avg" = "red", "30-Day Moving Avg" = "green"))
}, silent = FALSE)


dev.off()


if (!file.exists("moving_avg_plot.png")) {
  print("The moving average plot has not been created.")
} else {
  print("The moving average plot was successfully created.")
}

#Section 3: Visual Insights into Temporal and Seasonal Variations in Rainfall and Temperature
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(ggplot2)

# Load the Data set
cleaned_weather_data <- read.csv("E:/MSC/MSC NEW 2/cleaned_weather_data.csv")

cleaned_weather_data$Date <- as.Date(cleaned_weather_data$Date, format = "%Y-%m-%d")

# Convert Rainfall to numeric just in case it's read as a factor/string
cleaned_weather_data$Rainfall <- as.numeric(as.character(cleaned_weather_data$Rainfall))

# Checking for any NA values that might be causing issues
sum(is.na(cleaned_weather_data$Rainfall))  
sum(is.na(cleaned_weather_data$Date))      

# Plot
library(ggplot2)
p <- ggplot(cleaned_weather_data, aes(x = Date, y = Rainfall)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Rainfall Over Time", x = "Date", y = "Rainfall (mm)") +
  theme_minimal()

# Verify whether the data frame is empty
if(nrow(cleaned_weather_data) > 0 && sum(!is.na(cleaned_weather_data$Rainfall)) > 0) {
  print(p)
} else {
  print("No data available for plotting.")
}

# Save plot
ggsave("Rainfall_Time_Series_Plot.png", plot = p, width = 11, height = 8, dpi = 300)

# Calculating MinTemp and MaxTemp average
cleaned_weather_data <- cleaned_weather_data %>%
  mutate(AvgTemp = (MinTemp + MaxTemp) / 2)

# Time Series Analysis for Temperature Trends
ggplot(cleaned_weather_data, aes(x = Date)) +
  geom_line(aes(y = MinTemp, color = "Min Temperature"), size = 1) +
  geom_line(aes(y = MaxTemp, color = "Max Temperature"), size = 1) +
  labs(title = "Temperature Trends Over Time", x = "Date", y = "Temperature (°C)") +
  scale_color_manual(values = c("Min Temperature" = "blue", "Max Temperature" = "red")) +
  theme_minimal()
# Save the plot
ggsave("Temperature_Trends_Plot.png", width = 10, height = 6, dpi = 300)

# Distribution of Rainfall Amounts
ggplot(cleaned_weather_data, aes(x = Rainfall)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") + 
  coord_cartesian(ylim = c(0, 5000)) + 
  labs(title = "Distribution of Rainfall Amounts", x = "Rainfall (mm)", y = "Frequency") +
  theme_minimal() +
  xlim(0, 100)  


# Save the plot
ggsave("Rainfall_Amount_Plot.png", width = 10, height = 6, dpi = 300)

# Section 4 - Exploratory Data Analysis (EDA)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(naniar)  
library(ggpubr)  

#Load the Data set
cleaned_weather_data <- read.csv("cleaned_weather_data.csv")

locations <- unique(cleaned_weather_data$Location)

main_dir <- "E:/MSC/MSC NEW 2/EDA Results"
if (!dir.exists(main_dir)) {
  dir.create(main_dir)
}

for (location in locations) {
  # Print the location name to ensure it's correct
  cat("Processing location:", location, "\n")
  
  # Set up a specific folder for each location
  location_dir <- file.path(main_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir)
  }
  
  
  location_data <- cleaned_weather_data %>% filter(Location == location)
  
  location_data$Date <- as.Date(location_data$Date, format = "%Y-%m-%d")
  
  # Missing Values Analysis
  missing_data_plot <- gg_miss_var(location_data, show_pct = TRUE) +
    ggtitle(paste("Missing Data by Variable in", location)) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", color = "black", size = 14),  
      axis.title = element_text(color = "black", size = 12), 
      axis.text = element_text(color = "black", size = 10)  
    )
  
  # Save plot
  ggsave(filename = paste0(location_dir, "/missing_data_plot_", location, ".png"), plot = missing_data_plot, width = 8, height = 6)
  
  # Histogram of Rainfall
  histogram_rainfall <- ggplot(location_data, aes(x = Rainfall)) + 
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black") + 
    ggtitle(paste("Histogram of Rainfall in", location)) +
    xlab("Rainfall (mm)") + 
    ylab("Frequency") +
    xlim(0, 10) +  
    ylim(0, 1000) +  
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
  
  # Save thehistogram
  ggsave(filename = paste0(location_dir, "/histogram_rainfall_", location, ".png"), plot = histogram_rainfall, width = 6, height = 4)
  
  #boxplot analysis section
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Load the data set
  locations <- unique(cleaned_weather_data$Location)
  
  # Divide locations into groups of 4
  location_groups <- split(locations, ceiling(seq_along(locations)/4))
  
  # Create a directory for saving the combined boxplots
  combined_dir <- "E:/MSC/MSC NEW 2/Combined Boxplots"
  if (!dir.exists(combined_dir)) {
    dir.create(combined_dir)
  }
  
  # create a combined boxplot
  for (i in seq_along(location_groups)) {
    # Filter data for the current group of locations
    filtered_data <- cleaned_weather_data %>% filter(Location %in% location_groups[[i]])
    # Create the combined boxplot with log scale on the y-axis
    combined_boxplot <- ggplot(filtered_data, aes(x = Location, y = Rainfall, fill = Location)) + 
      geom_boxplot(fill = "orange", color = "red", outlier.shape = 16, outlier.size = 3) + 
      scale_y_log10() + # Apply log scale to the y-axis
      theme_minimal() + 
      ggtitle(paste("Rainfall Distribution in Locations Group", i)) +
      xlab("Location") + 
      ylab("Rainfall (mm)") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")
      )
    
    # Save the combined boxplot
    ggsave(filename = paste0(combined_dir, "/combined_boxplot_group_", i, ".png"), 
           plot = combined_boxplot, width = 12, height = 8)
  
  }


  # Scatter Plot: Rainfall vs Temp3pm
  scatter_plot_rainfall_temp <- ggplot(location_data, aes(x = Temp3pm, y = Rainfall)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    ggtitle(paste("Scatter Plot of Rainfall vs Temperature at 3 PM in", location)) +
    xlab("Temperature at 3 PM") +
    ylab("Rainfall (mm)") +
    theme_minimal()
  ggsave(filename = paste0(location_dir, "/scatter_rainfall_temp3pm_", location, ".png"), plot = scatter_plot_rainfall_temp, width = 8, height = 6)
  
  # Scatter Plot: Rainfall vs WindSpeed9am
  scatter_plot_rainfall_wind <- ggplot(location_data, aes(x = WindSpeed9am, y = Rainfall)) +
    geom_point(alpha = 0.5, color = "green") +
    geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
    ggtitle(paste("Scatter Plot of Rainfall vs Wind Speed at 9 AM in", location)) +
    xlab("Wind Speed at 9 AM (km/h)") +
    ylab("Rainfall (mm)") +
    theme_minimal()
  ggsave(filename = paste0(location_dir, "/scatter_rainfall_windspeed9am_", location, ".png"), plot = scatter_plot_rainfall_wind, width = 8, height = 6)
  # Summary Statistics
  summary_stats <- summary(location_data)
  write.csv(summary_stats, file = paste0(location_dir, "/summary_statistics_", location, ".csv"))
  
  # Outliers Detection
  detect_outliers <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(which(x < lower_bound | x > upper_bound))
  }
  rainfall_outliers <- detect_outliers(location_data$Rainfall)
  if (length(rainfall_outliers) > 0) {
    outlier_data <- data.frame(Rainfall = location_data$Rainfall[rainfall_outliers])
    write.csv(outlier_data, file = paste0(location_dir, "/outliers_", location, ".csv"))
  }
}

# Cross-Field Validation
invalid_temp <- which(location_data$MaxTemp < location_data$MinTemp)
if(length(invalid_temp) > 0) {
  write.csv(location_data[invalid_temp, ],
            paste0("invalid_temp_", location, ".csv"), row.names = FALSE)
} else {
  write(paste("No inconsistent temperature values found in", location), file = paste0("invalid_temp_", location, ".txt"))
}


print("EDA processing complete.") 

output_dir <- "E:/MSC/MSC NEW 2/Summery stastic Plots"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Histogram of Sunshine Hours for All Locations
hist_sunshine_all <- ggplot(cleaned_weather_data, aes(x = Sunshine)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "lightpink") +
  ggtitle("Histogram of Sunshine Hours Across All Locations") +
  xlab("Sunshine Hours") +
  ylab("Frequency") +
  theme_minimal()

# Save the plot
ggsave(file.path(output_dir, "histogram_sunshine_all.png"), plot = hist_sunshine_all, width = 8, height = 6)

# Boxplot of Wind Speed at 9 AM and 3 PM for All Locations
boxplot_wind_speed_all <- ggplot(cleaned_weather_data, aes(x = Location, y = WindSpeed9am)) +
  geom_boxplot(aes(fill = "WindSpeed9am"), color = "blue", alpha = 0.5) +
  geom_boxplot(aes(y = WindSpeed3pm, fill = "WindSpeed3pm"), color = "red", alpha = 0.5) +
  ggtitle("Boxplot of Wind Speed at 9 AM and 3 PM Across All Locations") +
  xlab("Location") +
  ylab("Wind Speed (km/h)") +
  scale_fill_manual(name = "Time of Day", values = c("WindSpeed9am" = "blue", "WindSpeed3pm" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(file.path(output_dir, "boxplot_wind_speed_all.png"), plot = boxplot_wind_speed_all, width = 12, height = 8)

# Histogram for Temperature at 9 AM for All Locations
hist_temp9am_all <- ggplot(cleaned_weather_data, aes(x = Temp9am)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "lightgreen") +
  ggtitle("Histogram of Temperature at 9 AM Across All Locations") +
  xlab("Temperature at 9 AM (°C)") +
  ylab("Frequency") +
  theme_minimal()

# Save the plot
ggsave(file.path(output_dir, "histogram_temp9am_all.png"), plot = hist_temp9am_all, width = 8, height = 6)

# Histogram for Temperature at 3 PM for All Locations
hist_temp3pm_all <- ggplot(cleaned_weather_data, aes(x = Temp3pm)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "lightgreen") +
  ggtitle("Histogram of Temperature at 3 PM Across All Locations") +
  xlab("Temperature at 3 PM (°C)") +
  ylab("Frequency") +
  theme_minimal()

# Save the plot
ggsave(file.path(output_dir, "histogram_temp3pm_all.png"), plot = hist_temp3pm_all, width = 8, height = 6)

# Bar Plot for Humidity at 9 AM for All Locations
barplot_humidity9am_all <- ggplot(cleaned_weather_data, aes(x = factor(Humidity9am))) +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Bar Plot of Humidity at 9 AM Across All Locations") +
  xlab("Humidity at 9 AM (%)") +
  ylab("Frequency") +
  theme_minimal() +  # Corrected theme function call
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(file.path(output_dir, "barplot_humidity9am_all.png"), plot = barplot_humidity9am_all, width = 10, height = 6)

# Bar Plot for Humidity at 3 PM for All Locations
barplot_humidity3pm_all <- ggplot(cleaned_weather_data, aes(x = factor(Humidity3pm))) +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Bar Plot of Humidity at 3 PM Across All Locations") +
  xlab("Humidity at 3 PM (%)") +
  ylab("Frequency") +
  theme_minimal() +  # Corrected theme function call
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save the plot
ggsave(file.path(output_dir, "barplot_humidity3pm_all.png"), plot = barplot_humidity3pm_all, width = 10, height = 6)



# Correlation Matrix for Numerical Variables
numeric_data <- cleaned_weather_data %>%
  select_if(is.numeric)  # Select only numeric columns

correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Print the Correlation Matrix
print(correlation_matrix)

# Save the Correlation Matrix to a file
write.csv(correlation_matrix, file.path("E:/MSC/MSC NEW 2/EDA Results", "Correlation_Matrix.csv"))

# Visualize the Correlation Matrix
library(ggplot2)
library(reshape2)

melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Save the plot
ggsave("Correlation_Matrix_Plot.png", width = 10, height = 8)

print("Correlation Matrix analysis complete.")


# Section 5 - Data Aggregation and Visualization

library(dplyr)
library(ggplot2)

# Load Data set
cleaned_weather_data <- read.csv("cleaned_weather_data.csv")

cleaned_weather_data$Date <- as.Date(cleaned_weather_data$Date, format = "%Y-%m-%d")

# Extract the Month in "Year-Month" format
cleaned_weather_data$Month <- format(cleaned_weather_data$Date, "%Y-%m")

# calculate Mean Rainfall by Month for all locations
monthly_rainfall <- cleaned_weather_data %>%
  group_by(Month) %>%
  summarize(Mean_Rainfall = mean(Rainfall, na.rm = TRUE))


print(head(monthly_rainfall))
print(summary(monthly_rainfall))
print(range(monthly_rainfall$Mean_Rainfall, na.rm = TRUE))

# Save the plot
png("monthly_rainfall_plot.png", width = 1200, height = 600)

# Create the plot
if (var(monthly_rainfall$Mean_Rainfall, na.rm = TRUE) != 0) {  
  ggplot(monthly_rainfall, aes(x = Month, y = Mean_Rainfall, group = 1)) +  
    geom_line(color = "blue", size = 2) + 
    labs(title = "Mean Rainfall by Month Across All Locations", x = "Month", y = "Mean Rainfall (mm)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),  
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12)) +
    scale_y_continuous(limits = c(0, max(monthly_rainfall$Mean_Rainfall, na.rm = TRUE) * 1.1))  
} else {
  print("No variance in data: All mean rainfall values are the same or NA.")
}

dev.off()


# Section 6 - Time Series Analysis

library(forecast)
library(ggplot2)
library(zoo)
library(dplyr)

# Load Data
cleaned_weather_data <- read.csv("cleaned_weather_data.csv")

locations <- unique(cleaned_weather_data$Location)

main_dir <- "E:/MSC/MSC NEW 2/Time Series Results"
if (!dir.exists(main_dir)) {
  dir.create(main_dir, showWarnings = FALSE)
  cat("Main directory created at", main_dir, "\n")
} else {
  cat("Main directory exists at", main_dir, "\n")
}

for (location in locations) {
  # Set up a specific folder for each location
  location_dir <- file.path(main_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir, showWarnings = FALSE)
    cat("Location directory created at", location_dir, "\n")
  } else {
    cat("Location directory exists at", location_dir, "\n")
  }
  
  
  location_data <- cleaned_weather_data %>% filter(Location == location)
  

  location_data$Date <- as.Date(location_data$Date, format = "%Y-%m-%d")
  

  cat("Current working directory is:", getwd(), "\n")
  
  if (nrow(location_data) > 0 && !all(is.na(location_data$Date))) {
    # Convert to a time series object
    location_ts_data <- ts(location_data$Rainfall, frequency = 365, start = c(min(location_data$Date), 1))
    location_ts_data <- na.approx(location_ts_data)  
    
    # STL Decomposition
    stl_fit <- stl(location_ts_data, s.window = "periodic")
    png(file.path(location_dir, "stl_decomposition.png"))
    plot(stl_fit, col="darkgreen")
    dev.off()
    cat("STL Decomposition plot saved for location:", location, "\n")
    
    # ARIMA Modeling
    arima_fit <- auto.arima(location_ts_data)
    arima_forecast <- forecast(arima_fit, h = 365)
    png(file.path(location_dir, "arima_forecast.png"))
    plot(arima_forecast, main = paste("ARIMA Forecast for", location), col="darkblue", fcol="blue", shadecols="lightblue", xlab="Time", ylab="Rainfall")
    dev.off()
    write.csv(data.frame(Date = time(arima_forecast$mean), Forecast = as.data.frame(arima_forecast$mean)), 
              file.path(location_dir, "arima_forecast.csv"), row.names = FALSE)
    cat("ARIMA forecast and data saved for location:", location, "\n")
    
    # ETS Modeling
    ets_fit <- ets(location_ts_data)
    ets_forecast <- forecast(ets_fit, h = 365)
    png(file.path(location_dir, "ets_forecast.png"))
    plot(ets_forecast, main = paste("ETS Forecast for", location), col="darkorange", fcol="orange", shadecols="lightpink", xlab="Time", ylab="Rainfall")
    dev.off()
    write.csv(data.frame(Date = time(ets_forecast$mean), Forecast = as.data.frame(ets_forecast$mean)), 
              file.path(location_dir, "ets_forecast.csv"), row.names = FALSE)
    cat("ETS forecast and data saved for location:", location, "\n")
    
    # check the access
    writeLines("Test content", file.path(location_dir, "simple_test.txt"))
    if (file.exists(file.path(location_dir, "simple_test.txt"))) {
      cat("Simple test file created successfully for location:", location, "\n")
    } else {
      cat("Failed to create simple test file for location:", location, "\n")
    }
  } else {
    cat("Not enough data for time series analysis in", location, "\n")
  }
}

cat("Time Series Analysis processing complete.\n")


# Cross-Correlation Analysis
for (location in locations) {
  # Set up a specific folder for each location
  location_dir <- file.path(main_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir, showWarnings = FALSE)
    cat("Location directory created at", location_dir, "\n")
  }
  

  location_data <- cleaned_weather_data %>% filter(Location == location)
  
  location_data$Date <- as.Date(location_data$Date, format = "%Y-%m-%d")
  
  if (nrow(location_data) > 0 && !all(is.na(location_data$Date))) {
    # Convert to a time series object for Rainfall
    location_ts_data <- ts(location_data$Rainfall, frequency = 365, start = c(min(location_data$Date), 1))
    location_ts_data <- na.approx(location_ts_data)  
    
    #  Cross-Correlation Analysis Temp3pm 
    if ("Temp3pm" %in% colnames(location_data) && sum(!is.na(location_data$Temp3pm)) > 365) {
      try({
        temp_ts_data <- ts(location_data$Temp3pm, frequency = 365, start = c(min(location_data$Date), 1))
        temp_ts_data <- na.approx(temp_ts_data)
        
        cross_corr <- ccf(location_ts_data, temp_ts_data, plot = FALSE)
        png(file.path(location_dir, "cross_correlation.png"))
        plot(cross_corr, main = paste("Cross-Correlation between Rainfall and Temp3pm in", location), col="darkred")
        dev.off()
        
        # Save cross-correlation results
        cross_corr_data <- data.frame(lag = cross_corr$lag, correlation = cross_corr$acf)
        write.csv(cross_corr_data, file.path(location_dir, "cross_correlation.csv"), row.names = FALSE)
        
        cat("Cross-correlation analysis saved for location:", location, "\n")
      }, silent = FALSE)
    } else {
      cat("Temperature data not available or insufficient for cross-correlation in", location, "\n")
    }
  } else {
    cat("Not enough data for time series analysis in", location, "\n")
  }
}

cat("Cross-correlation analysis completed for all locations.\n")


# Section 7 - Model Comparison
library(forecast)
library(ggplot2)
library(dplyr)
library(Metrics)


print(sessionInfo())

# Load data set
cleaned_weather_data <- read.csv("cleaned_weather_data.csv")

print(colnames(cleaned_weather_data))

weather_data <- cleaned_weather_data %>%
  dplyr::select(Date, MaxTemp) %>%  
  filter(!is.na(MaxTemp))

weather_data$Date <- as.Date(weather_data$Date, format = "%Y-%m-%d")

weather_data_grouped <- weather_data %>%
  group_by(Month = format(Date, "%Y-%m")) %>%
  summarise(MeanMaxTemp = mean(MaxTemp, na.rm = TRUE), .groups = 'drop')

weather_data_grouped$Month <- as.Date(paste0(weather_data_grouped$Month, "-01"))

ts_data <- ts(weather_data_grouped$MeanMaxTemp, frequency = 12)

#ARIMA model
arima_model <- auto.arima(ts_data)
arima_forecast <- forecast(arima_model, h = 12)  # Forecast next 12 months

# ETS model
ets_model <- ets(ts_data)
ets_forecast <- forecast(ets_model, h = 12)

# Extract dates for the forecast period
forecast_dates <- seq(from = max(weather_data_grouped$Month), by = "1 month", length.out = 12)

# Create a dataframe to combine actual data 
forecast_data <- data.frame(
  Date = forecast_dates,
  ARIMA = as.numeric(arima_forecast$mean),
  ETS = as.numeric(ets_forecast$mean)
)

actual_data <- tail(weather_data_grouped, 12)
forecast_data$Actual <- actual_data$MeanMaxTemp

# Calculate MAD
mad_arima <- mad(forecast_data$Actual, forecast_data$ARIMA)
mad_ets <- mad(forecast_data$Actual, forecast_data$ETS)

# Plotting
ggplot(forecast_data, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 1.2) +
  geom_line(aes(y = ARIMA, colour = "ARIMA Forecast"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = ETS, colour = "ETS Forecast"), size = 1.2, linetype = "dotted") +
  labs(title = "Temperature Forecast Comparison",
       y = "Temperature (°C)",
       colour = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "black", "ARIMA Forecast" = "blue", "ETS Forecast" = "red"))

# Save the plot
ggsave("model_comparison_plot.png", width = 10, height = 6)

# Print MAD values
print(paste("MAD for ARIMA Model: ", mad_arima))
print(paste("MAD for ETS Model: ", mad_ets))

# Save the comparison metrics
write.csv(data.frame(Model = c("ARIMA", "ETS"), MAD = c(mad_arima, mad_ets)), "model_comparison_metrics.csv", row.names = FALSE)

# Model Fitting and Forecasting for Each Location
# Load necessary libraries
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)


plot_dir <- "E:/MSC/MSC NEW 2/Location Plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}


process_location_data <- function(data, location) {
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")  
  
  # Group data by Month and calculate mean MaxTemp
  data_grouped <- data %>%
    group_by(Month = format(Date, "%Y-%m")) %>%
    summarise(MeanMaxTemp = mean(MaxTemp, na.rm = TRUE), .groups = 'drop')
  
  data_grouped$Month <- as.Date(paste0(data_grouped$Month, "-01"))  
  
  if (nrow(data_grouped) > 12) {  # Check for enough data
    ts_data <- ts(data_grouped$MeanMaxTemp, frequency = 12)
    
    # Fit ARIMA and ETS models
    arima_model <- auto.arima(ts_data)
    arima_forecast <- forecast(arima_model, h = 12)
    ets_model <- ets(ts_data)
    ets_forecast <- forecast(ets_model, h = 12)
    
    # Prepare data
    forecast_dates <- seq(from = max(data_grouped$Month), by = "1 month", length.out = 12)
    plot_data <- data.frame(
      Date = forecast_dates,
      ARIMA = as.numeric(arima_forecast$mean),  
      ETS = as.numeric(ets_forecast$mean)       
    )
    
    # Include actual data for the forecast period
    actual_data <- tail(data_grouped, 12)
    plot_data$Actual <- actual_data$MeanMaxTemp
    
    # Calculate MAD
    mad_arima <- mad(plot_data$Actual, plot_data$ARIMA)
    mad_ets <- mad(plot_data$Actual, plot_data$ETS)
    
    # Print MAD values
    cat(paste("MAD for ARIMA Model in", location, ":", mad_arima, "\n"))
    cat(paste("MAD for ETS Model in", location, ":", mad_ets, "\n"))
    
    # Plotting forecasts
    plot_forecast <- ggplot(plot_data, aes(x = Date)) +
      geom_line(aes(y = Actual, colour = "Actual"), size = 1.2) +
      geom_line(aes(y = ARIMA, colour = "ARIMA Forecast"), linetype = "dashed") +
      geom_line(aes(y = ETS, colour = "ETS Forecast"), linetype = "dotted") +
      labs(title = paste("Forecast for", location), x = "Month", y = "Mean Max Temperature") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "black", "ARIMA Forecast" = "blue", "ETS Forecast" = "red"))
    
    # Save the plot
    ggsave(file.path(plot_dir, paste0("forecast_", location, ".png")), plot = plot_forecast, width = 10, height = 6)
  } else {
    cat(paste("Not enough data to model for location:", location, "\n"))
  }
}


locations <- unique(cleaned_weather_data$Location)
for (location in locations) {
  location_data <- filter(cleaned_weather_data, Location == location)
  process_location_data(location_data, location)
}


# Section 8 - Multivariate Analysis

library(dplyr)
library(vars)  
library(ggplot2)
library(forecast)  
library(zoo)       


cleaned_weather_data <- read.csv("E:/MSC/MSC NEW 2/cleaned_weather_data.csv")

cleaned_weather_data$Date <- as.Date(cleaned_weather_data$Date, format = "%Y-%m-%d")

results_dir <- "E:/MSC/MSC NEW 2/Multivariate Analysis Results"

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Create a data frame
summary_table <- data.frame(Location = character(), Significant = logical(), stringsAsFactors = FALSE)

locations <- unique(cleaned_weather_data$Location)
for (location in locations) {
  cat("Processing location:", location, "\n")
  
  location_dir <- file.path(results_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir)
  }
  
  if (all(c("Date", "Rainfall", "Temp3pm", "WindSpeed9am") %in% names(cleaned_weather_data))) {
    location_data <- cleaned_weather_data %>%
      filter(Location == location) %>%
      dplyr::select(Date, Rainfall, Temp3pm, WindSpeed9am) %>%
      na.omit()
    
    if (nrow(location_data) > 50) {
      ts_data <- ts(data.matrix(location_data[, -1]),
                    start = c(as.numeric(format(min(location_data$Date), "%Y")),
                              as.numeric(format(min(location_data$Date), "%m"))),
                    frequency = 12)
      var_model <- VAR(ts_data, p = 2)
      
      # Save plot
      png(file.path(location_dir, paste0(location, "_var_model_plots.png")))
      plot(var_model)
      dev.off()  
      
      # Save the model
      saveRDS(var_model, file.path(location_dir, paste0(location, "_var_model.rds")))
      
      # Check residuals
      residuals_acf <- acf(residuals(var_model), plot = FALSE)
      significant <- any(abs(residuals_acf$acf[-1]) > 0.2) 
      
      summary_table <- rbind(summary_table, data.frame(Location = location, Significant = significant))
      
      if (significant) {
        cat("Significant autocorrelation detected in residuals for location:", location, "\n")
      }
      
    } else {
      cat("Not enough data for VAR modeling at location:", location, "\n")
    }
  } else {
    cat("Required columns missing in data for location:", location, "\n")
  }
}

# Save the summary table
write.csv(summary_table, file.path(results_dir, "var_model_summary.csv"), row.names = FALSE)

cat("Multivariate analysis completed for all locations.\n")

# Section 9 - Machine Learning

# Load libraries
library(dplyr)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC) 


base_dir <- "E:/MSC/MSC NEW 2/Machine Learning Result"
if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}


cleaned_weather_data <- read.csv("E:/MSC/MSC NEW 2/cleaned_weather_data.csv")
cleaned_weather_data$Date <- as.Date(cleaned_weather_data$Date, format = "%Y-%m-%d")

# Convert RainToday to binary format
unique_values <- unique(cleaned_weather_data$RainToday)
if (!all(unique_values %in% c(0, 1))) {
  cleaned_weather_data$RainToday <- ifelse(cleaned_weather_data$RainToday == "Yes", 1, 0)
}

locations <- unique(cleaned_weather_data$Location)

for (location in locations) {
  # Set up a specific folder for each location
  location_dir <- file.path(base_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir, showWarnings = FALSE)
    cat("Location directory created at", location_dir, "\n")
  }
  
  cat("Processing location:", location, "\n")
  

  location_data <- dplyr::filter(cleaned_weather_data, Location == location)
  location_data <- dplyr::select(location_data, Rainfall, Temp3pm, WindSpeed9am, RainToday)
  location_data <- na.omit(location_data)
  
  if (nrow(location_data) > 0) {
    #  testing datasets
    set.seed(123)
    indices <- createDataPartition(location_data$Rainfall, p = 0.8, list = FALSE)
    train_data <- location_data[indices, ]
    test_data <- location_data[-indices, ]
    
    # Linear Regression Model
    lm_model <- lm(Rainfall ~ Temp3pm + WindSpeed9am, data = train_data)
    lm_predictions <- predict(lm_model, newdata = test_data)
    lm_plot <- ggplot(data.frame(Actual = test_data$Rainfall, Predicted = lm_predictions), aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      ggtitle("Linear Regression: Actual vs Predicted")
    ggsave(file.path(location_dir, "LM_Predictions_Plot.png"), plot = lm_plot, width = 8, height = 6)
    
    # Logistic Regression Model with ROC Curve
    logistic_model <- glm(RainToday ~ Temp3pm + WindSpeed9am, family = binomial(link = "logit"), data = train_data)
    logistic_predictions <- predict(logistic_model, test_data, type = "response")
    logistic_classes <- ifelse(logistic_predictions > 0.5, 1, 0)
    confusion <- confusionMatrix(factor(logistic_classes), factor(test_data$RainToday))
    
    # Plot and save the confusion matrix
    cm_data <- as.data.frame(confusion$table)
    cm_plot <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = 1.5) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix for Logistic Regression", x = "Actual", y = "Predicted") +
      theme_minimal()
    ggsave(file.path(location_dir, "Logistic_Regression_Confusion_Matrix.png"), plot = cm_plot, width = 7, height = 6)
    
    # ROC Curve
    roc_curve <- roc(response = test_data$RainToday, predictor = as.numeric(logistic_predictions))
    auc_value <- auc(roc_curve)
    cat("AUC for Logistic Regression at", location, ":", auc_value, "\n")
    roc_data <- data.frame(FPR = 1 - roc_curve$specificities, TPR = roc_curve$sensitivities)
    roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
      geom_line() +
      geom_abline(linetype = "dashed") +
      labs(title = paste("ROC Curve for Logistic Regression -", location))
    ggsave(file.path(location_dir, "ROC_Curve_Logistic_Regression.png"), plot = roc_plot, width = 8, height = 6)
    
    # Decision Tree Model
    dt_model <- rpart(Rainfall ~ Temp3pm + WindSpeed9am, data = train_data)
    dt_predictions <- predict(dt_model, newdata = test_data)
    dt_accuracy <- postResample(dt_predictions, test_data$Rainfall)
    dt_plot_path <- file.path(location_dir, "DT_Model_Plot.png")
    png(dt_plot_path, width = 800, height = 600)
    rpart.plot(dt_model, main = "Decision Tree Model")
    dev.off()
    
    # Random Forest Model
    rf_model <- randomForest(Rainfall ~ Temp3pm + WindSpeed9am, data = train_data)
    rf_predictions <- predict(rf_model, newdata = test_data)
    rf_importance <- importance(rf_model)
    rf_importance_plot <- ggplot(data.frame(Feature = rownames(rf_importance), Importance = rf_importance[,1]), aes(x = Feature, y = Importance)) +
      geom_col() +
      coord_flip() +
      ggtitle("Random Forest Feature Importance")
    ggsave(file.path(location_dir, "RF_Feature_Importance.png"), plot = rf_importance_plot, width = 8, height = 6)
  } else {
    print(paste("Insufficient data for modeling at location:", location))
  }
}

# Load necessary libraries
library(dplyr)
library(nnet)
library(caret)
library(ggplot2)

# Set the base directory for outputs and ensure it exists
base_dir <- "E:/MSC/MSC NEW 2/Machine Learning Result"
if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}

# Function to scale numeric data only
scale_data <- function(data) {
  numeric_data <- data %>% select_if(is.numeric)  # Select only numeric columns
  scaled_data <- as.data.frame(lapply(numeric_data, function(x) {
    if (length(unique(x)) == 1) {
      return(rep(0, length(x)))  # Avoid division by zero if min and max are the same
    } else {
      return((x - min(x)) / (max(x) - min(x)))
    }
  }))
  return(scaled_data)
}

#Neural Network
# Loaddata
cleaned_weather_data <- read.csv("E:/MSC/MSC NEW 2/cleaned_weather_data.csv")
cleaned_weather_data$Date <- as.Date(cleaned_weather_data$Date, format = "%Y-%m-%d")


cleaned_weather_data <- cleaned_weather_data %>%
  dplyr::select(-Date)  # Exclude the Date column

cleaned_weather_data$Rainfall <- as.numeric(as.character(cleaned_weather_data$Rainfall))
cleaned_weather_data$Temp3pm <- as.numeric(as.character(cleaned_weather_data$Temp3pm))
cleaned_weather_data$WindSpeed9am <- as.numeric(as.character(cleaned_weather_data$WindSpeed9am))

locations <- unique(cleaned_weather_data$Location)
for (location in locations) {
  # Set up a specific folder for each location
  location_dir <- file.path(base_dir, location)
  if (!dir.exists(location_dir)) {
    dir.create(location_dir, showWarnings = FALSE)
    cat("Location directory created at", location_dir, "\n")
  }
  
  cat("Processing location:", location, "\n")
  
  location_data <- dplyr::filter(cleaned_weather_data, Location == location)
  location_data <- na.omit(location_data)
  
  if (nrow(location_data) > 0) {
    # testing sets
    set.seed(123)
    indices <- sample(1:nrow(location_data), size = 0.8 * nrow(location_data))
    train_data <- location_data[indices, ]
    test_data <- location_data[-indices, ]
    
    # Normalize the data
    train_data_scaled <- scale_data(train_data)
    test_data_scaled <- scale_data(test_data)
    
    nn_model <- nnet(Rainfall ~ Temp3pm + WindSpeed9am, data = train_data_scaled, size = 5, linout = TRUE, maxit = 100)
    
    # Make predictions
    predictions <- predict(nn_model, test_data_scaled)
    
    test_mad <- mean(abs(test_data_scaled$Rainfall - predictions))
    
    # Output results
    cat("Test MAD for", location, ":", test_mad, "\n")
    
    # Visualize predictions
    nn_plot <- ggplot(data.frame(Actual = test_data_scaled$Rainfall, Predicted = predictions), aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.5) +
      ggtitle(paste("Neural Network: Actual vs Predicted for", location))
    ggsave(file.path(location_dir, "NN_Predictions_Plot.png"), plot = nn_plot, width = 8, height = 6)
    
    #  Residual Plot
    residuals <- test_data_scaled$Rainfall - predictions
    residual_plot <- ggplot(data.frame(Residuals = residuals, Fitted = predictions), aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggtitle(paste("Residual Plot for", location))
    ggsave(file.path(location_dir, "NN_Residual_Plot.png"), plot = residual_plot, width = 8, height = 6)
    
    # Histogram of Residuals
    hist_plot <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
      ggtitle(paste("Histogram of Residuals for", location))
    ggsave(file.path(location_dir, "NN_Residual_Histogram.png"), plot = hist_plot, width = 8, height = 6)
    
    #  Summary Metrics
    r_squared <- cor(test_data_scaled$Rainfall, predictions)^2
    rmse <- sqrt(mean(residuals^2))
    
    cat("R-squared for", location, ":", r_squared, "\n")
    cat("RMSE for", location, ":", rmse, "\n")
    
    # Save summary metrics
    summary_file <- file.path(location_dir, "NN_Summary_Metrics.txt")
    writeLines(c(
      paste("MAD:", test_mad),
      paste("R-squared:", r_squared),
      paste("RMSE:", rmse)
    ), con = summary_file)
  } else {
    print(paste("Insufficient data for modeling at location:", location))
  }
}

