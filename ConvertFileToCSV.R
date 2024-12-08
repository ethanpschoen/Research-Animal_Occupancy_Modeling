library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Import file, change filepath/name as necessary
lines <- readLines("~/Desktop/School/Zellmer Lab/Junior/pacagdmd_subset.csv")

# Function to separate values by commas except within lists denoted by {}
split_outside_list <- function(line) {
  fields <- c()
  buffer <- ""
  brace_level <- 0
  
  for (i in 1:nchar(line)) {
    char <- substr(line, i, i)
    
    if (char == "{") {
      brace_level <- brace_level + 1
    }
    else if (char == "}") {
      brace_level <- brace_level - 1
    }
    
    if (char == "," && brace_level == 0) {
      fields <- c(fields, buffer)
      buffer <- ""
    } else {
      buffer <- paste0(buffer, char)
    }
  }
  
  fields <- c(fields, buffer)
  return(fields)
}

# First row are column names
header <- strsplit(lines[1], ",")[[1]]
# Call function defined above
data <- do.call(rbind, lapply(lines[-1], split_outside_list))

# Convert from lines to dataframe
data <- as.data.frame(data, stringsAsFactors = FALSE)

# Define column names
colnames(data) <- header

# Only need these columns for this script and intended output file
keep <- c("locAbbr", "Timestamp", "commonName", "Latitude.x", "Longitude.x", "maxDetectionConf")

# Only keep relevant columns
data <- data[keep]

# Rename columns
data <- data %>%
  rename("Latitude" = "Latitude.x",
         "Longitude" = "Longitude.x")

# Season data
season_dates <- data.frame(
  Season = 1:24,
  start_date = as.Date(c("2018-09-16", "2018-12-16", "2019-03-16", "2019-06-16",
                         "2019-09-16", "2019-12-16", "2020-03-16", "2020-06-16",
                         "2020-09-16", "2020-12-16", "2021-03-16", "2021-06-16",
                         "2021-09-16", "2021-12-16", "2022-03-16", "2022-06-16",
                         "2022-09-16", "2022-12-16", "2023-03-16", "2023-06-16",
                         "2023-09-16", "2023-12-16", "2024-03-16", "2024-06-16")),
  end_date = as.Date(c("2018-11-14", "2019-02-13", "2019-05-14", "2019-08-14",
                       "2019-11-14", "2020-02-13", "2020-05-14", "2020-08-14",
                       "2020-11-14", "2021-02-13", "2021-05-14", "2021-08-14",
                       "2021-11-14", "2022-02-13", "2022-05-14", "2022-08-14",
                       "2022-11-14", "2023-02-13", "2023-05-14", "2023-08-14",
                       "2023-11-14", "2024-02-13", "2024-05-14", "2024-08-14"))
)

# Function for calculating season based on date
get_season <- function(timestamp) {
  for (i in 1:nrow(season_dates)) {
    if (timestamp >= season_dates$start_date[i] & timestamp <= season_dates$end_date[i]) {
      return (season_dates$Season[i])
    }
  }
  return (NA)
}

# Create Season column
data <- data %>%
  mutate(Season = sapply(Timestamp, get_season)) %>%
  drop_na(Season)

# Output file
write.csv(data, "~/Desktop/School/Zellmer Lab/Junior/pacagdmd_subset_cleaned.csv", row.names = FALSE)

