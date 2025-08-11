library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Choose which city to clean
city <- "URIL"

# PACA has different file name, but the rest have the same format
if (city == "PACA") {
  lines <- readLines("../Input Data/pacagdmd_subset.csv")
} else {
  lines <- readLines(paste0("../Input Data/photo_database_fin_", city, ".csv"))
}

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
header <- gsub("^\"|\"$", "", header)
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
# Remove any entries that don't have a Timestamp value, won't be able to give a Season / day
data <- data[data$Timestamp != "NA",]

# Rename columns
data <- data %>%
  rename("Latitude" = "Latitude.x",
         "Longitude" = "Longitude.x")

# Remove quotes from any entries
data$locAbbr <- gsub("^\"|\"$", "", data$locAbbr)
data$Timestamp <- gsub("^\"|\"$", "", data$Timestamp)
data$commonName <- gsub("^\"|\"$", "", data$commonName)
# Set city column to the current city
data$City <- city

# Dataframe with season information
season_dates <- data.frame(
  Season = 1:35,
  start_date = as.Date(c("2015-12-18",
                         "2016-03-18", "2016-06-17", "2016-09-17", "2016-12-18",
                         "2017-03-18", "2017-06-17", "2017-09-17", "2017-12-18",
                         "2018-03-18", "2018-06-17", "2018-09-17", "2018-12-18", 
                         "2019-03-18", "2019-06-17", "2019-09-17", "2019-12-18", 
                         "2020-03-18", "2020-06-17", "2020-09-17", "2020-12-18", 
                         "2021-03-18", "2021-06-17", "2021-09-17", "2021-12-18", 
                         "2022-03-18", "2022-06-17", "2022-09-17", "2022-12-18", 
                         "2023-03-18", "2023-06-17", "2023-09-17", "2023-12-18", 
                         "2024-03-18", "2024-06-17")),
  end_date = as.Date(c("2016-02-14", "2016-05-14", "2016-08-14", "2016-11-14",
                       "2017-02-14", "2017-05-14", "2017-08-14", "2017-11-14",
                       "2018-02-14", "2018-05-14", "2018-08-14", "2018-11-14",
                       "2019-02-13", "2019-05-14", "2019-08-14", "2019-11-14", 
                       "2020-02-13", "2020-05-14", "2020-08-14", "2020-11-14", 
                       "2021-02-13", "2021-05-14", "2021-08-14", "2021-11-14", 
                       "2022-02-13", "2022-05-14", "2022-08-14", "2022-11-14", 
                       "2023-02-13", "2023-05-14", "2023-08-14", "2023-11-14", 
                       "2024-02-13", "2024-05-14", "2024-08-14"))
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
write.csv(data, paste0("../Input Data/photo_database_fin_", city, "_cleaned.csv"), row.names = FALSE)

