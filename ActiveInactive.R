library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Import file, change filepath/name as necessary
df_activity <- read.csv("~/Desktop/School/Zellmer Lab/Junior/Input Data/OccupancyReport - Active Dates.csv", skip = 2)

# Inactive if NA, Active if 0 or 1 
df_activity <- df_activity %>%
  mutate(across(starts_with("Day_"), ~ case_when(
    is.na(.) ~ "Inactive",
    . %in% c(1, 0) ~ "Active"
  )))

# Date of start of data
start_date <- as.Date("2018-09-15")

# Converts days in Day_1, Day_2, etc. format to actual date
df_activity_long <- df_activity %>%
  pivot_longer(
    cols = starts_with("Day_"),
    names_to = "Day",
    values_to = "Status"
  ) %>%
  mutate(
    day_number = as.integer(gsub("Day_", "", Day)),
    date = start_date + days(day_number - 1),
    Status = Status == "Active"
  ) %>%
  select(-Day, -day_number, -Season)

# Defines seasons
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

# Add Season column and rename other columns
df_activity_long <- df_activity_long %>%
  mutate(Season = sapply(date, get_season)) %>%
  drop_na(Season) %>%
  rename("locAbbr" = "Site", "commonName" = "Species")

# Make sure date is of type Date
df_activity_long$date <- as.Date(df_activity_long$date)

# Combine with season_dates to find relative_day (day of the season)
df_activity_long <- df_activity_long %>%
  left_join(season_dates, by = "Season") %>%
  mutate(relative_day = as.integer(difftime(as.Date(date), start_date, units = "days")) + 1) %>%
  filter(relative_day >= 1 & relative_day <= 60) %>%
  select(Season, locAbbr, Latitude, Longitude, relative_day, Status)

# Make columns for Day_1, Day_2, etc.
active_dates_wide <- df_activity_long %>%
  pivot_wider(names_from = relative_day, values_from = Status, names_prefix = "Day_")

active_dates_wide["City"] <- "PACA"

site_info <- active_dates_wide %>%
  select(City, locAbbr, Longitude, Latitude) %>%
  distinct(City, locAbbr, .keep_all = TRUE)

complete_rows <- expand_grid(
  locAbbr = unique(active_dates_wide$locAbbr),
  Season = season_dates$Season
) %>%
  left_join(site_info, by = "locAbbr") %>%
  filter(City %in% active_dates_wide$City)

df_complete <- complete_rows %>%
  left_join(active_dates_wide, by = c("Season", "City", "locAbbr", "Latitude", "Longitude"))

df_complete <- df_complete %>%
  select(Season, City, locAbbr, Latitude, Longitude, starts_with("Day_")) %>%
  mutate(across(starts_with("Day_"), ~ ifelse(is.na(.), FALSE, .)))

# Output file
write.csv(df_complete, "~/Desktop/School/Zellmer Lab/Junior/Input Data/active_dates.csv", row.names = FALSE)
