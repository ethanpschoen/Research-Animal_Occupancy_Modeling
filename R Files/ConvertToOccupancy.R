library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(rmarkdown)
library(stringr)
library(ggplot2)

# Import file, change filepath/name as necessary
df <- read.csv("~/Desktop/School/Zellmer Lab/Junior/pacagdmd_subset_cleaned.csv")

# Import file, change filepath/name as necessary
active_dates <- read.csv("~/Desktop/School/Zellmer Lab/Junior/active_dates.csv")

# Format before was FALSE for inactive, TRUE for active. Converts FALSE to NA and TRUE to 0 (will update to 1 if present)
active_dates <- active_dates %>%
  mutate(across(starts_with("Day_"), ~ ifelse(. == FALSE, NA, 0))) %>%
  mutate(across("locAbbr", substr, 5, nchar(locAbbr))) %>%
  arrange(Season)

# Dataframe with season information
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

# Converts Timestamp to proper date, combines with season_dates to add relative_days column (what day of the season)
df <- df %>%
  mutate(Timestamp = ymd_hms(Timestamp)) %>%
  left_join(season_dates, by = "Season") %>%
  mutate(relative_days = as.integer(difftime(Timestamp, start_date, units = "days")) + 1) %>%
  select(-start_date)

# Create header
header_df <- season_dates %>%
  rename("Season Legend:" = "Season", "Start Date" = "start_date", "End Date" = "end_date")

# Adds empty line of header to make margin before data
end_header_text <- data.frame(
  Season = "",
  Start = "",
  End = ""
)

# Combines headers
header <- bind_rows(header_df, end_header_text)

# Outputs file
write.csv(header, "~/Desktop/School/Zellmer Lab/Junior/Species Occupancy/header.csv", row.names = FALSE)

# Dataframe for tracking site-seasons for human vs. md for each species
species_counts <- data.frame(
  species = character(),
  source = character(),
  count = integer(),
  stringsAsFactors = FALSE
)

# List of different species
species_list <- unique(df$commonName)

# If these commonName not desired, uncomment
# exclude <- c("Human", "Empty", "Unknown")
for (species in species_list) {
  # if (species %in% exclude) {
  #   next
  # }
  
  # Subsets to only current species
  df_human <- df %>%
    filter(commonName == species)
  
  # New dataframe that excludes low confidence below threshold of 0.8
  df_md <- df_human %>%
    filter(maxDetectionConf >= 0.8)

  # List of dataframes to iterate through
  dfs <- list(df_human = df_human, df_md = df_md)
  
  # Creates empty list to add dataframes to
  results_list <- list()
  
  # For each dataframe
  for (df_name in names(dfs)) {
    # Reset cur_grid
    cur_grid <- active_dates %>%
      mutate(commonName = species)
    
    # Define df_cur to current dataframe
    df_cur <- dfs[[df_name]]
    
    # For each entry in dataframe
    for (i in 1:nrow(df_cur)) {
      # Get values of interest
      season <- df_cur$Season[i]
      relative_day <- df_cur$relative_day[i]
      location <- df_cur$locAbbr[i]
      
      # Get name of day column
      day_column_name <- paste0("Day_", relative_day)
      
      # Convert current entry's corresponding cell in cur_grid to 1
      cur_grid[cur_grid$Season == season & cur_grid$locAbbr == location, day_column_name] <- 1
    }
    
    # Ignore Empty species since it's not relevant (can ignore more species too)
    if (species != "Empty") {
      # Look through each site-season, return 1 if an observation was made
      site_seasons <- apply(cur_grid[, grep("^Day_", names(cur_grid))], 1, function(row) as.integer(any(row == 1, na.rm = TRUE)))
      # Add entry to species_counts with count of human vs. md data for species
      species_counts <- rbind(species_counts, data.frame(species = species, source = df_name, count = sum(site_seasons)))
    }
    
    # Add column specifying source
    updated_grid <- cur_grid %>%
      mutate(Treatment = df_name)
    
    # Change locAbbr name for script
    if (df_name == "df_md") {
      updated_grid$locAbbr <- paste0(updated_grid$locAbbr, "md")
    }
    
    # Add dataframe to list
    results_list[[df_name]] <- updated_grid
  }
  
  # Put MD data below Human data
  complete_grid <- bind_rows(results_list)
  
  # Order columns
  complete_grid <- complete_grid %>%
    rename("Species" = "commonName", "SeasonNumber" = "Season", "Site" = "locAbbr") %>%
    select(Species, SeasonNumber, Site, Latitude, Longitude, starts_with("Day_"), Treatment)
  
  # Change species name for file naming
  species_name <- gsub("/", "-", species)
  
  # Filepath specification
  output_filename <- paste0("~/Desktop/School/Zellmer Lab/Junior/Species Occupancy/OccupancyReport - ", species_name, " - Seasonal.csv")
  
  # Output file for each species
  write.csv(complete_grid, output_filename, row.names = FALSE)
}

# Order by highest to least count of site-season observations
species_order <- species_counts %>%
  filter(source == "df_human") %>%
  arrange(desc(count)) %>%
  pull(species)

# Apply order to species_counts, reverse for plotting
species_counts<- species_counts %>%
  mutate(species = factor(species, levels = rev(species_order)))

# Plot site-seasons against species and their human vs. md data
count <- ggplot(species_counts, aes(x = count, y = species, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Number of Site-Seasons with Detections", y = "Species") +
  scale_fill_manual(values = c("df_human" = "orange", "df_md" = "purple")) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "top"
  )

# Path to folder
filepath = "~/Desktop/School/Zellmer Lab/Junior/"

# Save plot
ggsave(filename = paste0(filepath, "Output/detection_by_treatment.png"), plot = count, width = 10, height = 6)

# Change name for filepath access
species_list[which(species_list == "Squirrel/Chipmunk")] <- "Squirrel-Chipmunk"

# Exclude due to modeling errors
exclude <- c("Mountain lion", "Snake", "Western Toad", "Domestic Horse", "Empty")

# For considering by day and by week
for (data_by in c("Day", "Week")) {
  # For considering all dates or just middle dates
  for (data_range in c("All", "Middle")) {
    # For each species...
    for (name in species_list) {
      # Skip if part of exclude list
      if (name %in% exclude) {
        next
      }
      
      # Run "Occupancy Modeling Tutorial.Rmd" with current species and specification
      render(paste0(filepath, "R Files/Occupancy Modeling Tutorial.Rmd"), params = list(path = filepath, sp_name = name, range = data_range, by = data_by))
    }
  }
}
