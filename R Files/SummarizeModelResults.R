library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# Change as intended
filepath <- "~/Desktop/School/Zellmer Lab/Junior/Output/"

# Used to generate list of species
old <- read.csv("~/Desktop/School/Zellmer Lab/Junior/pacagdmd_subset_cleaned.csv")
species_list <- unique(old$commonName)

# Adapt name for filepath
species_list[which(species_list == "Squirrel/Chipmunk")] <- "Squirrel-Chipmunk"

# Don't consider these species (change as intended)
exclude <- c("Empty", "Unknown", "Human", "Mountain lion", "Snake", "Western Toad", "Domestic Horse", "butterfly (cannot ID)") # Some excluded due to error in modeling, lack of output

# List of target species
target <- c("Mountain lion", "Bobcat", "Striped Skunk", "Rodent", "Gray fox", 
            "Squirrel-Chipmunk", "Coyote", "Virginia opossum", "Rabbit", "Mule deer",
            "Raccoon") # Black bear too? Missing any others?

# Empty dataframe to put model data into
df <- data.frame(
  Species = character(),
  Target = logical(),
  Range = character(),
  By = character(),
  model = character(),
  npar = numeric(),
  AIC = numeric(),
  delta = numeric(),
  AICwt = numeric(),
  cumltvWt = numeric()
)

# For both Day and Week
for (type in c("Day", "Week")) {
  # For both All and Middle weeks
  for (range in c("All", "Middle")) {
    # For each species...
    for (species in species_list) {
      # Not in exclude
      if (species %in% exclude) {
        next
      }
      
      # Read in data
      cur_file <- read.csv(paste0(filepath, range, " Weeks/By ", type, "/Results - ", species, "/", species, "_AIC.csv"))
      
      # Input new data for dataframe
      cur_file["Species"] <- species
      cur_file["Target"] <- species %in% target
      cur_file["Range"] <- range
      cur_file["By"] <- type
      
      # Add model data to general dataframe
      df <- rbind(df, cur_file)
    }
  }
}

# Re-order columns
df <- df %>%
  select(Species, Target, Range, By, model, npar, AIC, delta, AICwt, cumltvWt)

# Write out, change filepath as intended
write.csv(df, "~/Desktop/School/Zellmer Lab/Junior/Output/model_results.csv", row.names = FALSE)
