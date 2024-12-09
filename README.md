# Research: Animal Occupancy Modeling

A collection of R scripts for processing and analyzing wildlife occupancy data based on image entries and active/inactive date information. These scripts generate detailed files and perform statistical modeling to identify the best predictors of species occupancy.

The scripts in this repository are designed to process data from camera trap images and site activity records. They aim to generate species-specific occupancy grids and perform statistical modeling to determine factors influencing presence among sites and timeframes.


## Features
* Data Processing: Converts raw data into formatted occupancy grids for each species.
* Activity Handling: Integrates active and inactive site data to account for observational biases.
* Observation Grids: Creates species-specific files indicating site activity on a daily and weekly scale.
* Occupancy Modeling: Uses generated data to model and predict species occupancy based on various environmental and site-level predictors.

### Required Data
* Image Entries
* Active/Inactive Dates
* Season Dates


## Usage
1. Preprocess the Data: run ActiveInactive.R on the Active/Inactive Dates and run ConvertFileToCSV.R on the Image Entries.
2. Generate Species-Specific Grids: run ConvertToOccupancy.R on the produced files from before as well as the Season Dates.
3. Model Occupancy: run Occupancy Modeling Tutorial.Rmd on the Species-Specific Grids, and run SummarizeModelResults.R on the previous output to analyze best predictors.

### Output Files
* Occupancy Grids: For each species, a CSV file called "Occupancy Report - <species_name> - Seasonal.csv". This file includes daily and weekly activity indicators (0, 1, NA) for each site and season.
* Visualizaton: A bar graph comparing species activity counts across human and machine detections is saved as an image file.
* Model Output: Statistical summaries and predictor importance scores for species occupancy are saved as text and CSV files. For each model of each species for analyzing by all weeks or middle weeks and by day or by week, an image file displays the best-fit curve determined by the model.
* Model Analysis Across Methods: A CSV file compiling model results for all species for all weeks and middle weeks and by day and by week.
