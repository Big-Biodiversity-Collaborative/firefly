# Rachel Laura
# rlaura@arizona.edu
# 2024-09-04
# Script to clean firefly survey data and run generalized linear model

# Load packages
library(tidyverse)


# Bring in site survey data
site_data <- read.csv(file = "data/2024_field_data/firefly_survey_site_datasheet.csv", strip.white = TRUE)
# Clean up data
site_data <- site_data %>%
  mutate(ave_sm = rowMeans(cbind(sm1, sm2, sm3, sm4),na.rm = TRUE)) %>% # Create average soil moisture column
  mutate(ave_lux = rowMeans(cbind(min_lux, max_lux),na.rm = TRUE)) %>% # Create average lux column
  mutate(ave_temp = rowMeans(cbind(start_temp, end_temp), na.rm = TRUE)) %>%
  select(pt_id, obs_name, date, five_min_start, sunset_time, ave_temp, clouds, wind, precip, # Select only columns of interest
         humidity, ave_lux, light_source, sky_glow, ave_sm) %>%
  mutate_all((~ifelse(is.nan(.), NA, .))) # Convert NaN to NA 
# Convert date column to date type
site_data$date <- as.Date(as.character(site_data$date),format = "%Y%m%d")

# Bring in site/veg data
veg_data <- read.csv(file = "data/2024_field_data/veg_data.csv")

# Bring in distance sampling data
dist_data <- read.csv(file = "data/2024_field_data/distance_sampling.csv", strip.white = TRUE)
# Convert date column to date type
dist_data$date <- as.Date(as.character(dist_data$date),format = "%Y%m%d")

# Get abundance for each survey minute
abund_per_min <- dist_data %>%
  filter(species == "BW" | is.na(species)) %>% # Only use BW observations or surveys with no fireflies
  group_by(pt_id, date, time) %>% # Group by pt_id, date, and time
  summarize(sum(cluster_size)) %>% # Summarize the total # of fireflies seen during each 1-minute survey
  rename_at(4, ~"total_count") # rename last column

# Append site data to abundance data
covariates <- merge(site_data, abund_per_min,  by = c("pt_id", "date"))

# Append veg/site data
covariates_full <- merge(covariates, veg_data, by = "pt_id")

# Select only Patagonia sites
patagonia <- covariates_full %>%
  filter(str_detect(pt_id, "PS"))

# Add time since sunset column - Need to format these columns as times first
#patagonia <- patagonia %>%
#  mutate(time_since_sunset = )

# Run GLM to test how abundance varies with distance to water, date, temp, humidity, and habitat type
v1 <- glm(total_count ~ dist_to_water_m + date + ave_temp + humidity + 
            hab_type, data = patagonia, family = poisson())
### Time got weird, figure out what's going on there. Probably need to format column.
summary(v1)



