# Rachel Laura
# rlaura@arizona.edu
# 2024-10-21
# Open heirarchical distance sampling

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(unmarked)
library(scales)
library(lubridate)
library(data.table)

# Bring in site survey data
site_data <- read.csv(file = "data/2024_field_data/firefly_survey_site_datasheet.csv", strip.white = TRUE)
# Clean up data
site_data <- site_data %>%
  mutate(ave_sm = rowMeans(cbind(sm1, sm2, sm3, sm4),na.rm = TRUE)) %>% # Create average soil moisture column
  mutate(ave_lux = rowMeans(cbind(min_lux, max_lux),na.rm = TRUE)) %>% # Create & calcuate average lux column
  mutate(ave_temp = rowMeans(cbind(start_temp, end_temp), na.rm = TRUE)) %>% # Create & calculate average temp col 
  select(pt_id, obs_name, date, hour_of_start, minutes_after_sunset, ave_temp, clouds, wind, precip, # Select only columns of interest
         humidity, ave_lux, light_source, sky_glow, ave_sm) %>%
  filter(str_detect(pt_id,"PS")) %>% # Pull only Patagonia sites
  mutate_all((~ifelse(is.nan(.), NA, .)))  # Convert NaN to NA 
# Convert date column to date type
site_data$date <- as.Date(as.character(site_data$date),format = "%Y%m%d")
# Add Julian date column
site_data <- site_data %>%
  mutate(julian = as.POSIXlt(date)$yday) 
# Add survey week date (replicate)
site_data <- site_data %>%
  mutate(week = ifelse(julian == "160"|julian == "161", 1,
                       ifelse(julian == "168"|julian == "169", 2,
                              ifelse(julian == "174"|julian == "175", 3,
                                     ifelse(julian == "182"|julian == "183", 4,
                                            ifelse(julian == "189"|julian == "190", 5,
                                                   ifelse(julian == "203", 6,
                                                          ifelse(julian == "210", 7,
                                                                 ifelse(julian =="215",8,
                                                                        ifelse(julian == "225", 9,
                                                                               NA)))))))))) %>%
  mutate(point_index = ifelse(pt_id == "PS01", 1,
                              ifelse(pt_id == "PS03", 2,
                                     ifelse(pt_id == "PS04", 3,
                                            ifelse(pt_id == "PS05", 4,
                                                   ifelse(pt_id == "PS06", 5,
                                                          ifelse(pt_id == "PS07", 6,
                                                                 ifelse(pt_id == "PS08", 7,
                                                                        ifelse(pt_id == "PS09", 8,
                                                                               ifelse(pt_id == "PS10", 9,
                                                                                      ifelse(pt_id == "PS11", 10, NA)))))))))))

# Bring in site/veg data
veg_data <- read.csv(file = "data/2024_field_data/veg_data.csv")  

# Bring in distance sampling data
dist_data <- read.csv(file = "data/2024_field_data/distance_sampling.csv", strip.white = TRUE)
# Convert date column to date type
dist_data$date <- as.Date(as.character(dist_data$date),format = "%Y%m%d")
# Create a detections objects for BW and convert time to hour of the decimal (decimal min)
dist_data <- dist_data %>% # First create a dataframe with all detections (BW, PK, and Ukn)
  filter(cluster_size >= 1) %>% # Pull only records of detections
  filter(str_detect(pt_id,"PS")) %>% # Pull only Patagonia sites
  filter(species == "BW") %>% # Pull only BW records
  mutate(hour =  substring(time, 1,2)) %>% # Create a new column with hour (24h format)
  mutate(minute = substring(time,4,5)) %>% # Create a new column with minute
  mutate(decimal_min = as.numeric(minute)/60) %>% # Create new column with decimal minute
  mutate(hour_of_day = paste(hour, substring(decimal_min,3,4), sep = ".")) %>% #Combine hour with decimal minute
  mutate(julian = as.POSIXlt(date)$yday) %>% # Add Julian date column
  mutate(week = ifelse(julian == "160"|julian == "161", 1, # Create a week column to serve as replicate #s
                       ifelse(julian == "168"|julian == "169", 2,
                              ifelse(julian == "174"|julian == "175", 3,
                                     ifelse(julian == "182"|julian == "183", 4,
                                            ifelse(julian == "189"|julian == "190", 5,
                                                   ifelse(julian == "203", 6,
                                                          ifelse(julian == "210", 7,
                                                                 ifelse(julian =="215",8,
                                                                        ifelse(julian == "225", 9,
                                                                               NA)))))))))) %>%
  mutate(point_index = ifelse(pt_id == "PS01", 1,
                             ifelse(pt_id == "PS03", 2,
                                    ifelse(pt_id == "PS04", 3,
                                           ifelse(pt_id == "PS05", 4,
                                                  ifelse(pt_id == "PS06", 5,
                                                         ifelse(pt_id == "PS07", 6,
                                                                ifelse(pt_id == "PS08", 7,
                                                                       ifelse(pt_id == "PS09", 8,
                                                                              ifelse(pt_id == "PS10", 9,
                                                                                     ifelse(pt_id == "PS11", 10, NA))))))))))) %>%
  select(pt_id, date, julian, week, survey_minute, hour_of_day, point_index, species, dist_m) # Select only columns of interest

# List of points with surveyed with no detections *** essential to include these ***
ptNoObs <- read.csv(file = "data/2024_field_data/no_obs.csv", strip.white = TRUE) # Can ignore warning, brought it fine 

# Change pt_id from character to factor format for unmarked
dist_data$pt_id <- as.factor(dist_data$pt_id)
ptNoObs$pt_id <- as.factor(ptNoObs$pt_id)
# Change from integer to numeric
dist_data$dist_m <- as.numeric(dist_data$dist_m)


# Create limits for binning detection distances 
breaks <- c(0,5,10,15,20,25,30)

# Create effort matrix showing which points had surveys completed during which week
effortMatrix <- matrix(ncol=9, nrow=10, 0) # Create an empty matrix with j rows (sites) and i columns (weeks)
for (i in 1:9) { # For each week
       print(i)
  subframe <- site_data %>% # Create a subframe of surveys only in that week
    filter(week == i)
      print(subframe)
     for (obs in 1:nrow(subframe)) { # For each site within that subframe
      j <- subframe[obs,17] # Pull the point_index from each row
      print(j)
      effortMatrix[j,i] = 1}
} 


# Append weeks without detections to week list
dist_data$week <- as.factor(dist_data$week)
length(levels(dist_data$week)) # 7 levels
(levels(dist_data$week) <- c(levels(dist_data$week), 1,9)) # Append weeks 1 and 9, which had no obs
length(levels(dist_data$week)) # Now 9 levels

# Append points without detections to pt_id list
length(levels(dist_data$pt_id)) # 9 levels
(levels(dist_data$pt_id) <- c(levels(dist_data$pt_id), levels(ptNoObs$pt_id)))
length(levels(dist_data$pt_id)) # Now 10 levels

dist_data$point_index <- as.factor(dist_data$point_index) # Change to factor
length(levels(dist_data$point_index)) # 9 levels 
point_levels <- c(levels(dist_data$point_index),"8") # Add a level 8 for PS09 w/no obs
point_levels <- sort(as.integer(point_levels)) # sort in order
levels(dist_data$point_index) <- point_levels # Assign 10 levels
length(levels(dist_data$point_index)) # Now 10 levels


###### NEED TO DEAL WITH SURVEY MINUTE ALSO #########
### Run separately for each survey minute and then use model averaging
# For now, just use minute 3
dist_data <- dist_data %>%
  filter(survey_minute == 3)


# Use formatDistData() to create counts of detection in each bin for each transect and each week
yDat <- formatDistData(dist_data, 
                 distCol = "dist_m", 
                 transectNameCol = "point_index", 
                 dist.breaks = breaks,
                 occasionCol = "week",
                 effortMatrix = effortMatrix)


# Bring in observational covariates (for det and/or availability models)
date <- site_data %>%
  pivot_wider(id_cols = pt_id, names_from = week,
              values_from = c(julian))
# Put in order of pt_id
date <- setorder(date, "pt_id")
# Covert to matrix
date <- as.matrix(date)
# Remove pt_id column
date <- date[,-1]
# Fix field where there were two surveys at the same point on the same night
date[3,3] = 174

hour <- site_data %>%
  pivot_wider(id_cols = pt_id, names_from = week,
              values_from = c(hour_of_start))
# Put in order of pt_id
hour <- setorder(hour, "pt_id")
# Covert to matrix
hour <- as.matrix(hour)
# Remove pt_id column
hour <- hour[,-1]
# Fix field where there are two surveys in the same night.
# Just keep the first for now until I figure out how to deal with this###############
hour[3,3] = 19.58


min_after_sunset <- site_data %>%
  pivot_wider(id_cols = pt_id, names_from = week,
              values_from = c(minutes_after_sunset))
# Put in order of pt_id
min_after_sunset <- setorder(min_after_sunset, "pt_id")
# Covert to matrix
min_after_sunset <- as.matrix(min_after_sunset)
# Remove pt_id column
min_after_sunset <- min_after_sunset[,-1]
# Fix field where there are two surveys in the same night.
# Just keep the first for now until I figure out how to deal with this###############
min_after_sunset[3,3] = 3

obs <- site_data %>%
  pivot_wider(id_cols = pt_id, names_from = week,
              values_from = c(obs_name))
# Put in order of pt_id
obs <- setorder(obs, "pt_id")
# Covert to matrix
obs <- as.matrix(obs)
# Remove pt_id column
obs <- obs[,-1]
# Fix field where there are two surveys in the same night.
# Just keep the first for now until I figure out how to deal with this###############
obs[3,3] = "rl"


# Pull site-level covariates for abundance model as lists (by pt_id)
dist_to_creek_m <- veg_data$dist_to_water_m # Distance to creek 
veg_type <- veg_data$hab_type # Vegetation type


# Create replicates df
week <- site_data %>%
  pivot_wider(id_cols = pt_id, names_from = week,
              values_from = c(week))
# Put in order of pt_id
week <- setorder(week, "pt_id")
###############Order changes, but row names are out of order. Is this an issue?#########
# Covert to matrix
week <- as.matrix(week)
# Remove pt_id column
week <- week[,-1]
# Fix field where there are two surveys in the same night.
# Just keep the first for now until I figure out how to deal with this###############
week[3,3] = 3


# Standardize all continuous variables
# Date
mn.date <- mean(as.numeric(as.character(unlist(date)))) ; sd.date <- sd(as.numeric(as.character(unlist(date))))
# Hour of start
mn.hour <- mean(as.numeric(as.character(unlist(hour)))) ; sd.hour <- sd(as.numeric(as.character(unlist(hour))))
# Minutes after sunset
mn.min_after_sunset <- mean(as.numeric(as.character(unlist(min_after_sunset)))) ; sd.min_after_sunset <- sd(as.numeric(as.character(unlist(min_after_sunset))))
# Distance to creek
mn.dist <- mean(dist_to_creek_m) ; sd.dist <- sd(dist_to_creek_m)
date_cov <- (as.numeric(as.character(unlist(date))) - mn.date) / sd.date
hour_cov <- (as.numeric(as.character(unlist(hour))) - mn.hour) / sd.hour
min_after_sunset_cov <- (as.numeric(as.character(unlist(min_after_sunset))) - 
                           mn.min_after_sunset) / sd.min_after_sunset
dist_cov <- (dist_to_creek_m - mn.dist) / sd.dist

##################### Incorrect number of dimensions for these covs####################


# Convert categorical variables to factors
veg_type <- as.factor(veg_type)

# Convert lists to matrices
week_cov <- as.matrix(week)
date_cov <- as.matrix(date_cov)
hour_cov <- as.matrix(hour_cov)
min_after_sunset_cov <- as.matrix(min_after_sunset_cov)




# Package into unmarked GDS data frame and inspect the data
umf <- unmarkedFrameGDS(y = yDat[,1:54], survey="point", unitsIn="m",
                        dist.breaks=breaks, numPrimary = 9,
                        siteCovs = data.frame(dist_cov, veg_type),
                        yearlySiteCovs=list(week = week_cov, 
                                            date = date_cov, 
                                            hour = hour_cov, 
                                            min_after_sunset = min_after_sunset_cov))
str(umf)
summary(umf)

# Make a list of all the obs and availability covs as example from AHM1 (WHY?)
list <- list(week = week, date = date, hour = hour, min_after_sunset = min_after_sunset)


# Model fitting: Null models fm0
# exponential detection function
summary(fm0.exp <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                             keyfun = "exp", output = "density", unitsOut = "ha",
                             mixture = "P", K = 100, se = TRUE, data = umf) )

# hazard detection function
summary(fm0.haz <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                             keyfun = "haz", output = "density", unitsOut = "ha",
                             mixture = "P", K = 100, se = TRUE, data = umf ) )


# half-normal detection function
summary(fm0.hn <- gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1,
                            keyfun = "halfnorm", output = "density", unitsOut = "ha",
                            mixture = "P", K = 100, se = TRUE, data = umf,control=list(trace=TRUE, REPORT=1)) )

# Compare AIC scores for 3 detection functions
rbind('AIC exp' = fm0.exp@AIC, 'AIC haz' = fm0.haz@AIC, 'AIC hn' = fm0.hn@AIC)

# Half-normal has the lowest AIC
backTransform(fm0.hn, type="lambda") # Density (per ha) = 235
backTransform(fm0.hn, type="phi")
backTransform(fm0.hn, type="det")


# Model with time-dependent phi
fm1 <- gdistsamp(lambdaformula = ~1, phiformula = ~week-1,
                 pformula = ~1, keyfun = "haz", output = "density", unitsOut = "ha",
                 mixture = "P", K = 100, se = TRUE, data = umf)
######## GIVING ERROR: Error in model.frame.default(phiformula, yearlySiteCovs, na.action = NULL) : 
### invalid type (list) for variable 'week'

# Compare AIC for models with phi constant and phi time-dependent
rbind('AIC phi constant' = fm0.hhn@AIC, 'AIC phi time-dep' = fm1@AIC)































