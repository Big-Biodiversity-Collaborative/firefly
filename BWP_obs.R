# Analyzing dates of observations for historical records for BWP
# Rachel Laura
# rlaura@arizona.edu
# 2024-02-07

library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# _________________________________________________________

# Pull occurrence records from csv files, remove records w/NAs and BWW

bwp_curated_obs_data <- read_csv((file = "data/2023-09-11-Xerces_BW_curated_records.csv"))%>%
  filter(scientificName =="Bicellonycha wickershamorum piceum") %>%
  filter(!is.na(day))
bwp_petition_obs_data <- read_csv(file = "data/BW_petition_records.csv")%>%
  filter(!is.na(Accuracy)) %>%
  filter(!is.na(Lat)) %>%
  filter(Species == "Bicellonycha wickershamorum piceum")
bwp_ffa_obs_data <- read_csv(file = "data/2023-09-08-Xerces-Firefly-Atlas-Bicellonycha-data.csv") %>%
  filter(specificEpithet =="wickershamorum piceum")
bwp_negative_obs_data <- read_csv(( file = "data/2023-09-13-Xerces-Firefly-Atlas-Bicellonycha-negative-data.csv")) %>%
  filter(targetTaxonSpecificEpithet =="wickershamorum piceum")
bwp_bugguide_obs_data <- read_csv(file = "data/bugguide.csv")


# _________________________________________________________

# Get all data into a comparable format and combine into single dataset that includes dates

# Bring in curated data, convert event date to date format YYYY-MM-DD
bwp_curated_obs_data$ymd <- strptime(bwp_curated_obs_data$eventDate, "%m/%d/%y")

# Bring in Petition data, need to convert date column into date format YYYY-MM-DD
bwp_petition_obs_data$ymd <-strptime(bwp_petition_obs_data$Date, "%d %B %Y") 

# Remove NAs
bwp_petition_obs_data <- bwp_petition_obs_data %>%
  filter(!is.na(ymd))

# Bring in Firefly Atlas data, convert event date column into date format YYYY-MM-DD
bwp_ffa_obs_data$ymd <- strptime(bwp_ffa_obs_data$eventDate, "%m/%d/%y")

# Bugguide observations, add ymd column
bwp_bugguide_obs_data$ymd <- NA


# combine year, Month, and Day into one column
bwp_bugguide_obs_data$ymd <- paste(bwp_bugguide_obs_data$Day,
                               bwp_bugguide_obs_data$Month,
                               bwp_bugguide_obs_data$year)

bwp_bugguide_obs_data$ymd <-strptime(bwp_bugguide_obs_data$ymd, "%d %B %Y")


# _________________________________________________________

# Create single dataframe to hold all data, start by adding curated dataset

bwp_data_wdates <- data.frame(bwp_curated_obs_data$x, 
                             bwp_curated_obs_data$y, 
                             bwp_curated_obs_data$year,
                             bwp_curated_obs_data$ymd)

# Rename columns
columns <- c("x", "y", "year", "ymd")
colnames(bwp_data_wdates) = columns

# Cut bugguide dataset down to only the columns we are interested in and rename 
bwp_bugguide_obs_data <- subset(bwp_bugguide_obs_data, select = c("x", 
                                                          "y",
                                                          "year",
                                                          "ymd"))
# Append these columns into bw_data
bwp_data_wdates <- rbind(bwp_data_wdates, bwp_bugguide_obs_data)

# Cut petition obs dataset down to only the columns we are interested in and rename 
bwp_ffa_obs_data <- subset(bwp_ffa_obs_data, select = c("decimalLongitude",
                                                "decimalLatitude",
                                                "year",
                                                "ymd"))
# Rename columns to match
colnames(bwp_ffa_obs_data) = columns

# Append these columns into bw_data
bwp_data_wdates <- rbind(bwp_data_wdates, bwp_ffa_obs_data)

# Add petition obs and cut down to only columns we are interested in
bwp_petition_obs_data <- subset(bwp_petition_obs_data, select = c("Long",
                                                          "Lat",
                                                          "Year",
                                                          "ymd"))

# Rename columns to match
colnames(bwp_petition_obs_data) = columns

# Append these columns into bw_data
bwp_data_wdates <- rbind(bwp_data_wdates, bwp_petition_obs_data)

# _________________________________________________________

# Round all x and y to 5 decimal places
bwp_data_wdates <- bwp_data_wdates %>%
  mutate(across(is.numeric, round, digits = 5))

# Remove duplicates
bwp_data_wdates <- bwp_data_wdates %>% distinct(y,x, .keep_all = TRUE) 

# Add Julian date column
bwp_data_wdates$julian <- NA

# Convert to Julian dates
bwp_data_wdates$julian <- as.POSIXlt(bwp_data_wdates$ymd)$yday

boxplot(bwp_data_wdates$julian)

# Remove random record from February
bwp_data_wdates <- subset(bwp_data_wdates, bwp_data_wdates$julian > 50)

boxplot(bwp_data_wdates$julian)

date_mean <- mean(bwp_data_wdates$julian)
date_range <- range(bwp_data_wdates$julian)
dates_summary <- summary(bwp_data_wdates$julian)
date_quantile <- quantile(bwp_data_wdates$julian)

# Convert back to calendar dates
date_mean <- as.Date(mean(bwp_data_wdates$julian),
                     origin = as.Date("2024-01-01"))
date_quantile <- as.Date(quantile(bwp_data_wdates$julian),
                         origin = as.Date("2024-01-01"))
#date_summary <- as.Date(dates_summary,
                     #   origin = as.Date("2024-01-01"))
#date_range <- as.Date(range(bw_data_wdates$julian),
                    #  origin = as.Date("2024-01-01"))

hist(bwp_data_wdates$julian)

# Look at data by year
hist(bwp_data_wdates$year)

# Create time interval column
bwp_data_wdates$time_interval <- cut(bwp_data_wdates$year, breaks = c(1950,
                                                                    1980,
                                                                    2020,
                                                                    2021,
                                                                    2023))

# View individual year separately
boxplot(bwp_data_wdates$julian ~ bwp_data_wdates$year, outline = FALSE)

# Plot Julian date by time interval
boxplot(bwp_data_wdates$julian ~ bwp_data_wdates$time_interval)

# Plot Julian date by time interval using ggplot
ggplot(bwp_data_wdates) +
  geom_boxplot(aes(y = bwp_data_wdates$julian,
                   x = bwp_data_wdates$time_interval)) +
  xlab('Year') +
  ylab("Julian Date")

# Add a bin column
bwp_data_wdates$bin <- NA

# Use tidyverse to make bins with approx equal numbers of observations
#bwp_data_wdates <- bwp_data_wdates %>%
# mutate(bin = cut_number(year,
#                          n = 3))

# Count obs in each bin
#bwp_data_wdates %>%
#  count(bin)

#boxplot(bw_data_wdates$julian ~ bw_data_wdates$bin)



