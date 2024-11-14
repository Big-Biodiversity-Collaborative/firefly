# Analyzing dates of observations for historical records
# Rachel Laura
# rlaura@arizona.edu
# 2023-10-10

library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

#### Something weird happening with years/dates in the final bw_data_wdates
# dataframe. The year isn't matching between date column and year column.

# _________________________________________________________

# Pull occurrence records from csv files, remove records w/NAs and piceum

curated_obs_data <- read_csv((file = "data/2023-09-11-Xerces_BW_curated_records.csv"))%>%
  filter(scientificName =="Bicellonycha wickershamorum wickershamorum") %>%
  filter(!is.na(day))
petition_obs_data <- read_csv(file = "data/BW_petition_records.csv")%>%
  filter(!is.na(Accuracy)) %>%
  filter(!is.na(Lat)) %>%
  filter(Species == "Bicellonycha wickershamorum wickershamorum")
ffa_obs_data <- read_csv(file = "data/2023-09-08-Xerces-Firefly-Atlas-Bicellonycha-data.csv") %>%
  filter(specificEpithet =="wickershamorum wickershamorum")
negative_obs_data <- read_csv(( file = "data/2023-09-13-Xerces-Firefly-Atlas-Bicellonycha-negative-data.csv")) %>%
  filter(targetTaxonSpecificEpithet =="wickershamorum wickershamorum")
bugguide_obs_data <- read_csv(file = "data/bugguide.csv")

# _________________________________________________________

# Get all data into a comparable format and combine into single dataset that includes dates

# Bring in curated data, convert event date to date format YYYY-MM-DD
curated_obs_data$ymd <- strptime(curated_obs_data$eventDate, "%m/%d/%y")

# Bring in Petition data, need to convert date column into date format YYYY-MM-DD
petition_obs_data$ymd <-strptime(petition_obs_data$Date, "%d %B %Y") 

# Remove NAs
petition_obs_data <- petition_obs_data %>%
  filter(!is.na(ymd))

# Bring in Firefly Atlas data, convert event date column into date format YYYY-MM-DD
ffa_obs_data$ymd <- strptime(ffa_obs_data$eventDate, "%m/%d/%y")

# Bugguide observations, add ymd column
bugguide_obs_data$ymd <- NA


# combine year, Month, and Day into one column
bugguide_obs_data$ymd <- paste(bugguide_obs_data$Day,
                               bugguide_obs_data$Month,
                               bugguide_obs_data$year)

bugguide_obs_data$ymd <-strptime(bugguide_obs_data$ymd, "%d %B %Y")


# _________________________________________________________

# Create single dataframe to hold all data, start by adding curated dataset

bw_data_wdates <- data.frame(curated_obs_data$x, 
                             curated_obs_data$y, 
                             curated_obs_data$year,
                             curated_obs_data$ymd)

# Rename columns
columns <- c("x", "y", "year", "ymd")
colnames(bw_data_wdates) = columns

# Cut bugguide dataset down to only the columns we are interested in and rename 
bugguide_obs_data <- subset(bugguide_obs_data, select = c("x", 
                                                          "y",
                                                          "year",
                                                          "ymd"))
# Append these columns into bw_data
bw_data_wdates <- rbind(bw_data_wdates, bugguide_obs_data)

# Cut petition obs dataset down to only the columns we are interested in and rename 
ffa_obs_data <- subset(ffa_obs_data, select = c("decimalLongitude",
                                                "decimalLatitude",
                                                "year",
                                                "ymd"))
# Rename columns to match
colnames(ffa_obs_data) = columns

# Append these columns into bw_data
bw_data_wdates <- rbind(bw_data_wdates, ffa_obs_data)

# Add petition obs and cut down to only columns we are interested in
petition_obs_data <- subset(petition_obs_data, select = c("Long",
                                                "Lat",
                                                "Year",
                                                "ymd"))

# Rename columns to match
colnames(petition_obs_data) = columns

# Append these columns into bw_data
bw_data_wdates <- rbind(bw_data_wdates, petition_obs_data)

# _________________________________________________________

# Round all x and y to 5 decimal places
bw_data_wdates <- bw_data_wdates %>%
  mutate(across(is.numeric, round, digits = 5))

# Remove duplicates
bw_data_wdates <- bw_data_wdates %>% distinct(y,x, .keep_all = TRUE) 

# Add Julian date column
bw_data_wdates$julian <- NA

# Convert to Julian dates
bw_data_wdates$julian <- as.POSIXlt(bw_data_wdates$ymd)$yday

boxplot(bw_data_wdates$julian)

# Remove random record from February
bw_data_wdates <- subset(bw_data_wdates, bw_data_wdates$julian > 50)

boxplot(bw_data_wdates$julian)

date_mean <- mean(bw_data_wdates$julian)
date_range <- range(bw_data_wdates$julian)
dates_summary <- summary(bw_data_wdates$julian)
date_quantile <- quantile(bw_data_wdates$julian)

# Convert back to calendar dates
date_mean <- as.Date(mean(bw_data_wdates$julian),
                     origin = as.Date("2024-01-01"))
date_quantile <- as.Date(quantile(bw_data_wdates$julian),
                     origin = as.Date("2024-01-01"))
#date_summary <- as.Date(dates_summary,
#                         origin = as.Date("2024-01-01"))
date_range <- as.Date(range(bw_data_wdates$julian),
                      origin = as.Date("2024-01-01"))

hist(bw_data_wdates$julian)

ggplot(bw_data_wdates) +
  geom_histogram(aes(x = bw_data_wdates$julian)) +
  ylab('# of Observations') +
  xlab("Julian Date")

# Look at data by year
hist(bw_data_wdates$year)

# Create time interval column
bw_data_wdates$time_interval <- cut(bw_data_wdates$year, breaks = c(1950,
                                                                    1980,
                                                                    2020,
                                                                    2021,
                                                                    2023))

# View individual year separately
boxplot(bw_data_wdates$julian ~ bw_data_wdates$year, outline = FALSE)

# Plot Julian date by time interval
boxplot(bw_data_wdates$julian ~ bw_data_wdates$time_interval)

# Plot Julian date by time interval using ggplot
ggplot(bw_data_wdates) +
  geom_boxplot(aes(y = bw_data_wdates$julian,
                   x = bw_data_wdates$time_interval)) +
  xlab('Year') +
  ylab("Julian Date")

# Add a bin column
bw_data_wdates$bin <- NA

# Use tidyverse to make bins with approx equal numbers of observations
bw_data_wdates <- bw_data_wdates %>%
  mutate(bin = cut_number(year,
                            n = 3))

# Count obs in each bin
bw_data_wdates %>%
  count(bin)

boxplot(bw_data_wdates$julian ~ bw_data_wdates$bin)





# _________________________________________________________

# Save the outputs

#write.csv(bw_data_wdates, "output/bw_obs_wdates.csv")
