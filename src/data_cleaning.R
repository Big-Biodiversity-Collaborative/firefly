# Species distribution modeling for southwest spring firefly
# Rachel Laura
# rlaura@arizona.edu
# 2023-09-11

install.packages("tidyverse")
library(tidyverse)

# _________________________________________________________

# Pull occurrence records from csv files, remove records w/NAs

curated_obs_data <- read_csv((file = "data/2023-09-11-Xerces_BW_curated_records.csv"))
petition_obs_data <- read_csv(file = "data/BW_petition_records.csv")%>%
  filter(!is.na(Accuracy)) %>%
  filter(!is.na(Lat))
ffa_obs_data <- read_csv(file = "data/2023-09-08-Xerces-Firefly-Atlas-Bicellonycha-data.csv")
negative_obs_data <- read_csv(( file = "data/2023-09-13-Xerces-Firefly-Atlas-Bicellonycha-negative-data.csv"))

# Get all data into a comparable format and combine into single dataset
# Create single dataframe to hold all data, start by adding curated dataset

bw_data <- data.frame(curated_obs_data$x, curated_obs_data$y, curated_obs_data$year)

# Add a column for source and presence/absence

bw_data$source <- "curated"
bw_data$pa <- 1

# Rename columns
columns <- c("x", "y", "year", "source", "pa")
colnames(bw_data) = columns

# _________________________________________________________

# Petition records
# Add columns indicating source and pa
petition_obs_data$source <- "petition"
petition_obs_data$pa <- 1

# Cut data down to only the 5 columns we are interested in and rename to match df
petition_obs_data <- subset(petition_obs_data, select = c("Long", 
                                     "Lat", 
                                     "Year",
                                     "source",
                                     "pa"))
colnames(petition_obs_data) = columns


# Append these columns into bw_data
bw_data <- rbind(bw_data, petition_obs_data)

# _________________________________________________________

# Firefly atlas data
# Append to add source and pa columns
ffa_obs_data$source <- "ffatlas"
ffa_obs_data$pa <- 1

# Cut data down to only the 5 columns we are interested in and rename 
ffa_obs_data <- subset(ffa_obs_data, select = c("decimalLongitude", 
                                                "decimalLatitude", 
                                                "year", 
                                                "source", 
                                                "pa"))

colnames(ffa_obs_data) = columns

# Append these columns into bw_data
bw_data <- rbind(bw_data, ffa_obs_data)

# _________________________________________________________

# Negative data
# Append to add source and pa columns
negative_obs_data$source <- "neg"
negative_obs_data$pa <- 0


# Cut data down to only the 5 columns we are interested in and rename 
negative_obs_data <- subset(negative_obs_data, select = c("decimalLongitude", 
                                                "decimalLatitude", 
                                                "year", 
                                                "source", 
                                                "pa"))
colnames(negative_obs_data) = columns


# Append these columns into bw_data
bw_data <- rbind(bw_data, negative_obs_data)


# _________________________________________________________

# Save the output

write.csv(bw_data, "output/bw_obs.csv")

