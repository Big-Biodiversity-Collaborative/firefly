# Rachel Laura
# rlaura@arizona.edu
# 2024-08-18
# Script to clean firefly survey data and run distance modeling

library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(unmarked)
library(scales)
library(lubridate)

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

###### ADD MOON PHASE? #################################


############ Need to adjust for variable effort in surveys ####################
                           
# Bring in distance sampling data
dist_data <- read.csv(file = "data/2024_field_data/distance_sampling.csv", strip.white = TRUE)
# Convert date column to date type
dist_data$date <- as.Date(as.character(dist_data$date),format = "%Y%m%d")


# See how many 1-min surveys were done
num_surveys <- dist_data %>%
  group_by(date, pt_id, survey_minute) %>%
  tally()


# Create a detections objects for each species and unknowns
detects <- dist_data %>%
  filter(cluster_size >= 1) %>%
  select(pt_id, date, survey_minute, time, species, dist_m, cluster_size)
bw_detects <- detects %>%
  filter(species == "BW")
pk_detects <- detects %>%
  filter(species == "PK")
ukn_detects <- detects %>%
  filter(species == "Ukn")

###### Are we measuring fireflies or their cues?? Find some info on how this is addressed 
### for bird song/whale blows

### WHAT TO DO WITH THE SURVEY MINUTES WITH NO DETECTIONS? ##################

# List of points with surveyed with no detections *** essential to include these ***
ptNoObs <- read.csv(file = "data/2024_field_data/no_obs.csv")

# Bring in site/veg data
veg_data <- read.csv(file = "data/2024_field_data/veg_data.csv")

# Change pt_id from character to factor format for unmarked
bw_detects$pt_id <- as.factor(bw_detects$pt_id)
ptNoObs$pt_id <- as.factor(ptNoObs$pt_id)
# Change from integer to numeric
bw_detects$dist_m <- as.numeric(bw_detects$dist_m)

# Append pt_id without detections to detection dataset
length(levels(bw_detects$pt_id)) # 9 levels
(levels(bw_detects$pt_id) <- c(levels(bw_detects$pt_id), levels(ptNoObs$pt_id)))
length(levels(bw_detects$pt_id)) # Now 10 levels

# Create limits for binning detection distances 
dist.breaks <- c(0:30)

# Use formatDistData() to create counts of detection in each bin for each transect
yDat <- 
  formatDistData(bw_detects, 
                       distCol = "dist_m", 
                       transectNameCol = "pt_id", 
                       dist.breaks = dist.breaks)
yDat <- yDat[order(row.names(yDat)),]  # Sort by pt ID so they match the order in covs file
head(yDat)

# Pull only Patagonia sites
patagonia <- site_data %>%
  filter(str_detect(pt_id,"PS"))


######## Convert time to time field ############################


######## Add time since sunset column########################
#patagonia <- patagonia %>%
#  mutate(time_since_sunset = (five_min_start - sunset_time))

# Add column w/number corresponding to survey # for each point
patagonia <- patagonia %>%
  group_by(pt_id) %>%
  arrange(date) %>%
  dplyr::mutate(date_num = (1:n())) %>%
  ungroup()

# Convert data from long to wide format
patagonia_wide <- patagonia %>%
  pivot_wider(id_cols = pt_id, names_from = date_num, 
              # Pivot the table using pt_id as the first column, 
              # use the date_num column when numbering the new columns
              values_from = c(date, # The columns to bring over
                              obs_name, 
                              five_min_start,
                              sunset_time,
                              clouds,
                              wind,
                              precip,
                              humidity,
                              ave_temp,
                              ave_lux,
                              light_source,
                              sky_glow,
                              ave_sm
                              ))

# Add the first the variables from veg data to covariate dataset
covariates <- merge(veg_data, patagonia_wide,  by = "pt_id")

####### Convert some covariates to factors ###################

######### STANDARDIZE COVARIATES?###########################

# Assemble site covariates
site_covariates <- covariates %>%
  select(hab_type, dist_to_water_m, percent_tree_cover)

# Assemble survey covariates
survey_covariates <- covariates %>%
  select(-pt_id, -hab_type, -percent_tree_cover, -dist_to_water_m)
  
# Convert habitat type to factor
site_covariates$hab_type <- as.factor(site_covariates$hab_type)

############# Check for colinearity amongst covariates. Remove hab type? ################

# Create unmarked frame -- identify survey data and covariates, and set distances units

dist_frame <- unmarkedFrameDS(
  y           = as.matrix(yDat), 
  siteCovs    = covariates, 
  survey      = "point", 
  dist.breaks = dist.breaks, 
  unitsIn     = "m")
head(dist_frame)
hist(dist_frame, main="", xlab="Detection Distances (m)")  # Note, outlying obs eliminated

# Look at the unmarked frame

summary(dist_frame)

# Check the distribution of detection distances for BW

hist(bw_detects$dist_m, main="", xlab="Detection Distances (m)")

##### Detection on the line not == 1, need to address this violation of assumption? #########

### Fit Models

# Try a few different detection functions on intercept-only models for density
hn   <- distsamp(~1 ~1, dist_frame, keyfun="halfnorm", output="density", unitsOut="ha")
unif <- distsamp(~1 ~1, dist_frame, keyfun="uniform",  output="density", unitsOut="ha")
haz  <- distsamp(~1 ~1, dist_frame, keyfun="hazard",   output="density", unitsOut="ha")

# Model selection

fms <- fitList("half-normal det fct" = hn,
               "uniform det fct"     = unif,
               "hazard rate det fct" = haz)

(ms <- modSel(fms))
# Half-normal has lowest AIC

# Fit a more interesting model using half normal detection fct

##################### USE DENSITY OR ABUND HERE??###############################

# How to bring in covariates affecting detection when there are 8 of each??#########
m1 <- distsamp(~dist_to_water_m ~dist_to_water_m, dist_frame, 
               keyfun="halfnorm", output="density", unitsOut="ha")

m2 <- distsamp(~dist_to_water_m ~1, dist_frame, 
               keyfun="halfnorm", output="density", unitsOut="ha")

m3 <- distsamp(~1 ~dist_to_water_m, dist_frame, 
               keyfun="halfnorm", output="density", unitsOut="ha")

### Explore estimates 

summary(m1)                 # note both link functions are log scale
coef(m1)                    # betas
exp(coef(m1, type="state")) # backtransform density
exp(coef(m1, type="det"))   # backtransform SD from half-normal det fct

##### Review link functions/when to backtransform
##### Review model interpretation

# Plot density versus distance from water
dist_from_water_model <- data.frame(dist_to_water_m = seq(min(dist_frame@siteCovs$dist_to_water_m),
                                                          to = max(dist_frame@siteCovs$dist_to_water_m),
                                                          length = 100))
newPredict <- predict(m3, type = "state", newdata = dist_from_water_model,
                      appendData = TRUE)
head(newPredict)

plot(newPredict$dist_to_water_m, newPredict[,"Predicted"], type="l",
     ylim= c(0, 2500), xlim= c(0, 205), 
     xlab="Distance to water (m)", ylab="No. fireflies/ha (95% CI)")
lines(newPredict$dist_to_water_m, newPredict[,"Predicted"]-2*newPredict[,"SE"], lty=2)
lines(newPredict$dist_to_water_m, newPredict[,"Predicted"]+2*newPredict[,"SE"], lty=2)


# Try formatMult
#### Need to make an effortMatrix? 
### Figure out how to format the data to work this way
# Select only columns needed for analysis for BW (trying out Mult)
# format is -> formatDistData(distData, distCol, transectNameCol, dist.breaks, 
# occasionCol, effortMatrix)

distData <- dist_data %>%
  filter(species == "BW") %>%
  select(pt_id, dist_m)

# distCol = dist_m

# Transect name column = pt_id Convert to factor
distData$pt_id <- as.factor(distData$pt_id)

# dist.breaks = dist.breaks

# occasionCol

# effortMatrix

















################## Playing with the data###############################
# Create a dataframe will all observations, including negative
pos_neg_data <- dist_data %>%
  filter(str_detect(pt_id, "PS")) %>% # Only pull Patagonia sites
  select(pt_id, date, time, species, cluster_size) %>% # Select columns of interest
  group_by(pt_id, date, species, time) %>% # Group by pt_id, date, species, and survey minute
  summarize(sum(cluster_size)) %>% # Find the max cluster size in each group
  rename_at(5, ~"count") %>% # rename column
  group_by(pt_id, date, species) %>%
  summarise(max(count)) %>%
  rename_at(4, ~"count") # rename column
####### NEED TO USE TIME RATHER THAN SURVEY NUMBER SINCE PT04 WAS SURVEYED TWICE IN 
### ONE NIGHT
# Plot abundance by date (including negatives) for all species
pos_neg_data %>%
  ggplot(aes(x = date, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1", 
                    direction = -1) +
  labs(x = "Date", y = "Max Firefly Counts at Each Point, Summed for Each Night") 

######### SHOULDN'T JUST SUM THEM SINCE NOT THE SAME # OF PTS WERE SURVEYED EACH NIGHT########
### TRIED USING DODGE BUT LOOKS MESSY AND HARD TO READ

# Create a dataframe will all observations for BW only, including negative
pos_neg_bw <- pos_neg_data %>%
  filter(species == "BW" | is.na(species))
  
################################################################################
# Plot abundance by date (including negatives) for BW
pos_neg_bw %>%
  ggplot(aes(x = date, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1", 
                    direction = -1) +
  labs(x = "Date", y = "Max No. Fireflies (min)") +
  theme(legend.position="none", # Remove legend 
        axis.text=element_text(size=12),# Resize text so it's larger
        axis.title=element_text(size=14,face="bold"), # Resize y-axis label so it's larger
        plot.margin = margin(10, 30, 10, 10)) + # Make right margin larger so it doesn't cut off last date label
  scale_y_continuous(breaks = breaks_pretty()) # Make y-axis integers
###############################################################################
  

######### SHOULDN'T JUST SUM THEM SINCE NOT THE SAME # OF PTS WERE SURVEYED EACH NIGHT########
### TRIED USING DODGE BUT LOOKS MESSY AND HARD TO READ







max_count <- full_set %>%
  select(pt_id, date, survey_minute, cluster_size, hab_type) %>% # Select columns of interest
  group_by(pt_id, date, survey_minute, hab_type) %>% # Group data by pt_id, date, and survey minute
  summarise(COUNT = sum(cluster_size)) %>% # Get a sum of all fireflies seen in each survey minute
  group_by(pt_id, date, hab_type) %>%
  summarise(max(COUNT)) %>%
  rename_at(4, ~"max_count") # rename last column







# Look at data only by point and date (ignoring survey minute)
abund_date <- bw_detects %>%
  group_by(pt_id, date) %>%
  tally()

# Plot
abund_date %>% # Line graph
  ggplot(aes(x = date, y = n, color = pt_id)) +
  geom_line()

# Plot
abund_date %>% # Bar plot
  ggplot(aes(x = date, y = n, fill = pt_id)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_brewer(palette = "Paired", 
                    direction = -1)

# Group data by Point ID, date, and survey minute to look at firefly counts
abund_surveys <- bw_detects %>%
  group_by(pt_id, date,survey_minute) %>%
  tally()

# Plot
abund_surveys %>%
  ggplot(aes(x = date, y = n, fill = survey_minute)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_brewer(palette = "Set1", 
                    direction = -1)




# Add dist to water column
full_set <- merge(bw_detects, covariates,  by = "pt_id")

# Make a set with all detections and coviariates for BW and PK
full_set_both_sp <- merge(detects, covariates, by = "pt_id")

# For each date, take the survey minute with the highest # of obs and save it to object
max_count <- full_set %>%
  select(pt_id, date, time, cluster_size, hab_type) %>% # Select columns of interest
  group_by(pt_id, date, time, hab_type) %>% # Group data by pt_id, date, and time
  summarise(COUNT = sum(cluster_size)) %>% # Get a sum of all fireflies seen in each survey minute
  group_by(pt_id, date, hab_type) %>%
  summarise(max(COUNT)) %>%
  rename_at(4, ~"max_count") # rename last column

# Group data by date, max count, and habitat type
max_count_by_hab <- max_count %>%
  group_by(date, hab_type) %>%
  summarize(max(max_count)) %>%
  rename_at(3, ~"max_count")
###### Mess around with this plot to not stacked bars#############

# Plot date (x) vs. max count (y) colored by pt_id
max_count %>%
  ggplot(aes(x = date, y = max_count, fill = pt_id)) +
  geom_bar(stat = "identity") +
  labs(y = "Max Firefly Count", x = "")

# Plot date (x) vs. max count (y) colored by hab_type
max_count %>%
  ggplot(aes(x = date, y = max_count, fill = hab_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "") +
  scale_y_continuous(breaks = breaks_pretty())

# Group max counts by date to combine total counts for each night
max_per_night <- max_count %>%
  group_by(date) %>%
  summarise(sum(max_count)) %>%
  rename_at(2, ~"max")
# Plot max fireflies seen at each pt per night
max_per_night %>%
  ggplot(aes(x = date, y = max)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "") +
  scale_y_continuous(breaks = breaks_pretty())

# Plot max count by habitat type and date
max_count_by_hab %>%
  ggplot(aes(x = date, y = max_count, fill = hab_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "")+
  scale_y_continuous(breaks = breaks_pretty())


# Look at max counts by date
max_count %>%
  ggplot(aes(x = date, y = max_count)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "") +
  theme_bw()+
  scale_y_continuous(breaks = breaks_pretty())


# Plot counts per survey minute by distance to water
# Create a dataframe with count data by survey minute and distance to water
survey_count_by_min <- full_set %>%
  select(pt_id, date, time, cluster_size, dist_to_water_m, hab_type) %>% # Select only columns of interest
  group_by(pt_id, date, time, dist_to_water_m, hab_type) %>% # Group data by pt_id, date, and survey minute
  summarise(COUNT = sum(cluster_size)) %>% # Get a sum of all fireflies seen in each survey minute
  rename_at(6, ~"count") # Rename last column

#################################################################################
# Plot count by veg type and dist to water
survey_count_by_min %>%
  ggplot(aes(x = dist_to_water_m, y = count, color = hab_type)) +
  geom_point(alpha = 0.5) +
  labs(y = "No. Fireflies (min)", x = "Distance to Water (m)") +
  theme_bw()+
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_x_continuous(n.breaks = 8) +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  labs(color = "Vegetation Type") +
  theme(axis.text=element_text(size=12),# Resize text so it's larger
        axis.title=element_text(size=14,face="bold"), # Resize y-axis label so it's larger
        plot.margin = margin(10, 30, 10, 10))  # Make right margin larger so it doesn't cut off last date label
#################################################################################


# Plot
survey_count_by_min %>%
  ggplot(aes(x = dist_to_water_m, y = count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", se = TRUE, fullrange = FALSE, 
              level = 0.95) +
  labs(y = "Firefly count within 1-minute survey", x = "Distance to Water") +
  scale_y_continuous(breaks = breaks_pretty())

# Look at the timing of PK vs. BW
# For each date, take the survey minute with the highest # of obs for each species and save it to object
max_count_both_species <- full_set_both_sp %>%
  select(pt_id, date, time, cluster_size, hab_type, species) %>% # Select columns of interest
  group_by(pt_id, date, time, hab_type, species) %>% # Group data by pt_id, date, survey minute, and species
  summarise(COUNT = sum(cluster_size)) %>% # Get a sum of all fireflies seen in each survey minute
  group_by(pt_id, date, hab_type, species) %>%
  summarise(max(COUNT)) %>%
  rename_at(5, ~"max_count") # rename last column
# Plot
max_count_both_species %>%
  ggplot(aes(x = date, y = max_count, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "") +
  scale_y_continuous(breaks = breaks_pretty())

# Make boxplot
max_count_both_species_by_date %>%
  ggplot(aes(x=species, y=)) + 
  geom_boxplot()

# Make dataset of max counts by species per date
max_count_both_species_by_date <- max_count_both_species %>% 
  filter(species == "BW" | species == "PK") %>%
  group_by(date,species) %>%
  summarise(max(max_count)) %>%
  rename_at(3, ~"max_count") # rename last column

# Line graph of species by date
#max_count_both_species_by_date %>%
#  ggplot(aes(x=date, y=max_count, group=species)) +
#  geom_line(aes(color=species))+
#  geom_point(aes(color=species))

############################################################################
# Bar plot using only highest count per night
max_count_both_species_by_date %>%
  ggplot(aes(x = date, y = max_count, fill = species)) +
  geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
  labs(y = "Max Firefly Count", x = "") +
  scale_y_continuous(breaks = breaks_pretty()) + # Whole integers on the y-axis
  guides(fill=guide_legend(title="Species"))+ # Rename legend title 
  theme(legend.position="bottom", # Move legend to the bottom
  axis.text=element_text(size=12),# Resize text so it's larger
  axis.title=element_text(size=14,face="bold"), # Resize y-axis label so it's larger
  plot.margin = margin(10, 30, 10, 10)) # Make right margin larger so it doesn't cut off last date label
###########################################################################

# Look at habitat use by species
# Plot max count by habitat type, species, and date
max_count_both_species %>%
  ggplot(aes(x = date, y = max_count, color = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Max Firefly Count", x = "")+
  scale_y_continuous(breaks = breaks_pretty())

###############################################################################
# Faceted pie chart by species and habitat type
# Pull only needed columns
selection <- max_count_both_species %>% # Create a new dataframe called selection
  filter(species != "Ukn") %>% # Remove unknowns
  group_by(hab_type, species) %>% # Group by these columns
  summarize(sum(max_count)) %>% # rename last column
  rename_at(3, ~"max_count")
level_order <- c("BW", "PK")
selection %>%
  ggplot(aes(x = 0, y = max_count, fill = hab_type)) + 
  geom_col(position = "fill") + 
  facet_wrap(~species) +
  coord_polar(theta = "y") +
  scale_fill_identity() +
  theme_void() +
  guides(fill=guide_legend(title="Vegetation Type")) +
  scale_fill_manual(values=c('#CC6677', '#44AA99','#4477AA')) +
  theme(legend.position="bottom")

###############################################################################
  



# Plot time of survey vs. abundance by species
time_vs_abundance <- full_set_both_sp %>%
  filter(species != "Ukn") %>%
  select(time, species, cluster_size) %>%
  group_by(time, species) %>%
  tally()
# Density Plot
time_vs_abundance %>%
  ggplot(aes(x = time, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Time (24 hr)", y = "Density") +
  theme_classic()+
  scale_fill_manual(values = c("lightblue", "salmon"),
                    name = "Species",
                    breaks = c("BW", "PK"),
                    labels = c("BW", "PK"))

# Look only at the weeks when the two species overlap
both_sp_overlap_dates <- full_set_both_sp %>%
  filter(date > '2024-07-22') %>%
  filter(species != "Ukn") %>%
  select(date, time, species, cluster_size) %>%
  group_by(time, species) %>%
  tally()
# Histogram
both_sp_overlap_dates %>%
  ggplot(aes(x = time, color = species, fill = species)) +
  geom_histogram() +
  scale_x_time() +
  labs(y = "Frequency)", x = "Time (24 hr)") +
  theme_bw() +
  scale_fill_manual(values = c("salmon", "lightblue"))

# Look at timing of last observation of BW by date
latest_obs <- full_set %>%
  filter(species == "BW") %>%
  select(date, time, cluster_size) %>%
  group_by(date) %>%
  summarise(max(time)) # Take only the latest observation for each date

# Look at abundance by time and date for BW
abund_by_time <- full_set %>%
  filter(species == "BW") %>%
  select(date, time, cluster_size) %>%
  group_by(date, time) %>%
  summarise(count = sum(cluster_size))
# Plot
abund_by_time %>%
  ggplot(aes(x = time, y = count, fill = date)) +
  geom_bar(stat = "identity") 
# Don't really see a different in the time of day BW was out over the season


both_sp_overlap_dates %>%
  ggplot(aes(x = time, color = species, fill = species)) +
  geom_histogram() +
  scale_x_time() +
  labs(y = "Frequency)", x = "Time (24 hr)") +
  theme_bw() +
  scale_fill_manual(values = c("salmon", "lightblue"))


# Look at abundance by time and date for both species
abund_by_time_both <- full_set_both_sp %>%
  filter(species != "Ukn") %>% # Remove unknowns
  select(date, time, cluster_size, species) %>% # Select columns of interest
  group_by(date, time, species) %>% # Group data by time, date, and species
  summarise(count = sum(cluster_size)) # Get counts within those groups
# Plot
abund_by_time_both %>%
  ggplot(aes(x = time, y = count, fill = species)) +
  geom_bar(stat = "identity")


both_sp_overlap_dates %>%
  ggplot(aes(x = time, color = species, fill = species)) +
  geom_histogram() +
  scale_x_time() +
  labs(y = "Frequency)", x = "Time (24 hr)") +
  theme_bw() +
  scale_fill_manual(values = c("salmon", "lightblue"))

####### Try averaging across survey minute for each date/pt_id #################

### Need to rethink how to do this. 
ave_count <- bw_detects %>%
  group_by(pt_id, date) %>%
  tally()




