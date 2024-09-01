####SECTION 1: SET UP####
#setwd("PATH\TO\WORKING\DIRECTORY\FOLDER") 

##Install packages
if (!requireNamespace("readr", quietly = TRUE)) {install.packages("readr")}
if (!requireNamespace("dplyr", quietly = TRUE)) {install.packages("dplyr")}
if (!requireNamespace("geosphere", quietly = TRUE)) {install.packages("geosphere")}
if (!requireNamespace("ggplot2", quietly = TRUE)) {install.packages("ggplot2")}
if (!requireNamespace("sp", quietly = TRUE)) {install.packages("sp")}
if (!requireNamespace("raster", quietly = TRUE)) {install.packages("raster")}
if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}
if (!requireNamespace("sf", quietly = TRUE)) {install.packages("sf")}
if (!requireNamespace("leaflet", quietly = TRUE)) {install.packages("leaflet")}
if (!requireNamespace("nngeo", quietly = TRUE)) {install.packages("nngeo")}
if (!requireNamespace("patchwork", quietly = TRUE)) {install.packages("patchwork")}
if (!requireNamespace("scales", quietly = TRUE)) {install.packages("scales")}
if (!requireNamespace("xgboost", quietly = TRUE)) {install.packages("xgboost")}

##Load packages
library(readr)
library(dplyr)
library(geosphere)
library(ggplot2)
library(sp)
library(raster)
library(tidyverse)
library(sf)
library(leaflet)
library(ggmap)
library(nngeo)
library(patchwork)
library(scales)
library(xgboost)
--------------------------------------------------------------------------------------------------------------------------------------------
####SECTION 2: CLIMBING DATA CLEANING, PREP AND EDA####
##Import climbing data
England <- read_csv("England_2000-2024.csv")
Wales <- read_csv("Wales_2000-2024.csv")
Scotland <- read_csv("Scotland_2000-2024.csv")

##Data cleaning and exploration 
#check column names match 
colnames(England)
colnames(Wales)
colnames(Scotland)
#yes

#combine into one GB dataset
climb_gb <- rbind(England, Wales, Scotland)

#Crag_Id is definite unique identifier
#Check if Crag_Name is also unique identifier
# Check number of unique Crag_Id
num_unique_crag_id <- climb_gb %>%
  summarise(unique_crag_id = n_distinct(Crag_Id))

# Check number of unique Crag_Name
num_unique_crag_name <- climb_gb %>%
  summarise(unique_crag_name = n_distinct(Crag_Name))
#no, less unique crag names than unique crag id's

#find which crag names are associated with multiple ids
# Count number of unique Crag_Id per Crag_Name
crag_name_counts <- climb_gb %>%
  group_by(Crag_Name) %>%
  summarise(num_unique_crag_id = n_distinct(Crag_Id)) %>%
  filter(num_unique_crag_id > 1)

# Print the Crag_Name associated with more than one Crag_Id
print("Crag_Name associated with more than one Crag_Id:")
print(crag_name_counts$Crag_Name)
#150 crag names associated with more than 1 crag id - if referring to crag name when cleaning CHECK LIST and if necessary switch to crag id

# Check for NA values and 'UNKNOWN' values in the dataset
# Find the columns with NA values
na_counts <- sapply(climb_gb, function(x) sum(is.na(x)))
na_counts
#no NA values

#how many 0 values in dataset (shouldn't be any as no reason any variables would have 0 value)
# Function to check for 0 values in each column
check_zero_values <- function(data) {
  zero_cols <- sapply(data, function(col) any(col == 0))
  zero_cols <- names(zero_cols)[zero_cols]
  return(zero_cols)
}

# Apply the function to climb_gb
zero_value_columns <- check_zero_values(climb_gb)
#lat and lng columns have some rows with 0 values - this indicates issues with those columns, need to check

#subset of climb_gb where lat and long aren't in line with GB values
# Define rough boundaries for Great Britain, including scottish islands
gb_lat_min <- 49.0
gb_lat_max <- 61.0
gb_lng_min <- -8.7
gb_lng_max <- 2.5

# Subset climb_gb where lat and lng values are outside Great Britain bounds
climb_gb_outside_gb <- climb_gb %>%
  filter(lat < gb_lat_min | lat > gb_lat_max | 
           lng < gb_lng_min | lng > gb_lng_max)
#187 rows with erroneous lat/lng values

#identify how many unique crags with lat/lng issues
num_unique_crag_names_latlng <- climb_gb_outside_gb %>%
  summarise(unique_crag_names = n_distinct(Crag_Name))
#60 crags

#input researched values for incorrect/missing lat/lng values or remove rows as appropriate

#remove rows where climb count is < 3, will barely register in later analysis
#Crag Z -access not granted by land owner so location kept relatively secret - remove from dataset
#The Goldilocks Zone - access banned and exact location rescinded to stop visits - only ascents in 2016/17 - remove from dataset
#Land of the Giants - complicated access - given through crag moderator, location not available to the public - 16 ticks over 24 years - remove
#Orphaned Route Crag - not a real crag but a repository for routes that have been orphaned from deleted crags - remove
#Apple Crumble Cave, Rattles Hill Quarry, Castle Hill Quarry, Patience, Craig Rhiw Goch Uchaf, Kilchiaran Bay Geo, Murlaggan, Carlaway, South West Crag  - misc location cant be found - remove

#all others lat and lng values input

# Remove rows with specific Crag_Name values
climb_gb_cleaned <- climb_gb %>%
  filter(!(Crag_Name %in% c("Crag Z", "The Goldilocks Zone", "Land of the Giants [LOTG]", "Apple Crumble Cave", "Rattles Hill Quarry", "Orphaned Route Crag", "Castle Hill Quarry", "Patience", "Craig Rhiw Goch Uchaf", "Kilchiaran Bay Geo", 	
                            "Murlaggan", "Carlaway", "South West Crag")))

# Remove rows where Climb_Count is 1 and both lat and lng are 0
climb_gb_cleaned <- climb_gb_cleaned %>%
  filter(!(Climb_Count < 3 & lat == 0 & lng == 0))

# Impute specific lat and lng values for certain Crag_Name values
# Impute lat and lng for "Brow Side"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Brow Side", 54.38405, lat),
         lng = ifelse(Crag_Name == "Brow Side", -3.1858, lng))

# Impute lat and lng for "Ayrmer Cove"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Ayrmer Cove", 50.2939, lat),
         lng = ifelse(Crag_Name == "Ayrmer Cove", -3.9096, lng))

# Impute lat and lng for "Lobwell wood"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Lobwell wood",  53.3175, lat),
         lng = ifelse(Crag_Name == "Lobwell wood", -1.2037, lng))

# Impute lat and lng for "Betsy Crag Slabs"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Betsy Crag Slabs",  54.0069, lat),
         lng = ifelse(Crag_Name == "Betsy Crag Slabs", -2.7869, lng))

# Impute lat and lng for "Mossup Crag"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Mossup Crag",  54.3788, lat),
         lng = ifelse(Crag_Name == "Mossup Crag", -3.1791, lng))

# Impute lat and lng for "Fetlock Zawn"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Fetlock Zawn",  51.5485, lat),
         lng = ifelse(Crag_Name == "Fetlock Zawn", -4.2504, lng))

# Impute lat and lng for "South Buttress"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "South Buttress",  51.9028, lat),
         lng = ifelse(Crag_Name == "South Buttress", -5.3136, lng))

# Impute lat and lng for "Croesor Quarry"
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(lat = ifelse(Crag_Name == "Croesor Quarry",  52.9822, lat),
         lng = ifelse(Crag_Name == "Croesor Quarry", -4.0412, lng))


#recheck all rows in climb_gb have appropriate lat/lng values, should be 0 rows pulled out
climb_gb_check <- climb_gb_cleaned %>%
  filter(lat < gb_lat_min | lat > gb_lat_max | 
           lng < gb_lng_min | lng > gb_lng_max)
#yes

# Find the columns with 'UNKNOWN' values
unknown_counts <- sapply(climb_gb_cleaned, function(x) sum(x == 'UNKNOWN', na.rm = TRUE))
unknown_counts
#86272 columns with UNKNOWN value for rock type

unknown_counts2 <- sapply(climb_gb_cleaned, function(x) sum(x == 'Unknown', na.rm = TRUE))
unknown_counts2
#2 rows have unknown for region name
#checked and it's for 1 crag that can be identified as being in northern scotland
climb_gb_cleaned <- climb_gb_cleaned %>%
  mutate(Region_Name = ifelse(Region_Name == "Unknown", "Northern Scotland", Region_Name))

#Find which crags have UNKNOWN rock type
# Filter the dataset for rows where Rock_Type is 'UNKNOWN'
unknown_rock_type <- climb_gb_cleaned %>% filter(Rock_Type == 'UNKNOWN')

# Count unique Crag_Name entries with 'UNKNOWN' Rock_Type
unique_crag_Id <- unknown_rock_type %>% distinct(Crag_Id)

# Get the number of total unique Crag_Id entries and how many have no rock type listed
num_unique_crag_id <- climb_gb_cleaned %>%
  summarise(unique_crag_id = n_distinct(Crag_Id))
#8170

num_unique_crags <- nrow(unique_crag_Id)
#3274 crags have no listed rock type = 40% of crags
#too many to research/guess/impute?


#Remove all rows with 'Ice' as Rock_Type as specifically looking at rock climbing impacts
climb_gb_cleaned2 <- climb_gb_cleaned %>%
  filter(Rock_Type != "Ice")
#684 rows removed

##Explore dataset

#1. create subset where climb count is totalled per crag for the 24 year period
# Create a subset with total Ascents per Crag_Id, rank by ascents
climb_gb_totals_crag <- climb_gb_cleaned2 %>%
  group_by(Crag_Id, Crag_Name, lat, lng, Rock_Type, Climb_Count, Region_Name, Country) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Total_Ascents))
head(climb_gb_totals_crag)
#most popular crags in Peak, top is Stanage Popular with over 2x ticks as 2nd place Burbage North

#how many ascents total in the dataset
total_ascents_all_crags <- sum(climb_gb_totals_crag$Total_Ascents, na.rm = TRUE)
# Print the total ascents of all crags
print(total_ascents_all_crags)

#2. Create subset with total Ascents by Region
#indicates where climbing is popular? or where logging is popular?
climb_gb_totals_region <- climb_gb_cleaned2 %>%
  group_by(Region_Name) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  ungroup()%>%
  arrange(desc(Total_Ascents))
#plot
ggplot(climb_gb_totals_region, aes(x = reorder(Region_Name, -Total_Ascents), y = Total_Ascents)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +  # Consistent color palette
  theme_minimal(base_size = 14) +  # Increase base font size for readability
  labs(title = "Total Ascents per Region",
       subtitle = "Data aggregated over a 24-year period",
       x = "Region",
       y = "Total Ascents") +  # Removed caption
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  # Adjust label angle for better readability
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold and center title
        plot.subtitle = element_text(size = 14, hjust = 0.5),  # Center and slightly increase subtitle size
        axis.text = element_text(face = "bold"),  # Bold axis text for emphasis
        axis.title = element_text(face = "bold"),  # Bold axis titles for emphasis
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soften grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines
#peak area by far region with most ascents >200,000 comapred to next highest N Wales with ~75,000



#3.Create subset with total Ascents by Year
climb_gb_totals_year <- climb_gb_cleaned2 %>%
  group_by(Year) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  ungroup()

#4.Create subset with total Ascents by Month
climb_gb_totals_month <- climb_gb_cleaned2 %>%
  group_by(Month) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  ungroup()
# Convert numeric month to factor with month names
climb_gb_totals_month <- climb_gb_totals_month %>%
  mutate(Month = factor(Month, levels = 1:12, labels = month.name))

# Plot month and year histograms together
# Plot 1: Total Ascents per Year with normal number formatting on y-axis
plot_year <- ggplot(climb_gb_totals_year, aes(x = Year, y = Total_Ascents)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
  theme_minimal(base_size = 14) +
  labs(title = "Total Ascents per Year",
       x = "Year",
       y = "Total Ascents") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels as regular numbers
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank())

# Plot 2: Total Ascents per Month
plot_month <- ggplot(climb_gb_totals_month, aes(x = Month, y = Total_Ascents)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
  theme_minimal(base_size = 14) +
  labs(title = "Total Ascents per Month",
       x = "Month",
       y = "Total Ascents") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_blank())

# Combine the two plots side by side
combined_plot <- plot_year + plot_month + plot_layout(ncol = 2)

# Display the combined plot
print(combined_plot)


#7. Number of unique Rock_Type
num_unique_rock_types <- climb_gb_cleaned2 %>%
  summarise(unique_rock_types = n_distinct(Rock_Type))
print(paste("Number of unique Rock_Type:", num_unique_rock_types$unique_rock_types))
#41

#8. Most popular Rock_Type ascented
# Aggregate total ascents by Rock_Type across all crags
rock_type_popularity <- climb_gb_cleaned2 %>%
  group_by(Rock_Type) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  arrange(desc(Total_Ascents))

rock_type_popularity
#Gritstone, Limestone, Rhyolite, Sandstone (Hard), Granite, Grit (Quarried), Sandstone (Soft), Unknown, Dolerite, Slate.....

#9. Range of Climb_Count monthly
climb_count_range <- climb_gb_cleaned2 %>%
  summarise(min_climb_count = min(Climb_Count, na.rm = TRUE),
            max_climb_count = max(Climb_Count, na.rm = TRUE))
print(paste("Range of Climb_Count: from", climb_count_range$min_climb_count, "to", climb_count_range$max_climb_count))
#from 1 to 1294 ticks per month per crag


#10. Is it just crags with lots of climbs that see lots of ascents?
# Plot the relationship between Total_Ascents and Climb_Count
ggplot(climb_gb_totals_crag, aes(x = Climb_Count, y = Total_Ascents)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relationship between Total Ascents and Climb Count per Crag",
       x = "Climb Count",
       y = "Total Ascents") +
  geom_smooth(method = "lm", col = "blue", se = FALSE) # Optional: Add a trend line
#sort of but not really, but some real stand out crags in terms of no of climbs or total ascents

#ouptut pretty skewed, try log
# Add a small constant (e.g., 1) to avoid log(0) issues
climb_gb_totals_cragplus1 <- climb_gb_totals_crag %>%
  mutate(Log_Total_Ascents = log(Total_Ascents + 1),
         Log_Climb_Count = log(Climb_Count + 1))

# Plot the relationship using log-transformed values
ggplot(climb_gb_totals_cragplus1, aes(x = Log_Climb_Count, y = Log_Total_Ascents)) +
  geom_point(color = "steelblue", size = 1, alpha = 0.7) +  # Consistent point color and size
  geom_smooth(method = "lm", col = "darkorange", se = FALSE, size = 1) +  # Consistent line color and size
  theme_minimal(base_size = 14) +  # Increase base font size for readability
  labs(title = "Relationship between Total Ascents and Climb Count per Crag",
       subtitle = "Analysis of Log-Transformed Values",
       x = "Log of Climb Count",
       y = "Log of Total Ascents") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold and centered title
        plot.subtitle = element_text(size = 14, hjust = 0.5),  # Centered subtitle
        axis.text = element_text(face = "bold"),  # Bold axis text
        axis.title = element_text(face = "bold"),  # Bold axis titles
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soften grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines
#appears to be positive relationship - crags with more climbs see more ascents

#10. # Average total ascents per crag by region
#Summarise total ascents by Crag_Id and Region_Name
ascents_per_crag_region <- climb_gb_cleaned2 %>%
  group_by(Crag_Id, Region_Name) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE)) %>%
  ungroup()

# Calculate average ascents per crag for each region
avg_ascents_per_region <- ascents_per_crag_region %>%
  group_by(Region_Name) %>%
  summarise(Avg_Ascents_per_Crag = mean(Total_Ascents, na.rm = TRUE)) %>%
  arrange(desc(Avg_Ascents_per_Crag))

# Display the average ascents per crag for each region
avg_ascents_per_region

#plot
ggplot(avg_ascents_per_region, aes(x = reorder(Region_Name, -Avg_Ascents_per_Crag), y = Avg_Ascents_per_Crag)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Average Ascents per Crag for Each Region",
       x = "Region",
       y = "Average Ascents per Crag") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#central southern england has highest average ascents per crag despite having relatively low number of total ascents - not many crags but quite intensely used?, peak 2nd

#11. # Average climb count per crag by region
# Calculate average climb count per crag for each region
avg_climb_count_per_region <- climb_gb_cleaned2 %>%
  group_by(Region_Name) %>%
  summarise(Avg_Climb_Count_per_Crag = mean(Climb_Count, na.rm = TRUE)) %>%
  arrange(desc(Avg_Climb_Count_per_Crag))

# Display the average climb count per crag for each region
avg_climb_count_per_region

# Plot histogram of average climb count per crag for each region
ggplot(avg_climb_count_per_region, aes(x = reorder(Region_Name, -Avg_Climb_Count_per_Crag), y = Avg_Climb_Count_per_Crag)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogram of Average Climb Count per Crag for Each Region",
       x = "Region",
       y = "Average Climb Count per Crag") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#south-east england has highest average no of climbs per crag, then peak


###quantifying and visualising climbing intensity for gb
  
#calculate climbing intensity for each crag as log(climb_count)*log(total_ascents)
  # avoiding zero results by adding a small constant (1) as some crags have only one recorded route and log(1)=0
  climb_gb_totals_crag <- climb_gb_totals_crag %>%
  mutate(Climb_Intensity = log(Climb_Count + 1) * log(Total_Ascents + 1))
  

#Compute the maximum value of Climb_Intensity
max_climb_intensity <- max(climb_gb_totals_crag$Climb_Intensity, na.rm = TRUE)

#Add the scaled Climb_Intensity column from 0-100%
climb_gb_totals_crag <- climb_gb_totals_crag %>%
  mutate(Climb_Intensity_Scaled = (Climb_Intensity / max_climb_intensity) * 100)
  
  
##try alternative not logged - logging makes it so double ascents leads only 4% higher intensity
#calculate climbing intensity for each crag as climb_count*total_ascents
#climb_gb_totals_crag <- climb_gb_totals_crag %>%
#  mutate(Climb_Intensity2 = Climb_Count * Total_Ascents)


#Compute the maximum value of Climb_Intensity
#max_climb_intensity2 <- max(climb_gb_totals_crag$Climb_Intensity2, na.rm = TRUE)

#Add the scaled Climb_Intensity column from 0-100%
#climb_gb_totals_crag <- climb_gb_totals_crag %>%
#  mutate(Climb_Intensity_Scaled2 = (Climb_Intensity2 / max_climb_intensity2) * 100)
# Create a spatial points dataframe
#coordinates(climb_gb_totals_crag) <- ~lng + lat
#proj4string(climb_gb_totals_crag) <- CRS("+init=epsg:4326") # WGS 84


#save to import into arcgispro
#using EPSG:4326 (WGS 84)- a common CRS for global data using latitude and longitude.
climb_sf <- st_as_sf(climb_gb_totals_crag, coords = c("lng", "lat"), crs = 4326)

# Save as a shapefile
st_write(climb_sf, "climb_per_crag_totals.shp")


##create time series data frames with intensity and scaled intensity values per crag per year, and per month
#per crag per month
# Step 1: Filter out rows where Year is 2024
climb_gb_cleaned2 <- climb_gb_cleaned2 %>%
  filter(Year != 2024)

# Step 2: Calculate Climb_Intensity for each row
climb_gb_cleaned2 <- climb_gb_cleaned2 %>%
  mutate(Climb_Intensity = log(Climb_Count + 1) * log(Ascents + 1))

# Step 3: Compute the maximum value of Climb_Intensity in the dataset
max_climb_intensity <- max(climb_gb_cleaned2$Climb_Intensity, na.rm = TRUE)

# Step 4: Add the scaled Climb_Intensity column from 0-100%
climb_gb_cleaned2 <- climb_gb_cleaned2 %>%
  mutate(Climb_Intensity_Scaled = (Climb_Intensity / max_climb_intensity) * 100)

#per crag per year
# Step 1: Summarize ascents per crag per year
climb_gb_totals_crag_year <- climb_gb_cleaned2 %>%
  group_by(Crag_Id, Year) %>%
  summarise(Total_Ascents = sum(Ascents, na.rm = TRUE),
            Climb_Count = first(Climb_Count),  # Climb_Count is static, take the first occurrence
            .groups = 'drop')  # Dropping the grouping after summarising

# Step 2: Calculate Climb_Intensity using the static Climb_Count and Total_Ascents for each year
climb_gb_totals_crag_year <- climb_gb_totals_crag_year %>%
  mutate(Climb_Intensity = log(Climb_Count + 1) * log(Total_Ascents + 1))

# Step 3: Compute the maximum value of Climb_Intensity for scaling
max_climb_intensity_year <- max(climb_gb_totals_crag_year$Climb_Intensity, na.rm = TRUE)

# Step 4: Add the scaled Climb_Intensity column from 0-100%
climb_gb_totals_crag_year <- climb_gb_totals_crag_year %>%
  mutate(Climb_Intensity_Scaled = (Climb_Intensity / max_climb_intensity_year) * 100)

# Step 5: Join with the original dataset to include additional columns
climb_gb_totals_crag_year <- climb_gb_totals_crag_year %>%
  left_join(
    climb_gb_cleaned2 %>%
      dplyr::select(Crag_Id, Crag_Name, lat, lng, Rock_Type, Region_Name, Country) %>%
      distinct(Crag_Id, .keep_all = TRUE), 
    by = "Crag_Id"
  )

# Step 6: Remove any rows with Year 2024
climb_gb_totals_crag_year <- climb_gb_totals_crag_year %>%
  filter(Year != 2024)

# Reorder columns for better readability
climb_gb_totals_crag_year <- climb_gb_totals_crag_year %>%
  dplyr::select(Crag_Id, Crag_Name, Year, Total_Ascents, Climb_Count, Climb_Intensity, Climb_Intensity_Scaled, 
         lat, lng, Rock_Type, Region_Name, Country)

#save both to import into arcgispro
#using EPSG:4326 (WGS 84)- a common CRS for global data using latitude and longitude.
climb_byyear_sf <- st_as_sf(climb_gb_totals_crag_year, coords = c("lng", "lat"), crs = 4326)

# Save as a shapefiles
st_write(climb_byyear_sf, "climb_percragyeartotals.shp")

--------------------------------------------------------------------------------------------------------------------------------------
####SECTION 3: PLANT DATA PREP, CLEANING AND EDA####
# Read the occurrence.txt file
plant_data <- read_delim("Plants/occurrence.txt", delim = "\t", locale = locale(encoding = "UTF-8"))

# View the first few rows of the data
head(plant_data)
  

##CLEANING
#223 variables, need to cut it down so I can look at what I have
# Check for missing values in each column
na_counts <- sapply(plant_data, function(x) sum(is.na(x)))
na_counts

#remove columns that are 100% NA values - will be no use anyway
plant_data <- plant_data[, colSums(is.na(plant_data)) < nrow(plant_data)]
#down to 81 variables


#check it's all presence data (no absences)
count_not_present <- sum(plant_data$occurrenceStatus != "PRESENT", na.rm = TRUE)
print(count_not_present)
#0, yes it's all presence data

#Count the number of unique pairs of decimalLatitude and decimalLongitude
unique_locations_count <- plant_data %>%
  distinct(decimalLatitude, decimalLongitude) %>%
  nrow()
# Print the count of unique pairs
print(unique_locations_count)

#split into positive and negative indicators
# Define the list of verbatimScientificNames to filter for negative_plant_data - these are marked as negative indicators by npms
negative_names <- c("Agrostis capillaris", "Alopecurus myosuroides", "Anthriscus sylvestris", 
                    "Azolla filiculoides", "Brachypodium pinnatum", "Buddleja davidii", 
                    "Carpobrotus edulis", "Centranthus ruber", "Cirsium arvense", 
                    "Conifer seedlings / saplings", "Crassula helmsii", "Elodea canadensis", 
                    "Elodea nuttallii", "Elodea canadensis / nuttallii", "Galium aparine", 
                    "Glyceria maxima", "Heracleum mantegazzianum", "Impatiens glandulifera", 
                    "Juncus inflexus", "Juncus effusus", "Juncus conglomeratus", 
                    "Juncus inflexus / effusus / conglomeratus", "Lysichiton americanus", 
                    "Myriophyllum aquaticum", "Pteridium aquilinum", "Rhododendron ponticum", 
                    "Rosa rugosa", "Rubus fruticosus agg.", "Rumex obtusifolius", 
                    "Spartina anglica", "Stellaria media", "Symphoricarpos albus", 
                    "Ulex europaeus", "Urtica dioica")

# Filter the dataset to create negative_plant_data
negative_plant_data <- plant_data %>%
  filter(verbatimScientificName %in% negative_names)

##Some plants are in some habitats positive indicators and in others negative indicators
# How many observations of these species?
# Define the list of verbatimScientificNames to count
species_list <- c("Anthoxanthum odoratum", "Betula pubescens", "Betula pendula", 
                  "Betula pubescens / pendula", "Crataegus monogyna", 
                  "Deschampsia cespitosa", "Deschampsia flexuosa", "Hedera helix", 
                  "Prunus spinosa", "Ranunculus repens", "Rumex crispus", 
                  "Senecio jacobaea")



# Create positive_plant_data by excluding the negative_plant_data
positive_plant_data <- plant_data %>%
  filter(!verbatimScientificName %in% negative_names)
# Filter the positive_plant_data for the specified species and count the total number of observations
total_observations_positive <- positive_plant_data %>%
  filter(verbatimScientificName %in% species_list) %>%
  tally()

# Print the total number of observations
print(total_observations_positive)
#19960 ~10% of positive_plant_data

# also exclude species that can be positive or negative since habitat type isnt recorded in the dataset so cant split them out accurately
positive_plant_data <- positive_plant_data %>%
  filter(!verbatimScientificName %in% species_list)

#create subsets with only useful variables
positive_plant_data_subset <- subset(positive_plant_data, select = c('decimalLatitude', 'decimalLongitude', 'year', 'month', 'coordinateUncertaintyInMeters', 'scientificName', 'iucnRedListCategory', 'level2Name'))
negative_plant_data_subset <- subset(negative_plant_data, select = c('decimalLatitude', 'decimalLongitude', 'year', 'month', 'coordinateUncertaintyInMeters', 'scientificName', 'iucnRedListCategory', 'level2Name'))

#remove instances with high coordinate uncertainty
#visualise uncertainty in that column for +ve
hist(positive_plant_data_subset$coordinateUncertaintyInMeters, 
     main = "Histogram of Coordinate Uncertainty in Meters",
     xlab = "Coordinate Uncertainty in Meters",
     ylab = "Frequency",
     col = "blue",
     breaks = 100) # Number of bins
#most very small, a few very large, remove any with uncertainty over 1000m

positive_plant_data_subset_filtered <-positive_plant_data_subset[positive_plant_data_subset$coordinateUncertaintyInMeters <= 1000 | is.na(positive_plant_data_subset$coordinateUncertaintyInMeters), ]
#3393 rows removed

#remove uncertainty column
positive_plant_data_subset_filtered <- subset(positive_plant_data_subset_filtered, select = -coordinateUncertaintyInMeters)

#same for -ve dataset
#visualise uncertainty in that column for -ve
hist(negative_plant_data_subset$coordinateUncertaintyInMeters, 
     main = "Histogram of Coordinate Uncertainty in Meters",
     xlab = "Coordinate Uncertainty in Meters",
     ylab = "Frequency",
     col = "blue",
     breaks = 1000) # Number of bins
#similar to spread for +ve, most very small, a few very large, remove any with uncertainty over 1000m

negative_plant_data_subset_filtered <-negative_plant_data_subset[negative_plant_data_subset$coordinateUncertaintyInMeters <= 100 | is.na(negative_plant_data_subset$coordinateUncertaintyInMeters), ]
#577 rows removed

#remove uncertainty column
negative_plant_data_subset_filtered <- subset(negative_plant_data_subset_filtered, select = -coordinateUncertaintyInMeters)

#create time series dataset with monthly species richness for each location, and overall % change over study period
# Calculate species richness (unique scientificName count) for each location per month
Species_Richness <- positive_plant_data_subset_filtered %>%
  group_by(decimalLatitude, decimalLongitude, year, month) %>%
  summarise(species_richness = n_distinct(scientificName)) %>%
  ungroup()

# Calculate average species richness over the study period for each (lat, long) pair
average_species_richness <- species_richness %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  summarise(average_species_richness = mean(species_richness)) %>%
  ungroup()

# Calculate % change in species richness over the study period for each (lat, long) pair
# Find the start and end year-month for each (lat, long) pair
species_richness <- species_richness %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  mutate(start_year_month = min(year * 12 + month),
         end_year_month = max(year * 12 + month)) %>%
  ungroup()

# Get species richness at start and end of the study period
start_richness <- species_richness %>%
  filter((year * 12 + month) == start_year_month) %>%
  dplyr::select(decimalLatitude, decimalLongitude, start_richness = species_richness)

end_richness <- species_richness %>%
  filter((year * 12 + month) == end_year_month) %>%
  dplyr::select(decimalLatitude, decimalLongitude, end_richness = species_richness)

# Merge start and end richness data
richness_change <- start_richness %>%
  inner_join(end_richness, by = c("decimalLatitude", "decimalLongitude"))

# Calculate percentage change
richness_change <- richness_change %>%
  mutate(percentage_change = ((end_richness - start_richness) / start_richness) * 100) %>%
  dplyr::select(decimalLatitude, decimalLongitude, percentage_change)

# Create the final dataset with lat, long, average species richness, and % change in species richness
Positive_plant_final_dataset <- average_species_richness %>%
  inner_join(richness_change, by = c("decimalLatitude", "decimalLongitude"))

# Filter out records where the year is 2024
positive_plant_data_subset_filtered <- positive_plant_data_subset_filtered %>%
  filter(year != 2024)

# Calculate species richness (unique scientificName count) for each (lat, long, year)
species_richness_per_year <- positive_plant_data_subset_filtered %>%
  group_by(decimalLatitude, decimalLongitude, year) %>%
  summarise(species_richness = n_distinct(scientificName)) %>%
  ungroup()


##Again for negative plant data - not used in this analysis but steps included for completeness
# Calculate species richness (unique scientificName count) for each (lat, long, year, month)
#species_richness <- negative_plant_data_subset_filtered %>%
#  group_by(decimalLatitude, decimalLongitude, year, month) %>%
#  summarise(species_richness = n_distinct(scientificName)) %>%
#  ungroup()

# Calculate average species richness over the study period for each (lat, long) pair
#average_species_richness <- species_richness %>%
#  group_by(decimalLatitude, decimalLongitude) %>%
#  summarise(average_species_richness = mean(species_richness)) %>%
#  ungroup()

# Calculate % change in species richness over the study period for each (lat, long) pair
# Find the start and end year-month for each (lat, long) pair
#start_richness <- species_richness %>%
#  group_by(decimalLatitude, decimalLongitude) %>%
#  filter(year == min(year) & month == min(month)) %>%
#  ungroup() %>%
#  dplyr::select(decimalLatitude, decimalLongitude, start_richness = species_richness)

#end_richness <- species_richness %>%
#  group_by(decimalLatitude, decimalLongitude) %>%
#  filter(year == max(year) & month == max(month)) %>%
#  ungroup() %>%
#  dplyr::select(decimalLatitude, decimalLongitude, end_richness = species_richness)

# Merge start and end richness data
#richness_change <- start_richness %>%
#  inner_join(end_richness, by = c("decimalLatitude", "decimalLongitude"))

# Calculate percentage change
#richness_change <- richness_change %>%
#  mutate(percentage_change = ((end_richness - start_richness) / start_richness) * 100) %>%
#  dplyr::select(decimalLatitude, decimalLongitude, percentage_change)

# Create the final dataset with lat, long, average species richness, and % change in species richness
#Negative_plant_final_dataset <- average_species_richness %>%
#  inner_join(richness_change, by = c("decimalLatitude", "decimalLongitude"))

#convert both to sf and save as shapefiles
positive_plant_sf <- st_as_sf(Positive_plant_final_dataset, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
species_richness_per_year_sf<- st_as_sf(species_richness_per_year, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#negative_plant_sf <- st_as_sf(Negative_plant_final_dataset, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Save as a shapefile
st_write(positive_plant_sf, "positive_plants.shp")
st_write(species_richness_per_year_sf, "species_richness_peryear.shp")
#st_write(negative_plant_sf, "negative_plants.shp")


------------------------------------------------------------------------------------------------------------------------------------
### MOVE INTO ARCGIS AND FOLLOW STEPS 1-22 LAID OUT IN THE ADDITIONAL MATERIALS DOCUMENT
------------------------------------------------------------------------------------------------------------------------------------
####SECTION 4: CORRELATION ANALYSIS AND XGBOOST MODELLING
#import data for modelling
modelling_data <- read_csv("modelling_data.csv")

# Fill NA values in Clm_I_S column with 0s
modelling_data$Clm_I_S[is.na(modelling_data$Clm_I_S)] <- 0

#preliminary scatterplot of scaled climbing intensity against plant species richness
ggplot(modelling_data, aes(x = Clm_I_S, y = spcs_rc)) +
  geom_point(color = "steelblue", size = 1, alpha = 0.7) +  # Consistent point color and size
  theme_minimal(base_size = 12) +  # Increase base font size for readability
  labs(title = "Relationship between Species Richness and Scaled Climbing Intensity",
       x = "Scaled Climbing Intensity",
       y = "Species Richness")+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  # Bold and centered title
        axis.text = element_text(face = "bold"),  # Bold axis text
        axis.title = element_text(face = "bold"),  # Bold axis titles
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soften grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines


#create a binary column for whether a location is within a protected area
modelling_data <- modelling_data %>%
  mutate(Protected_Area = ifelse(!is.na(FIRST_NAME) | !is.na(AONB_NAME) | !is.na(NAME) | !is.na(NPARK21NM), 1, 0))

#merge protected area are columns into one (empty if not in a protected area)
modelling_data <- modelling_data %>%
  mutate(Protected_Area_Area = coalesce(HECTARES, AREA_HA_1, STAT_AREA))

#rename columns to make clean and improve graphics later
colnames(modelling_data) <- c("Object ID", "Year", "Species Richness", "Crag ID", "Crag Name", "Ascents", "Climb Count", "Climbing Intensity", "Climbing Intensity Scaled", "Area of Habitat parcel", "Habitat type", "Habitat name", "NSA Name", "NSA Area", "Welsh AONB Name", "Welsh AONB Area", "English AONB Name", "English AONB Area", "National Park Name", "Elevation", "Humidity", "Max Temp", "Min Temp", "Precipitation volume", "Latitude", "Longitude", "Is a protected area?", "Area of Protected Area")

#subset data to just numeric columns for modelling
modelling_subset_data <- modelling_data[, c(2, 3, 9:11, 20:28)]

#Explore dataset - pairwise relationships, correlation coefficients and P values
ggpairs(
  modelling_subset_data,
  diag = list(continuous = "densityDiag")     # Use density plots on the diagonal
) +
  ggtitle("Pairwise Relationships for Species Richness and potentially related variables ") +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust y-axis label size
    strip.text = element_text(size = 8),  # Adjust facet label size
    legend.position = "bottom"  # Move legend to the bottom
  )

# Select only numeric columns including "Species Richness"
numeric_data <- modelling_subset_data %>% 
  dplyr::select("Species Richness", everything()) %>% 
  dplyr::select_if(is.numeric)

# Initialize lists to store results
correlations <- numeric()
p_values <- numeric()

# Calculate correlation and p-value for each variable
for (var in colnames(numeric_data)) {
  if (var != "Species Richness") {
    # Calculate correlation coefficient
    corr_test <- cor.test(numeric_data$`Species Richness`, numeric_data[[var]])
    
    # Store results
    correlations[var] <- corr_test$estimate
    p_values[var] <- corr_test$p.value
  }
}

# Combine results into a data frame
results <- data.frame(
  Variable = names(correlations),
  Correlation = correlations,
  P_Value = p_values
)

# Print the results
print(results)


#convert data to DMatrix
#split into test and train data
set.seed(123)  # Set seed for reproducibility
train_indices <- sample(1:nrow(modelling_subset_data), 0.8 * nrow(modelling_subset_data))  # 80% training data
train_data <- modelling_subset_data[train_indices, ]
test_data <- modelling_subset_data[-train_indices, ]

# Separate features and target variable
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% dplyr::select(-"Species Richness")),
                            label = train_data$`Species Richness`)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% dplyr::select(-"Species Richness")),
                           label = test_data$`Species Richness`)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",  #For regression
  eta = 0.05,  # Low learning rate
  max_depth = 7,  # Moderate depth for complexity
  subsample = 0.7,  # Randomness in sampling
  colsample_bytree = 0.7,  # Randomness in feature selection
  min_child_weight = 5,  # Helps prevent overfitting
  gamma = 1,  # Adds a penalty for more conservative splits
  lambda = 1,  # L2 regularisation
  alpha = 0.1  # L1 regularisation
)

xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 300,  # Increased rounds due to lower eta
  watchlist = list(train = train_matrix, test = test_matrix),
  print_every_n = 10,
  early_stopping_rounds = 10
)

#evaluate model performance using RMSE
predictions <- predict(xgb_model, test_matrix)
rmse <- sqrt(mean((test_data$spcs_rc - predictions)^2))
print(paste("RMSE:", rmse))
#6.1239204

#interpret feature importance
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

# Calculate Percentage of Gain
importance_matrix <- importance_matrix %>%
  mutate(Percentage = (Gain / sum(Gain)) * 100)

# Print results
print(importance_matrix)

# Plot importance
xgb.plot.importance(importance_matrix)

#create nicer plot for report
# Assuming you already have the XGBoost model and importance matrix
# Get feature importance
importance_matrix <- xgb.importance(model = xgb_model)

# Convert the importance matrix to a data frame
importance_df <- importance_matrix %>%
  dplyr::mutate(Feature = reorder(Feature, Gain)) %>%  # Reorder by Gain for better visual
  dplyr::select(Feature, Gain) %>%
  dplyr::rename(Importance = Gain)

# Create the plot using ggplot2
importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Use consistent color
  coord_flip() +  # Flip coordinates to have horizontal bars
  theme_minimal(base_size = 14) +  # Increase base font size
  labs(title = "Feature Importance in XGBoost Model",
       subtitle = "Based on Gain Metric",
       x = "Features",
       y = "Importance (Gain)") +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold and centered title
        plot.subtitle = element_text(size = 14, hjust = 0.5),  # Centered subtitle
        axis.text = element_text(face = "bold"),  # Bold axis text
        axis.title = element_text(face = "bold"),  # Bold axis titles
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Soften grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# Print the plot
print(importance_plot)




