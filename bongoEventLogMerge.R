###############################################################
################################################################################
#############          NES - LTER Cruise           #############################
#############             JUN-2024                 #############################
#############     NES BONGO EVENT DATASHEETS       #############################
## by: Alexandra Cabanelas 
################################################################################
# merging all available bongo event log datasheets 
# exporting for zooplankton inventory data package update
# 2018-2024 cruises

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(here)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(maps)
library(sf)
library(plotly)
library(tidyr)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##

# Location of CSV files
directory <- here("raw")

# List of CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv", full.names = TRUE)

# Read and clean each CSV file
read_and_clean_csv <- function(file) {
  df <- read_csv(file, na = "-")
  
  # List of columns to ensure are character type
  columns_to_convert <- c("cast", "DateUTC", "TimeInWaterUTC")
  
  for (col in columns_to_convert) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # Ensure all columns are of consistent type
  df[] <- lapply(df, function(x) {
    if (is.numeric(x) && any(is.na(as.numeric(x)))) {
      return(as.character(x))
    } else if (is.character(x)) {
      return(x)
    } else if (inherits(x, "time")) {
      return(as.character(x))
    } else {
      return(as.character(x))
    }
  })
  
  return(df)
}

# Merge all files
list_of_dataframes <- lapply(csv_files, read_and_clean_csv)

# Make df
combined_dataframe <- bind_rows(list_of_dataframes)

# ring net only: AR31A, AR39B, AR34B, AR28B, AR66B, AR61B

class(combined_dataframe)
combined_dataframe <- as.data.frame(combined_dataframe)

## ------------------------------------------ ##
#            Data Wrangling -----
## ------------------------------------------ ##

# Remove empty rows
combined_dataframe <- combined_dataframe %>% 
  filter(!is.na(cast) & cast != "")

# Replace all text 'NA' values with R's NA
combined_dataframe[combined_dataframe == "NA"] <- NA
# Replace all empty strings with NA
combined_dataframe[combined_dataframe == ""] <- NA

# rename longitude column from long to lon 
combined_dataframe <- combined_dataframe %>%
  rename(Time_start_UTC = TimeInWaterUTC,
         Time_end_UTC = TimeOutWaterUTC,
         lon = long, # latitude_start_decdeg , longitude_start_decdeg
         depth_bottom = bot_depth,
         depth_target = target_depth,
         depth_TDR = TDRdepth, 
         FlowMeterSerial_335 = "335FlowMeterNum", #not a good practice to have col name start with num
         FlowStart_335 = "335FlowStart",
         FlowEnd_335 = "335FlowEnd",
         TotFlow_335 = "335TotFlow",
         Vol_Filtered_m3_335 = "335Volume_filteredm3",
         NOAA_335 = "335_NOAA",
         DNA_335 = "335_DNA",
         FlowMeterSerial_150 = "150FlowMeterNum",
         FlowStart_150 = "150FlowStart",
         FlowEnd_150 = "150FlowEnd",
         TotFlow_150 = "150TotFlow",
         Vol_Filtered_m3_150 = "150Volume_filteredm3",
         MorphID_150 = "150_MorphID",
         DNA_150 = "150_DNA",
         SizeFract_150 = "150_SizeFract",
         TaxaPick_150 = "150_TaxaPicking")

# Convert time to "hh:mm:ss" format to maintain consistency 
combined_dataframe$Time_start_UTC <- sapply(combined_dataframe$Time_start_UTC, function(time) {
  if (is.na(time)) {
    return(NA)
  } else if (nchar(time) == 5) { # Format hh:mm
    return(format(parse_date_time(time, "HM"), "%H:%M:%S"))
  } else if (nchar(time) == 8) { # Format hh:mm:ss
    return(time)
  } else {
    return(NA) # Handle unexpected format
  }
})

combined_dataframe$Time_end_UTC <- sapply(combined_dataframe$Time_end_UTC, function(time) {
  if (is.na(time)) {
    return(NA)
  } else if (nchar(time) == 5) { # Format hh:mm
    return(format(parse_date_time(time, "HM"), "%H:%M:%S"))
  } else if (nchar(time) == 8) { # Format hh:mm:ss
    return(time)
  } else {
    return(NA) # Handle unexpected format
  }
})

combined_dataframe <- combined_dataframe %>%
  mutate(
    DateUTC = as.numeric(DateUTC),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    depth_bottom = as.numeric(depth_bottom),
    depth_target = as.numeric(depth_target),
    avg_angle = as.numeric(avg_angle),
    depth_TDR = as.numeric(depth_TDR),
    FlowMeterSerial_335 = as.numeric(FlowMeterSerial_335),
    FlowStart_335 = as.numeric(FlowStart_335),
    FlowEnd_335 = as.numeric(FlowEnd_335),
    TotFlow_335 = as.numeric(TotFlow_335),
    Vol_Filtered_m3_335 = as.numeric(Vol_Filtered_m3_335),
    FlowMeterSerial_150 = as.numeric(FlowMeterSerial_150),
    FlowStart_150 = as.numeric(FlowStart_150),
    FlowEnd_150 = as.numeric(FlowEnd_150),
    TotFlow_150 = as.numeric(TotFlow_150),
    Vol_Filtered_m3_150 = as.numeric(Vol_Filtered_m3_150)
  )

## ------------------------------------------ ##
#            QA/QC -----
## ------------------------------------------ ##

colnames(combined_dataframe)
str(combined_dataframe)

unique(combined_dataframe$cruise)
unique(combined_dataframe$station)
unique(combined_dataframe$cast)
unique(combined_dataframe$sample_name)
unique(combined_dataframe$FlowMeterSerial_335)
unique(combined_dataframe$MorphID_150)
unique(combined_dataframe$DNA_150)
unique(combined_dataframe$SizeFract_150)
unique(combined_dataframe$TaxaPick_150)
unique(combined_dataframe$EtOHchanged)

combined_dataframe %>%
  summarise(
    min_DateUTC = min(DateUTC, na.rm = TRUE),
    max_DateUTC = max(DateUTC, na.rm = TRUE),
    min_lat = min(lat, na.rm = TRUE),
    max_lat = max(lat, na.rm = TRUE),
    min_lon = min(lon, na.rm = TRUE),
    max_lon = max(lon, na.rm = TRUE),
    min_depth_bottom = min(depth_bottom, na.rm = TRUE),
    max_depth_bottom = max(depth_bottom, na.rm = TRUE),
    min_depth_target = min(depth_target, na.rm = TRUE),
    max_depth_target = max(depth_target, na.rm = TRUE),
    min_avg_angle = min(avg_angle, na.rm = TRUE),
    max_avg_angle = max(avg_angle, na.rm = TRUE),
    min_depth_TDR = min(depth_TDR, na.rm = TRUE),
    max_depth_TDR = max(depth_TDR, na.rm = TRUE),
    min_FlowStart_335 = min(FlowStart_335, na.rm = TRUE),
    max_FlowStart_335 = max(FlowStart_335, na.rm = TRUE),
    min_FlowEnd_335 = min(FlowEnd_335, na.rm = TRUE),
    max_FlowEnd_335 = max(FlowEnd_335, na.rm = TRUE),
    min_TotFlow_335 = min(TotFlow_335, na.rm = TRUE),
    max_TotFlow_335 = max(TotFlow_335, na.rm = TRUE),
    min_Vol_Filtered_m3_335 = min(Vol_Filtered_m3_335, na.rm = TRUE),
    max_Vol_Filtered_m3_335 = max(Vol_Filtered_m3_335, na.rm = TRUE),
    min_FlowStart_150 = min(FlowStart_150, na.rm = TRUE),
    max_FlowStart_150 = max(FlowStart_150, na.rm = TRUE),
    min_FlowEnd_150 = min(FlowEnd_150, na.rm = TRUE),
    max_FlowEnd_150 = max(FlowEnd_150, na.rm = TRUE),
    min_TotFlow_150 = min(TotFlow_150, na.rm = TRUE),
    max_TotFlow_150 = max(TotFlow_150, na.rm = TRUE),
    min_Vol_Filtered_m3_150 = min(Vol_Filtered_m3_150, na.rm = TRUE),
    max_Vol_Filtered_m3_150 = max(Vol_Filtered_m3_150, na.rm = TRUE),
  ) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
  print(n = Inf)

## ------------------------------------------ ##
#            QA/QC coordinates -----
## ------------------------------------------ ##

# Convert your data frame to an sf object (Spatial DataFrame)
sf_data <- st_as_sf(combined_dataframe, 
                    coords = c("lon", "lat"), 
                    crs = 4326, 
                    agr = "constant")

# Map
ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(38, 44)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")


ggplot() +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_sf(data = sf_data, color = "blue", size = 1) +    # Add your points
  coord_sf(xlim = c(-72, -69), ylim = c(41, 43)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")



# Interactive maps

sf_data2 <- cbind(sf_data, st_coordinates(sf_data))

p <- ggplot(sf_data2, aes(x = X, y = Y, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(color = "blue", size = 3) +                 # Add your points
  coord_sf(xlim = c(-72, -69), ylim = c(41, 43)) +       # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

#interactive
ggplotly(p, tooltip = "text")

p1 <- ggplot(sf_data2, aes(x = X, y = Y, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(color = "blue", size = 3) +                 # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(38, 44)) +     # Set limits for the map
  theme_minimal() +
  labs(x = "lon",
       y = "lat")

#interactive
ggplotly(p1, tooltip = "text")


p2 <- ggplot(sf_data2, aes(x = X, y = Y, color = station, text = paste("Sample:", sample_name))) +
  borders("world", colour = "gray85", fill = "gray80") + # Base world map
  geom_point(size = 3) +                                 # Add your points
  coord_sf(xlim = c(-74, -68), ylim = c(39, 43)) +       # Set limits for the map
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude")

#interactive
ggplotly(p2, tooltip = "text")

# EN655 L6, AR34B L10, EN649 L6, EN644 L11 seem off but double checked on elog

#write.csv(combined_dataframe, "NES_LTER_TOW_DATA_2024.csv")