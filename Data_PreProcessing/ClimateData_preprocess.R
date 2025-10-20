## DWH additon to get climate data preprocessed

##first clean up airport csv

library(tidyverse)

roa <- read.csv("./Data_sources/NOAA_ROA/4043554_NOAA_ROAdaily_1jan1948_2jun2025.csv")
##units are in inches and F, need to convert to meters and C
df <- roa |>
  dplyr::select(DATE, PRCP, TMIN, TMAX) |>
  mutate(PRCP = PRCP/39.37,
         TMIN = ((TMIN-32) * (5/9)),
         TMAX = ((TMAX-32) * (5/9))
  )


#Function to structure to climate data in RHESSys format
#Note: the 1 is a place holder for hour but data is daily sequence. see 'Format for Time Series Input Files' from 'Climate-Inputs' link below

write_variable_file <- function(df, var_name, file_name) {
  # Ensure DATE is in Date format
  df$DATE <- as.Date(df$DATE)

  # Extract first date components without leading zeros
  first_date <- df$DATE[1]
  header <- paste(format(first_date, "%Y"),
                  as.integer(format(first_date, "%m")),
                  as.integer(format(first_date, "%d")),
                  "1")

  # Extract variable values only
  values <- df[[var_name]]

  # Format values as character
  value_lines <- as.character(values)

  # Write to file
  writeLines(c(header, value_lines), con = file_name)
}


# Write climate files from precip, tmin, and tmax
write_variable_file(df, "PRCP", "ClimateFiles/clim/ccr_daily.rain")
write_variable_file(df, "TMIN", "ClimateFiles/clim/ccr_daily.tmin")
write_variable_file(df, "TMAX", "ClimateFiles/clim/ccr_daily.tmax")




# from https://github.com/RHESSys/RHESSys/wiki/Climate-Inputs
# code for reading in RHESSys formatting climate data: https://github.com/RHESSys/RHESSysIOinR/wiki/Climate


## set up climate base file for CCR
library(RHESSysIOinR)

ccr_base <- IOin_clim(
  base_station_id = 101,
  x_coordinate = 100.0,
  y_coordinate = 100.0,
  z_coordinate = 346.7,
  effective_lai = 3.5,
  screen_height = 2,
  daily_prefix = "/clim/ccr_daily")


getwd()

write.table(ccr_base, file="ClimateFiles/clim/ccr_base", row.names=F, col.names=F, quote=F)


