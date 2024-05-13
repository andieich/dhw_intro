#Functions and packages
library(tidyverse)
library(httr)

# Function to compare a value with a vector. 
# Returns the value of the vector that is closest to the input vector. 
# Is used to get the ERDDAP coordinates that are closest to the coordinates that the user inserted

find_closest <- function(val, vec){
  vec[which.min(abs(vec-val))]
}

# Formats filezise in an readable way. 
# Input is filesize in bytes. The function transforms it, depending on size, in KB, MB, GB, etc
file_size_formated <- function(size){
  
  k = size/1024.0 ^ 1
  m = size/1024.0 ^ 2
  g = size/1024.0 ^ 3
  t = size/1024.0 ^ 4
  
  if (t > 1) {
    outSize = paste(round(t,2),"TB")
  } else if (g > 1) {
    outSize = paste(round(g,2),"GB")
  } else if (m > 1) {
    outSize = paste(round(m,2),"MB")
  } else if (k > 1) {
    outSize = paste(round(k,2),"KB")
  } else{
    outSize = paste(round(size,2),"B")
  }
  
  return(outSize)
}


# Function to generate the ERDDAP URL
# Inputs are:
# lat1: Start latitude in °
# lat2: End latitude in °
# stride_lat: Stride for latitude: if 1: every value will be downloaded, if 2 every second value, etc. 
# Spatial resolution for DHW data is 0.05° and for MUR data 0.01°
# lon1: Start longitude in °
# lon2: End longitude in °
# stride_lon: Stride for longitude: See stride_lat for details
# day1: Start day
# day2: End day
# stride_day: Stride for date values: if 1, data for every day will be downloaded, if 2, data for every second day, etc
# server_type: Server from which data will be downloaded. For CRW DHW either "PacIOOS" or "CoastWatch", or "MUR" for MUR SST


make_url <- function(lat1,
                     lat2,
                     stride_lat,
                     lon1,
                     lon2,
                     stride_lon,
                     day1,
                     day2,
                     stride_day,
                     server_type,
                     parameter){
  
  if(server_type == "PacIOOS"){
    
    base_url <- "https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.csv?"
    if(is.na(parameter)){ parameter <- "CRW_DHW"}
    
  } else if (server_type == "CoastWatch"){
    
    base_url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.csv?"
    if(is.na(parameter)){ parameter <- "CRW_DHW"}
    
  } else if (server_type == "MUR"){
    
    base_url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.csv?"
    if(is.na(parameter)){ parameter <- "analysed_sst"}
    
  } else {stop("server_type not known. Has to be PacIOOS or CoastWatch for CRW DHW data or MUR")}
  
  url <- paste0(base_url,
                parameter,
                "[(",
                day1, "):",
                stride_day, ":(",
                day2, ")][(",
                
                lat1, "):",
                stride_lat, ":(",
                lat2, ")][(",
                
                lon1, "):",
                stride_lon, ":(",
                lon2, ")]"
  )
  
  return(url)
  
}

# This function uses the generated url to download ERDDAP data.
# Most input parameters are described in the other functions
# path: If set destination of downloads. If not set, a temporary file will be generated
# timeout: timeout for downloads in seconds. Default is 10 mins

download_erddap_csv <- function(lat,
                                lon,
                                day1,
                                day2,
                            server_type,
                            parameter = NA,
                            path = NA,
                            timeout = 600){
  
  lat1 <- lat
  lat2 <- lat
  stride_lat <- 1
  
  lon1 <- lon
  lon2 <- lon
  stride_lon <- 1
  
  stride_day <- 1
  
  # make path
  if(is.na(path)){
    path <- tempfile(fileext = ".csv")
    print(paste("Data will be downloaded to", path))
  }
  
  #for crw
  if (server_type == "PacIOOS" | server_type == "CoastWatch"){
    
    lat_s <- seq(-89.975, 89.975, by = 0.05) %>% round(3)
    lon_s <- seq(-179.975, 179.975, by = 0.05) %>% round(3)
    
  } else if (server_type == "MUR"){
    lat_s <- seq(-89.99, 89.99, by = 0.01) %>% round(2)
    lon_s <- seq(-179.99, 180, by = 0.01) %>% round(2)
    
  }
  
  lat1 <- find_closest(lat1, lat_s)
  lat2 <- find_closest(lat2, lat_s)
  
  lon1 <- find_closest(lon1, lon_s)
  lon2 <- find_closest(lon2, lon_s)
  
  estimate_filesize(day1 = day1,
                    day2 = day2)
  
  url <- make_url(lat1 = lat1,
                  lat2 = lat2,
                  stride_lat = stride_lat,
                  lon1 = lon1,
                  lon2 = lon2,
                  stride_lon = stride_lon,
                  day1 = day1,
                  day2 = day2,
                  stride_day = stride_day,
                  server_type = server_type,
                  parameter = parameter)
  
  print(paste("Will download from", url))
  
  download <- httr::GET(url, 
                        httr::write_disk(path, 
                                         overwrite = T),
                        httr::add_headers('Accept-Encoding' = 'gzip, deflate'),
                        httr::progress("down"),
                        httr::timeout(timeout))
  
  
  stop_for_status(download)
  
}


# This function estimates the file size of .nc data.
# Input parameters are described in the other functions

estimate_filesize <- function(day1,
                              day2){
  

  
  day1 <- as.Date(day1)
  day2 <- as.Date(day2)
  
  n_day <- round(abs(day2 - day1),0) + 1 %>%
    as.integer()
  
  
  est_fz_per_day <- 46.52388
  fz_per_file <- 74.15293
  
  est_fz <- fz_per_file + est_fz_per_day*n_day
  
  print(paste("The estimated filesize is", 
              file_size_formated(est_fz)))
  
  print(paste("Will download ", n_day, "day vals"))
  
}


