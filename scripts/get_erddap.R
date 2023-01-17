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
    
    base_url <- "https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?"
    if(is.na(parameter)){ parameter <- "CRW_DHW"}

  } else if (server_type == "CoastWatch"){
    
    base_url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.nc?"
    if(is.na(parameter)){ parameter <- "CRW_DHW"}
    
  } else if (server_type == "MUR"){
    
    base_url <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?"
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

download_erddap <- function(lat1,
                      lat2,
                      stride_lat = 1,
                      lon1,
                      lon2,
                      stride_lon = 1,
                      day1,
                      day2,
                      stride_day = 1,
                      server_type,
                      parameter = NA,
                      path = NA,
                      timeout = 600){

  # make path
  if(is.na(path)){
    path <- tempfile(fileext = ".nc")
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

  estimate_filesize(lat1 = lat1,
              lat2 = lat2,
              stride_lat = stride_lat,
              lon1 = lon1,
              lon2 = lon2,
              stride_lon = stride_lon,
              day1 = day1,
              day2 = day2,
              stride_day = stride_day,
              server_type = server_type)
  
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
  #print(download)
  
}

# This function estimates the file size of .nc data.
# Input parameters are described in the other functions

estimate_filesize <- function(lat1,
                        lat2,
                        stride_lat,
                        lon1,
                        lon2,
                        stride_lon,
                        day1,
                        day2,
                        stride_day,
                        server_type){
  
  
  if (server_type == "PacIOOS" | server_type == "CoastWatch"){
    
    n_lat <- round(1+abs(lat2 - lat1)/(0.05 * stride_lat),0) %>%
      as.integer()
    
    n_lon <- round(1+abs(lon2 - lon1)/(0.05 * stride_lon),0) %>%
      as.integer()
    
  } else if(server_type == "MUR"){
    
    n_lat <- round(1+abs(lat2 - lat1)/(0.01 * stride_lat),0) %>%
      as.integer()
    
    n_lon <- round(1+abs(lon2 - lon1)/(0.01 * stride_lon),0) %>%
      as.integer()
    
  }
  
  day1 <- as.Date(day1)
  day2 <- as.Date(day2)
  
  n_day <- round(abs(day2 - day1)/stride_day,0) + 1 %>%
    as.integer()
  
  
  est_fz_per_day <- n_lon*n_lat*8 +#8 bytes per value
    8 +#float64 for each day
    4*(n_lat + n_lon)
  
  est_fz <- est_fz_per_day*n_day
  
  print(paste("The estimated filesize per day is",
      file_size_formated(est_fz_per_day),
      " and", file_size_formated(est_fz),
      "in total"))
  
  print(paste("Will download ", n_lat, "lat vals,",
      n_lon, "lon vals, and",
      n_day, "day vals"))

}



#Read .nc files


# Reads separate files, if lat/lon borders are given, filters the data
# The raster package is used to read .nc files and transforms them into a data.frame
# Inputs
# path: path to .nc file
# lat_min: lower latitude border. If set, will be included
# lat_max: upper latitude border. If set, will be included
# lon_min: lower longitude border. If set, will be included
# lon_max: upper longitude border. If set, will be included
# varname used in .nc file. 
# values_to:  column name for varname

read_and_format <- function(path, 
                            lat_min = NA,
                            lat_max = NA,
                            lon_min = NA,
                            lon_max = NA,
                            varname = varname,
                            values_to = values_to){
  
  
  dat <- raster::brick(path,
                       varname = varname) %>% 
    raster::as.data.frame(xy = T) %>% 
    pivot_longer(3:ncol(.), names_to = "date", values_to = values_to) %>% 
    mutate(date = substr(date,2,11)) %>% 
    mutate(date = as.Date(lubridate::fast_strptime(date, "%Y.%m.%d"))) %>% 
    mutate(lat = round(y,3),
           lon = round(x, 3)) %>% 
    dplyr::select(-x, -y)
  
  
  if(!is.na(lat_min) & !is.na(lat_max)){
    
    if(lat_min > lat_max){
      lat_tmp <- lat_min
      lat_min <- lat_max
      lat_max <- lat_tmp}
    
    dat <- dat %>% 
      filter(lat >= lat_min, lat <= lat_max)
  }
  
  if(!is.na(lon_min) & !is.na(lon_max)){
    
    if(lon_min > lon_max){
      lon_tmp <- lon_min
      lon_min <- lon_max
      lon_max <- lon_tmp}
    
    dat <- dat %>% 
      filter(lon >= lon_min, lon <= lon_max)
  }
  
  return(dat)
}


# Helper function to get file extension. Used to check if input file is really .nc

getExtension <- function(path){
  ext <- strsplit(path, "\\.")[[1]]
  ext <-tail(ext, 1)
  return(ext)
} 


# Reads .nc files
# Can handle folders or separate files
# if lat/lon _min/_max is set, data will be filtered (including borders)
# if path is folder, all .nc files will be combined

read_nc <- function(path,
                    lat_min = NA,
                    lat_max = NA,
                    lon_min = NA,
                    lon_max = NA,
                    varname = "CRW_DHW",
                    values_to = "dhw"){
  
  library(pbapply) #with progress bar, or use "normal" lapply
  library(raster) #DON'T USE terra PACKAGE!
  library(lubridate)
  
  if(dir.exists(path)){
    
    files <- list.files(path = path,
                        pattern = "\\.nc$", 
                        full.names = T)
    
    if(length(files) == 0){
      stop("Folder does not contain .nc files")
    } else {
      files <- list.files(path = path,
                          pattern = "\\.nc$", 
                          full.names = T)
      
      print(paste("Will read", length(files), "files from folder"))
      
      dat <- do.call(rbind,
                     pbapply::pblapply(files, 
                                       read_and_format, 
                                       lat_min, 
                                       lat_max, 
                                       lon_min, 
                                       lon_max,
                                       varname,
                                       values_to))
      
    }
    
  } else if(file.exists(path)){
    
    if(getExtension(path) != "nc"){
      stop("File is no .nc file")
    } else {
      
      print("Will read one file")
      dat <-  read_and_format(path = path,
                              lat_min = lat_min,
                              lat_max = lat_max,
                              lon_min = lon_min,
                              lon_max = lon_max,
                              varname = varname,
                              values_to = values_to)
    }
    
  } else {
    stop("File or folder not found")
  }
  
  return(dat)
  
}
