#get functions
library(devtools)
library(here)


source_url("https://raw.githubusercontent.com/andieich/dhw_intro/main/get_erddap.R")


#download DHW data
download_erddap(lat1 = -17.46,
                lat2 = -17.5,
                lon1 = -149.81,
                lon2 = -149.85,
                day1 = "2019-01-01",
                day2 = "2019-12-31",
                server_type = "CoastWatch", #or "CoastWatch"
                path = here("data", "moorea_2019.nc"),
                parameter = "CRW_SST"
                ) #temp file if not set

download_erddap(lat1 = -17.46,
                lat2 = -17.5,
                lon1 = -149.81,
                lon2 = -149.85,
                day1 = "2016-01-01",
                day2 = "2016-12-31",
                server_type = "CoastWatch", #or "CoastWatch"
                path = here("data", "moorea_2016.nc"),
                parameter = "CRW_SST"
) #temp file if not set


moorea_sst_2019_crw <- read_nc(here("data", "moorea_2019.nc"), varname = "CRW_SST", values_to = "sst_crw")
moorea_sst_2016_crw <- read_nc(here("data", "moorea_2016.nc"), varname = "CRW_SST", values_to = "sst_crw")

moorea_sst_2019_crw <- moorea_sst_2019_crw %>%
  filter(lat == -17.475, lon == -149.825)

moorea_sst_2016_crw <- moorea_sst_2016_crw %>%
  filter(lat == -17.475, lon == -149.825)


moorea_sst_crw <- bind_rows(moorea_sst_2016_crw, moorea_sst_2019_crw) %>% 
  mutate(yr = format(date, format = "%Y"))  %>% 
  mutate(month = format(date, format = "%m"))

moorea_sst_crw <- moorea_sst_crw %>% 
  filter(month == "04") %>% 
  filter(date >= as.Date("2016-04-06") & date <= as.Date("2016-04-13") |
           date >= as.Date("2019-04-11") & date <= as.Date("2019-04-18"))

moorea_sst_lter <- read.csv(here("data", "MCR_LTER02_BottomMountThermistors_20220304.csv"))

library(lubridate)

moorea_sst_lter <- moorea_sst_lter %>% 
 # filter(sensor_depth_m == 10) %>% 
  mutate(date = as.Date(substr(time_local, 1, 10))) %>% 
  mutate(date_time = ymd_hms(time_local)) %>% 
  mutate(yr = format(date, format = "%Y")) %>% 
  filter(yr %in% c(2016, 2019)) %>% 
  mutate(month = format(date, format = "%m"))

moorea_sst_lter <- moorea_sst_lter %>% 
  filter(month == "04") %>% 
  filter(date >= as.Date("2016-04-06") & date <= as.Date("2016-04-12") |
         date >= as.Date("2019-04-11") & date <= as.Date("2019-04-17"))
  
  
library(viridis)
fig_insitu <- ggplot(moorea_sst_lter %>% filter(sensor_depth_m %in% c(1, 20)) , aes( x = date_time))+
  geom_hline(yintercept = 29.8, col = "grey", linetype = "11")+
  geom_line(aes(y = temperature_c, col = factor(sensor_depth_m)), alpha = .7)+
  scale_colour_okabe_ito(name = "Depth (m)") +
  geom_line( data = moorea_sst_crw, aes(y = sst_crw, x = as.POSIXct(date)), col = "#D55E00", lwd = 1)+
  facet_wrap(~factor(yr), nrow = 2, scales = "free_x")+
  labs(x = NULL, y = "Temperature (°C)")+
  theme_minimal()

fig_insitu

saveRDS(fig_insitu, here("plots", "fig_insitu.rds"))


# ---

#download area
#-17.461259547571736, -149.98095875518965
#-17.61175558267374, -149.7342084647527


download_erddap(lat1 = -17.46,
                lat2 = -17.61,
                lon1 = -149.98,
                lon2 = -149.73,
                day1 = "2019-04-15",
                day2 = "2019-04-15",
                server_type = "CoastWatch", #or "CoastWatch"
                path = here("data", "moorea_area_crw.nc"),
                parameter = "CRW_DHW"
) #temp file if not set

moorea_area <- read_nc(here("data", "moorea_area_crw.nc"))


library(sf)

map_FrenchPolynesia <- st_read(dsn = here("maps", "polynesia", "PYF_adm0.shp")) %>% 
  st_as_sf()


moorea_map <- ggplot() +
  geom_raster(data = moorea_area,
              aes(y = as.numeric(lat),
                  x = as.numeric(lon),
                  fill = as.numeric(dhw)))+
  geom_sf(data = map_FrenchPolynesia, 
          fill = "antiquewhite", 
          col = "black")+
  coord_sf(xlim = c(-150, -149.7), 
           ylim = c(-17.45, -17.65), 
           expand = FALSE)+
  scale_fill_viridis(name = "DHW (°C weeks)")+
  theme_minimal()+
  labs(x=NULL, y = NULL, title = "2019-04-15")
moorea_map
saveRDS(moorea_map, here("plots", "moorea_map.rds"))


# --- Dixon 1km MMM vs CRW MMM with MUR SST

dat_comparison_mmm <- readRDS(here("data", "dat_comparison_mmm.rds"))

# compare
plot_mmm_comparison <- dat_comparison_mmm %>% 
  ggplot(aes(x = mmm, y = mmm_dixon))+
  geom_abline(slope = 1, intercept = 0, col = "grey")+
  geom_point(shape = 1)+
  labs(x = "MMM (°C) | CRW", y = "MMM (°C) | Dixon et al. 2022")+
  lims(x = c(28.6, 29), y = c(28.6, 29))+
  theme_minimal()

saveRDS(plot_mmm_comparison, here("plots", "plot_mmm_comparison.rds"))


### --- calculate MUR DHW based on Dixon and CRW data for 15.04.2019
as.Date("2019-04-15")-84#2019-01-21

#download MUR SST 
download_erddap(lat1 = -17.46,
                lat2 = -17.61,
                lon1 = -149.98,
                lon2 = -149.73,
                day1 = "2019-01-21",
                day2 = "2019-04-15",
                server_type = "MUR", #or "CoastWatch"
                path = here("data", "moorea_area_mur.nc"))


moorea_area_mur <- read_nc(here("data", "moorea_area_mur.nc"), varname = "analysed_sst",values_to =  "sst")

# make mmm dataset

moorea_area_mmm_crw <- read_nc(here("data", "ct5km_climatology_v3.1.nc"), varname = "sst_clim_mmm",values_to =  "mmm_crw")

moorea_area_mmm_crw <- moorea_area_mmm_crw %>% 
  mutate(lat_orig = lat, lon_orig = lon) %>% 
  filter(lat > -17.65, lat < -17.4,
         lon > -150, lon < -149.5) %>% 
  dplyr::select(-date)
  





moorea_area_mmm_dixon <- read.csv(here("data", "HighResCoralStress_metrics_observed_Polynesia_1985_2019_v2.0.csv")) %>% 
  dplyr::select(latitude, longitude, mmm) %>% 
  rename(lat = latitude, 
         lon = longitude,
         mmm_dixon = mmm) %>% 
  filter(lat > -17.65, lat < -17.4,
         lon > -150, lon < -149.5) 

crw_lat_s <- seq(-89.975, 89.975, by = 0.05) %>% round(3)
crw_lon_s <- seq(-179.975, 179.975, by = 0.05) %>% round(3)




moorea_area_mmm_dixon <- moorea_area_mmm_dixon %>%
  mutate(lat_crw = round(as.numeric(pblapply(lat, find_closest, crw_lat_s)), 3),
         lon_crw = round(as.numeric(pblapply(lon, find_closest, crw_lon_s)), 3))


#combine mmm
moorea_area_mmm <- moorea_area_mmm_dixon %>% 
  left_join(moorea_area_mmm_crw %>% 
              rename(lat_crw = lat, 
                     lon_crw = lon), by = c("lat_crw", "lon_crw")) %>% 
  dplyr::select(-lat_orig, -lon_orig, -lat_crw, -lon_crw)
  

library(zoo)
#calculate DHW
moorea_area_mur_dhw <- moorea_area_mur %>% 
  left_join(moorea_area_mmm,
            by = c("lat", "lon")) %>% 
  mutate(hot_spot_dixon = sst - mmm_dixon,
         hot_spot_crw = sst - mmm_crw) %>% 
  mutate(hot_spot_dixon = ifelse(hot_spot_dixon >= 1, hot_spot_dixon, 0),
         hot_spot_crw = ifelse(hot_spot_crw >= 1, hot_spot_crw, 0)) %>% 
  group_by(lat,lon) %>% 
  arrange(date) %>%
  mutate(dhw_dixon = c(rep(NA, 12*7-1), rollapply(hot_spot_dixon/7, 12*7, sum, partial = F)),
         dhw_crw = c(rep(NA, 12*7-1), rollapply(hot_spot_crw/7, 12*7, sum, partial = F))) %>% #for 12 weeks
  mutate(dhw_dixon = round(dhw_dixon,2),
         dhw_crw = round(dhw_crw,2)) %>% 
  filter(date == as.Date("2019-04-15"))




map_moorea_dhw_crw <- ggplot() +
  geom_raster(data = moorea_area_mur_dhw %>% 
                filter(!is.na(dhw_crw)),
              aes(y = as.numeric(lat),
                  x = as.numeric(lon),
                  fill = as.numeric(dhw_crw)))+
  geom_sf(data = map_FrenchPolynesia, 
          fill = "antiquewhite", 
          col = "black")+
  coord_sf(xlim = c(-150, -149.7), 
           ylim = c(-17.45, -17.65), 
           expand = FALSE)+
  scale_fill_viridis(name = "DHW (°C weeks)", limits = c(1, 3.3))+
  theme_minimal()+
  labs(x=NULL, y = NULL, title = "CRW MMM")
map_moorea_dhw_crw

map_moorea_dhw_dixon <- ggplot() +
  geom_raster(data = moorea_area_mur_dhw %>% 
                filter(!is.na(dhw_dixon)),
              aes(y = as.numeric(lat),
                  x = as.numeric(lon),
                  fill = as.numeric(dhw_dixon)))+
  geom_sf(data = map_FrenchPolynesia, 
          fill = "antiquewhite", 
          col = "black")+
  coord_sf(xlim = c(-150, -149.7), 
           ylim = c(-17.45, -17.65), 
           expand = FALSE)+
  scale_fill_viridis(name = "DHW (°C weeks)", limits = c(1, 3.3))+
  theme_minimal()+
  labs(x=NULL, y = NULL, title = "Dixon et al. MMM")
map_moorea_dhw_dixon



saveRDS(map_moorea_dhw_dixon, here("plots", "map_moorea_dhw_dixon.rds"))

saveRDS(map_moorea_dhw_crw, here("plots", "map_moorea_dhw_crw.rds"))

#### - compare DHW DHD


#for dhw 

as.Date("2019-04-01")-84#"2019-01-07"

download_erddap(lat1 = -17.46,
                lat2 = -17.5,
                lon1 = -149.81,
                lon2 = -149.85,
                day1 = "2018-10-01",
                day2 = "2019-07-31",
                server_type = "CoastWatch", #or "CoastWatch"
                parameter = "CRW_SST",
                path = here("data", "moorea_area_sst_for_dhw.nc"))

moorea_dhw_dhd <- read_nc(here("data", "moorea_area_sst_for_dhw.nc"), varname = "CRW_SST",values_to =  "sst")

moorea_dhw_dhd <- moorea_dhw_dhd %>%
  filter(lat == -17.475, lon == -149.825)

moorea_dhw_dhd <- moorea_dhw_dhd %>% 
  left_join(moorea_area_mmm_crw, by = c("lat", "lon")) %>% 
  dplyr::select(-lat_orig, -lon_orig)



moorea_dhw_dhd <- moorea_dhw_dhd %>% 
  mutate(hot_spot = sst - mmm_crw) %>% 
  mutate(hot_spot = ifelse(hot_spot >= 1, hot_spot, 0)) %>% 
  group_by(lat,lon) %>% 
  arrange(date) %>%
  mutate(DHW = c(rep(NA, 12*7-1), rollapply(hot_spot/7, 12*7, sum, partial = F)),
         DHD = c(rep(NA, 12-1), rollapply(hot_spot, 12, sum, partial = F))) %>% 
  filter(date > as.Date("2019-02-01"))

plot_sst_dhd <- ggplot(data = moorea_dhw_dhd, aes(x = date))+
  geom_hline(aes(yintercept = mmm_crw), col = "grey", linetype = "11")+
  geom_hline(aes(yintercept = mmm_crw + 1), col = "grey")+
  geom_line(aes(y = sst), col = "#4E84C4")+
  labs (x = NULL, y = "SST (°C)")+
  theme_minimal()
plot_sst_dhd

plot_dhw_dhd <- ggplot(data = moorea_dhw_dhd, aes(x = date))+
  geom_area(aes(y = DHD), fill = "#52854C", alpha = .6)+
  annotate(x = as.Date("2019-04-28"), y = 10, label = "DHD", col = "#52854C", geom = "text")+
  geom_area(aes(y = DHW), fill = "#D16103", alpha = .6)+
  annotate(x = as.Date("2019-05-28"), y = 4, label = "DHW", col = "#D16103", geom = "text")+
  labs (x = NULL, y = "DHW (°C weeks) / DHD (°C days)")+
  theme_minimal()
plot_dhw_dhd


saveRDS(plot_sst_dhd, here("plots", "plot_sst_dhd.rds"))
saveRDS(plot_dhw_dhd, here("plots", "plot_dhw_dhd.rds"))


library(cowplot)

plot_grid(plot_sst_dhd, plot_dhw_dhd, align = "v", ncol = 1)
