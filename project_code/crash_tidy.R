# load data and packages
library(readr)
library(tidyverse)
crash <- read_csv("~/Downloads/stat_612_proj/raw_data/Crashes_in_DC.csv")
crash_detail <- read_csv("~/Downloads/stat_612_proj/raw_data/Crash_Details_Table.csv")

# drop unnecessary columns
colnames(crash)
cols.remove <- c("CCN", "MEASURE", "OFFSET", "FROMDATE", "TODATE", 
                 "MARID", "MAR_ADDRESS", "MAR_SCORE", "NEARESTINTROUTEID",
                 "OFFINTERSECTION", "INTAPPROACHDIRECTION", 
                 "LOCATIONERROR", "LASTUPDATEDATE", "BLOCKKEY",
                 "SUBBLOCKKEY")

crash <- crash[, ! names(crash) %in% cols.remove, drop = F]

# rename columns
crash <- crash %>% 
  rename(x = X, 
         y = Y, 
         object_id = OBJECTID,
         crime_id = CRIMEID, 
         report_date = REPORTDATE, 
         route_id = ROUTEID, 
         street_seg_id = STREETSEGID, 
         rdwy_seg_id = ROADWAYSEGID,
         address = ADDRESS, 
         lat = LATITUDE, 
         lon = LONGITUDE,
         xcor = XCOORD, 
         ycor = YCOORD,
         ward = WARD,
         event_id = EVENTID,
         maj_inj_biker = MAJORINJURIES_BICYCLIST,   
         min_inj_biker = MINORINJURIES_BICYCLIST,
         unknown_inj_biker = UNKNOWNINJURIES_BICYCLIST,
         fatal_biker = FATAL_BICYCLIST,
         maj_inj_driver= MAJORINJURIES_DRIVER,
         min_inj_driver= MINORINJURIES_DRIVER,
         unknown_inj_driver = UNKNOWNINJURIES_DRIVER,
         fatal_driver = FATAL_DRIVER,
         maj_inj_ped = MAJORINJURIES_PEDESTRIAN,
         min_inj_ped = MINORINJURIES_PEDESTRIAN,
         unknown_inj_ped = UNKNOWNINJURIES_PEDESTRIAN,
         fatal_ped = FATAL_PEDESTRIAN,
         total_vehicles = TOTAL_VEHICLES,
         total_bikes = TOTAL_BICYCLES,
         total_ped = TOTAL_PEDESTRIANS,
         ped_impaired = PEDESTRIANSIMPAIRED,
         biker_impaired = BICYCLISTSIMPAIRED,
         drivers_impaired = DRIVERSIMPAIRED,
         total_taxis = TOTAL_TAXIS,
         total_government = TOTAL_GOVERNMENT,
         speeding_involv = SPEEDING_INVOLVED,
         nearest_int_street_name = NEARESTINTSTREETNAME,
         mpd_lat = MPDLATITUDE,
         mpd_lon = MPDLONGITUDE,
         mpd_geo_x= MPDGEOX,
         mpd_geo_y = MPDGEOY)

# create year and month column
crash$year <- substr(crash$report_date, 1, 4)
crash$month <- substr(crash$report_date, 6, 7)

# export data
write.csv(crash, file = "crash")

# drop unnecessary columns
colnames(crash_detail)
cols.remove <- c("CCN")

crash_detail <- 
  crash_detail[, ! names(crash_detail) %in% cols.remove, drop = F]

# rename columns
crash_detail <- crash_detail %>% 
  rename(object_id = OBJECTID,
         crime_id = CRIMEID, 
         person_id = PERSONID, 
         person_type = PERSONTYPE,
         age = AGE,
         fatal = FATAL, 
         maj_inj = MAJORINJURY,
         min_inj = MINORINJURY,
         vehicle_id = VEHICLEID,
         in_vehicle_type = INVEHICLETYPE,
         ticket_issued = TICKETISSUED,
         license_plate_state = LICENSEPLATESTATE, 
         impaired = IMPAIRED,
         speeding = SPEEDING)

# merge crash and crash_detail
crash_full <- merge(crash, crash_detail, by = "crime_id")

# cut crash_full into two sets for export
crash_full1 <- crash_full[1:236815, ]
crash_full2 <- crash_full[236816:473631, ]

# export crash_detail and crash_full
write.csv(crash_detail, file = "crash_detail")
write.csv(crash_full1, file = "crash_full1")
write.csv(crash_full2, file = "crash_full2")


