# load packages and data
library(readr)
library(tidyverse)
library(stringr)

vz <- read_csv("~/Downloads/stat_612_proj/raw_data/Vision_Zero_Safety.csv")

# rename columns
colnames(vz)

vz <- vz %>% 
  rename(x = X, 
         y = Y, 
         object_id = OBJECTID, 
         global_id = GLOBALID,
         request_id = REQUESTID,
         request_type = REQUESTTYPE, 
         request_date = REQUESTDATE,
         status = STATUS,
         street_seg_id = STREETSEGID,
         comments = COMMENTS,
         user_type = USERTYPE)

# create year and month column
vz$year <- substr(vz$request_date, 1, 4)
vz$month <- substr(vz$request_date, 6, 7)

# export data
write.csv(vz, file = "vz")
