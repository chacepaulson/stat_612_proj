# load data and packages 
library(readr)
library(tidyverse)
library(Stack)

tv2010 <- read_csv("~/Downloads/stat_612_proj/raw_data/2010_Traffic_Volume.csv")
tv2011 <- read_csv("~/Downloads/stat_612_proj/raw_data/2011_Traffic_Volume.csv")
tv2012 <- read_csv("~/Downloads/stat_612_proj/raw_data/2012_Traffic_Volume.csv")
tv2013 <- read_csv("~/Downloads/stat_612_proj/raw_data/2013_Traffic_Volume.csv")
tv2014 <- read_csv("~/Downloads/stat_612_proj/raw_data/2014_Traffic_Volume.csv")
tv2015 <- read_csv("~/Downloads/stat_612_proj/raw_data/2015_Traffic_Volume.csv")
tv2016 <- read_csv("~/Downloads/stat_612_proj/raw_data/2016_Traffic_Volume.csv")
tv2017 <- read_csv("~/Downloads/stat_612_proj/raw_data/2017_Traffic_Volume.csv")

# check which data frames have the same column names 
my_func <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

my_func(tv2010, tv2011)
my_func(tv2011, tv2012)
my_func(tv2012, tv2013)
my_func(tv2013, tv2014)
my_func(tv2014, tv2015)
my_func(tv2015, tv2016)
my_func(tv2016, tv2017)

# rename column in 2015
colnames(tv2015)

tv2015 <- tv2015 %>% 
  rename(AADT2015 = COUNT2015)

# Split AADT column to create year column
gather_func<- function(df, x){
  df <- df %>% 
    gather(x, 
           key = "year", value = "AADT")
}

tv2010 <- gather_func(tv2010, "AADT2010")
tv2011 <- gather_func(tv2011, "AADT2011")
tv2012 <- gather_func(tv2012, "AADT2012")
tv2013 <- gather_func(tv2013, "AADT2013")
tv2014 <- gather_func(tv2014, "AADT2014")
tv2015 <- gather_func(tv2015, "AADT2015")

tv2010$year <- str_sub(tv2010$year, 5, 8)
tv2011$year <- str_sub(tv2011$year, 5, 8)
tv2012$year <- str_sub(tv2012$year, 5, 8)
tv2013$year <- str_sub(tv2013$year, 5, 8)
tv2014$year <- str_sub(tv2014$year, 5, 8)
tv2015$year <- str_sub(tv2015$year, 5, 8)

# cut down columns 
colnames(tv2010)

select_func <- function(df){
  df <- df %>% 
    select(year, OBJECTID, QUADCODE, ROADTYPE, REGISTEREDNAME, STREETTYPE,
           QUADRANT, FROMADDRESSLEFT, TOADDRESSLEFT, FROMADDRESSRIGHT,
           TOADDRESSRIGHT, FROMGRADE, TOGRADE, AADT)
}

tv2010 <- select_func(tv2010)
tv2011 <- select_func(tv2011)
tv2012 <- select_func(tv2012)
tv2013 <- select_func(tv2013)
tv2014 <- select_func(tv2014)
tv2015 <- select_func(tv2015)

# rename columns 
colnames(tv2010)

rename_func <- function(df){
  df <- df %>% 
    rename(object_id = OBJECTID, 
           quad_code = QUADCODE, 
           road_type = ROADTYPE, 
           registered_name = REGISTEREDNAME, 
           street_type = STREETTYPE,
           quadrant = QUADRANT, 
           from_add_left = FROMADDRESSLEFT, 
           to_add_left = TOADDRESSLEFT, 
           from_add_right = FROMADDRESSRIGHT,
           to_add_right = TOADDRESSRIGHT, 
           from_grade = FROMGRADE, 
           to_grade = TOGRADE, 
           aadt = AADT)
}

tv2010 <- rename_func(tv2010)
tv2011 <- rename_func(tv2011)
tv2012 <- rename_func(tv2012)
tv2013 <- rename_func(tv2013)
tv2014 <- rename_func(tv2014)
tv2015 <- rename_func(tv2015)

# combine street columns in location in pv 
location2010 <- tv2010 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")
location2011 <- tv2011 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")
location2012 <- tv2012 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")
location2013 <- tv2013 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")
location2014 <- tv2014 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")
location2015 <- tv2015 %>% 
  unite("location", c(registered_name, street_type, quadrant),
        sep = " ")

tv2010$location <- location2010$location
tv2011$location <- location2011$location
tv2012$location <- location2012$location
tv2013$location <- location2013$location
tv2014$location <- location2014$location
tv2015$location <- location2015$location

# remove unnecessary columns from 2016 and 2017 
colnames(tv2016)

tv2016 <- tv2016 %>% 
  select(AADT_YEAR, OBJECTID, AADT)

tv2017 <- tv2017 %>% 
  select(AADT_YEAR, OBJECTID, AADT)

# rename columns from 2016 and 2017 
tv2016 <- tv2016 %>% 
  rename(object_id = OBJECTID, 
         aadt = AADT, 
         year = AADT_YEAR)

tv2017 <- tv2017 %>% 
  rename(object_id = OBJECTID, 
         aadt = AADT, 
         year = AADT_YEAR)

# join along key for 2016 and 2017 
join1 <- tv2016 %>% 
  left_join(tv2010, by = "object_id")

join2 <- tv2017 %>% 
  left_join(tv2010, by = "object_id")

# drop and rename repeat columns
colnames(join1)

cols.remove <- c("year.y", "aadt.y")
join1 <- join1[, ! names(join1) %in% cols.remove, drop = F]
join2 <- join2[, ! names(join2) %in% cols.remove, drop = F]

join1 <- join1 %>% 
  rename(year = year.x, 
         aadt = aadt.x)

join2 <- join2 %>% 
  rename(year = year.x, 
         aadt = aadt.x)

tv2016 <- join1
tv2017 <- join2

# match limits on the aadt 
tv2010$aadt <- tv2010$aadt * 1000
tv2011$aadt <- tv2011$aadt * 1000
tv2012$aadt <- tv2012$aadt * 1000
tv2013$aadt <- tv2013$aadt * 1000
tv2014$aadt <- tv2014$aadt * 1000
tv2015$aadt <- tv2015$aadt * 1000

# stack finalized data 
tv1 <- Stack(tv2010, tv2011)
tv2 <- Stack(tv2012, tv2013)
tv3 <- Stack(tv2014, tv2015)
tv4 <- Stack(tv2016, tv2017)

tv5 <- Stack(tv1, tv2)
tv6 <- Stack(tv3, tv4)

tv <- Stack(tv5, tv6)

# export data 
write.csv(tv, "tv.csv")
