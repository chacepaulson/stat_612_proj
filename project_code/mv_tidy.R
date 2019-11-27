## Load Packages 
library(Stack)
library(dataCompareR)
library(tidyverse)

## Load Data 
library(readr)
mv2010 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2010.csv")
mv2011 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2011.csv")
mv2012 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2012.csv")
mv2013 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2013.csv")
mv2014 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2014.csv")
mv2015 <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Summary_for_2015.csv")

mv2016_jan <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_January_2016.csv")
mv2016_feb <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_February_2016.csv")
mv2016_mar <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_March_2016.csv")
mv2016_apr <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_April_2016.csv")
mv2016_may <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_May_2016.csv")
mv2016_jun <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_June_2016.csv")
mv2016_jul <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_July_2016.csv")
mv2016_aug <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_August_2016.csv")
mv2016_sep <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_September_2016.csv")
mv2016_oct <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_October_2016.csv")
mv2016_nov <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_November_2016.csv")
mv2016_dec <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_December_2016.csv")

mv2017_jan <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_January_2017.csv")
mv2017_feb <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_February_2017.csv")
mv2017_mar <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_March_2017.csv")
mv2017_apr <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_April_2017.csv")
mv2017_may <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_May_2017.csv")
mv2017_jun <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_June_2017.csv")
mv2017_jul <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_July_2017.csv")
mv2017_aug <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_August_2017.csv")
mv2017_sep <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_September_2017.csv")
mv2017_oct <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_October_2017.csv")
mv2017_nov <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_November_2017.csv")
mv2017_dec <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_December_2017.csv")

mv2018_jan <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_January_2018.csv")
mv2018_feb <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_February_2018.csv")
mv2018_mar <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_March_2018.csv")
mv2018_apr <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_April_2018.csv")
mv2018_may <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_May_2018.csv")
mv2018_jun <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_June_2018.csv")
mv2018_jul <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_July_2018.csv")
mv2018_aug <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_August_2018.csv")
mv2018_sep <- read_csv("~/Downloads/stat_612_proj/raw_data/Moving_Violations_Issued_in_September_2018.csv")

## check if column names are the same
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

my_func(mv2018_jan, mv2018_feb)
my_func(mv2018_feb, mv2018_mar)
my_func(mv2018_mar, mv2018_apr)
my_func(mv2018_apr, mv2018_jun)
my_func(mv2018_jun, mv2018_jul)
my_func(mv2018_jul, mv2018_aug)
my_func(mv2018_aug, mv2018_sep)

my_func(mv2017_jan, mv2017_feb)
my_func(mv2017_feb, mv2017_mar)
my_func(mv2017_mar, mv2017_apr)
my_func(mv2017_apr, mv2017_jun)
my_func(mv2017_jun, mv2017_jul)
my_func(mv2017_jul, mv2017_aug)
my_func(mv2017_aug, mv2017_sep)
my_func(mv2017_sep, mv2017_oct)
my_func(mv2017_oct, mv2017_nov)
my_func(mv2017_nov, mv2017_dec)

my_func(mv2016_jan, mv2016_feb)
my_func(mv2016_feb, mv2016_mar)
my_func(mv2016_mar, mv2016_apr)
my_func(mv2016_apr, mv2016_jun)
my_func(mv2016_jun, mv2016_jul)
my_func(mv2016_jul, mv2016_aug)
my_func(mv2016_aug, mv2016_sep)
my_func(mv2016_sep, mv2016_oct)
my_func(mv2016_oct, mv2016_nov)
my_func(mv2016_nov, mv2016_dec)

## Stack Months Frames 
mv1 <- Stack(mv2016_jan, mv2016_feb)
mv2 <- Stack(mv2016_mar, mv2016_apr)
mv3 <- Stack(mv2016_may, mv2016_jun)
mv4 <- Stack(mv2016_jul, mv2016_aug)
mv5 <- Stack(mv2016_sep, mv2016_oct)
mv6 <- Stack(mv2016_nov, mv2016_dec)

mv7 <- Stack(mv1, mv2)
mv8 <- Stack(mv3, mv4)
mv9 <- Stack(mv5, mv6)

mv2016 <- Stack(mv7, mv8)
mv2016 <- Stack(mv2016, mv9)

mv1 <- mv2 <- mv3 <- mv4 <- mv5 <- mv6 <- mv7 <- mv8 <- mv9 <- NA

mv1 <- Stack(mv2017_jan, mv2017_feb)
mv2 <- Stack(mv2017_mar, mv2017_apr)
mv3 <- Stack(mv2017_may, mv2017_jun)
mv4 <- Stack(mv2017_jul, mv2017_aug)
mv5 <- Stack(mv2017_sep, mv2017_oct)
mv6 <- Stack(mv2017_nov, mv2017_dec)

mv7 <- Stack(mv1, mv2)
mv8 <- Stack(mv3, mv4)
mv9 <- Stack(mv5, mv6)

mv2017 <- Stack(mv7, mv8)
mv2017 <- Stack(mv2017, mv9)

mv1 <- mv2 <- mv3 <- mv4 <- mv5 <- mv6 <- mv7 <- mv8 <- mv9 <- NA

mv1 <- Stack(mv2018_jan, mv2018_feb)
mv2 <- Stack(mv2018_mar, mv2018_apr)
mv3 <- Stack(mv2018_may, mv2018_jun)
mv4 <- Stack(mv2018_jul, mv2018_aug)
mv4 <- Stack(mv4, mv2018_sep)

mv5 <- Stack(mv1, mv2)
mv6 <- Stack(mv3, mv4)

mv2018 <- Stack(mv5, mv6)

## check that row counts are equal
nrow(mv2016_jan) + nrow(mv2016_feb) + nrow(mv2016_mar) +
  nrow(mv2016_apr) + nrow(mv2016_may) + nrow(mv2016_jun) +
  nrow(mv2016_jul) + nrow(mv2016_aug) + nrow(mv2016_sep) +
  nrow(mv2016_oct) + nrow(mv2016_nov) + nrow(mv2016_dec)

nrow(mv2016)

nrow(mv2017_jan) + nrow(mv2017_feb) + nrow(mv2017_mar) +
  nrow(mv2017_apr) + nrow(mv2017_may) + nrow(mv2017_jun) +
  nrow(mv2017_jul) + nrow(mv2017_aug) + nrow(mv2017_sep) +
  nrow(mv2017_oct) + nrow(mv2017_nov) + nrow(mv2017_dec)

nrow(mv2017)

nrow(mv2018_jan) + nrow(mv2018_feb) + nrow(mv2018_mar) +
  nrow(mv2018_apr) + nrow(mv2018_may) + nrow(mv2018_jun) +
  nrow(mv2018_jul) + nrow(mv2018_aug) + nrow(mv2018_sep)

nrow(mv2018)

# add year column to data frames 
mv2010$year <- 2010
mv2011$year <- 2011
mv2012$year <- 2012
mv2013$year <- 2013
mv2014$year <- 2014
mv2015$year <- 2015
mv2016$year <- 2016
mv2017$year <- 2017
mv2018$year <- 2018


## check that row names from year data frames are the same 
my_func(mv2018, mv2017)
my_func(mv2017, mv2016)
my_func(mv2016, mv2015)
my_func(mv2015, mv2014)
my_func(mv2014, mv2013)
my_func(mv2013, mv2012)
my_func(mv2012, mv2011)
my_func(mv2011, mv2010)

## merge years that are identical 
mv1 <- mv2 <- mv3 <- mv4 <- mv5 <- mv6 <- mv7 <- mv8 <- mv9 <- NA

mv1 <- Stack(mv2016, mv2017)
mv1 <- Stack(mv1, mv2018)

mv2 <- Stack(mv2014, mv2015)

mv3 <- Stack(mv2012, mv2013)
mv3 <- Stack(mv3, mv2011)

mv4 <- mv2010

# find which colnames are different 
mv1_names <- colnames(mv1)
mv2_names <- colnames(mv2)
mv3_names <- colnames(mv3)
mv4_names <- colnames(mv4)

mv3_names[!mv3_names %in% mv4_names]
mv4_names[!mv4_names %in% mv3_names]


# Edit MV4 ----------------------------------------------------------------

# remove columns from mv4
colnames(mv4)
cols.remove <- c("ISSUING_AGENCY_CODE", "VIOLATION_CODE", "PLATE_STATE",
                 "DISPOSITION_CODE", "DISPOSITION_TYPE",
                 "DISPOSITION_DATE", "TOTAL_PAID", "PENALTY_1", 
                 "PENALTY_2", "PENALTY_3", "PENALTY_4", "PENALTY_5",
                 "BODY_STYLE",
                 "XCOORD", "YCOORD", "MAR_ID", "GIS_LAST_MOD_DTTM")

mv4 <- mv4[, ! names(mv4) %in% cols.remove, drop = F]

# remove columns round 2 
cols.remove <- c("TICKET_NUMBER", "RP_MULT_OWNER_NO")
mv4 <- mv4[, ! names(mv4) %in% cols.remove, drop = F]

# rename columns
mv4 <- mv4 %>% 
  rename(object_id = OBJECTID, 
         location = LOCATION,
         issue_date = ISSUE_DATE,
         issue_time = ISSUE_TIME,
         iss_agency = ISSUING_AGENCY_NAME,
         iss_agency_short = ISSUING_AGENCY_SHORT,
         violation_desc = VIOLATION_PROCESS_DESC,
         accident = ACCIDENT_INDICATOR,
         fine_amount = FINE_AMOUNT,
         lat = LATITUDE,
         lon = LONGITUDE
  )

# Edit MV3 ----------------------------------------------------------------

# remove unnecessary columns
cols.remove <- c("FROMADDRESSLEFTTHEO", "TOADDRESSLEFTTHEO", "SHAPE_", "SHAPELEN")
mv3 <- mv3[, ! names(mv3) %in% cols.remove, drop = F]

# merge location into one single column 
mv3 <- mv3 %>% 
  unite("location", c(STREETSEG, REGISTEREDNAME, STREETTYPE, QUADRANT),
        sep = " ")

# reorder columns 
mv3 <- mv3 %>% 
  select(OBJECTID, location, WEEKNUMBER, year, 
         HIGHSPEED_AM_NORUSH:UNSAFEVEHICLE_PM_RUSH,
         HIGHSPEED_NO_TIME:UNSAFEVEHICLE_NO_TIME)

# gather and separate columns
mv3 <- mv3 %>% 
  gather(HIGHSPEED_AM_NORUSH:UNSAFEVEHICLE_NO_TIME, 
         key = "type", value = "value") 

mv3 <- mv3 %>% 
  filter(value > 0)

mv3 <- mv3 %>% 
  separate(type, c("violation_type", "time", "rush"), sep = "_")

# rename and remove columns 
colnames(mv3)

mv3 <- mv3 %>% 
  rename(object_id = OBJECTID, 
         week_num = WEEKNUMBER)

# Edit MV2 ----------------------------------------------------------------

# merge location into one single column 
mv2 <- mv2 %>% 
  unite("location", c(STREETSEG, REGISTEREDNAME, STREETTYPE, QUADRANT),
        sep = " ")

# gather and separate columns
mv2 <- mv2 %>% 
  gather(HIGHSPEED_AM_NORUSH:UNSAFEVEHICLE_PM_RUSH, 
         key = "type", value = "value") 

mv2 <- mv2 %>% 
  filter(value > 0)

mv2 <- mv2 %>% 
  separate(type, c("violation_type", "time", "rush"), sep = "_")

# remove unnecessary columns
colnames(mv2)

cols.remove <- c("FROMADDRESSLEFTTHEO", "TOADDRESSLEFTTHEO",
                 "SHAPE_", "SHAPELEN")

mv2 <- mv2[, ! names(mv2) %in% cols.remove, drop = F]

# rename columns 
mv2 <- mv2 %>% 
  rename(object_id = OBJECTID, 
         week_num = WEEKNUMBER)


# Edit MV1 ----------------------------------------------------------------

# remove columns
cols.remove <- c("XCOORD", "YCOORD", "TICKET_NUMBER", 
                 "ISSUING_AGENCY_CODE", "VIOLATION_CODE", "PLATE_STATE",
                 "DISPOSITION_CODE", "DISPOSITION_TYPE","DISPOSITION_DATE",
                 "TOTAL_PAID", "PENALTY_1", "PENALTY_2", "PENALTY_3",
                 "PENALTY_4", "PENALTY_5", "RP_MULT_OWNER_NO","BODY_STYLE",
                 "MAR_ID", "GIS_LAST_MOD_DTTM")   

mv1 <- mv1[, ! names(mv1) %in% cols.remove, drop = F]
 
# rename columns 
mv1 <- mv1 %>% 
  rename(object_id = OBJECTID,
         location = LOCATION,
         issue_date = ISSUE_DATE,
         issue_time = ISSUE_TIME,
         iss_agency = ISSUING_AGENCY_NAME,
         iss_agency_short = ISSUING_AGENCY_SHORT,
         violation_desc = VIOLATION_PROCESS_DESC,
         accident = ACCIDENT_INDICATOR,
         fine_amount = FINE_AMOUNT,
         lat = LATITUDE,
         lon = LONGITUDE)


# Create Violation Type ---------------------------------------------------

## EXTERNAL HAND CODE FROM EXPORTED CSV FOR VIOLATION_TYPE
unique(mv3$violation_type)

mv_violation1 <- unique(mv1$violation_desc)
mv_violation2 <- unique(mv4$violation_desc)

mv_violation1 <- data.frame(type = mv_violation1)
mv_violation2 <- data.frame(type = mv_violation2)

# load in key data 
mv_violation1 <- read_csv("~/Downloads/stat_612_proj/raw_data/mv_violation1.csv")
mv_violation2 <- read_csv("~/Downloads/stat_612_proj/raw_data/mv_violation2.csv")

# attach key to mv1 and mv4 
mv1 <- merge(mv1, mv_violation1, by = "violation_desc")
mv4 <- merge(mv4, mv_violation2, by = "violation_desc")

cols.remove <- c("violation_type.y")

# Merge MV Data Frames ----------------------------------------------------
colnames(mv1)
colnames(mv2)
colnames(mv3)
colnames(mv4)

test <- Stack(mv1, mv2)
test2 <- Stack(mv3, mv4)

mv <- Stack(test, test2)

# reorder columns 
colnames(mv)

mv <- mv %>% 
  select(object_id, location, lat, lon, iss_agency, iss_agency_short,
         issue_date, issue_time, year, week_num, violation_desc, 
         violation_type, time, rush, value, accident, fine_amount)

# fill NA in value column 

output <- character(nrow(mv))

for(i in 1:nrow(mv)){
  if(is.na(mv$value[i])){
    output[i] <- 1
  }else{
    output[i] <- mv$value[i]
  }
}

mv$value <- output

# Export Data -------------------------------------------------------------
nrow(mv) / 10

mv_full1 <- mv[1:377408, ]
mv_full2 <- mv[377409:754816, ]
mv_full3 <- mv[754817:1132224, ]
mv_full4 <- mv[1132225:1509632, ]
mv_full5 <- mv[1509633:1887040, ]
mv_full6 <- mv[1887041:2264448, ]
mv_full7 <- mv[2264449:2641856, ]
mv_full8 <- mv[2641857:3019264, ]
mv_full9 <- mv[3019265:3396672, ]
mv_full10 <- mv[3396673:3774072, ]

write.csv(mv_full1, "mv1.csv")
write.csv(mv_full2, "mv2.csv")
write.csv(mv_full3, "mv3.csv")
write.csv(mv_full4, "mv4.csv")
write.csv(mv_full5, "mv5.csv")
write.csv(mv_full6, "mv6.csv")
write.csv(mv_full7, "mv7.csv")
write.csv(mv_full8, "mv8.csv")
write.csv(mv_full9, "mv9.csv")
write.csv(mv_full10, "mv10.csv")

