# import data and packages 
library(tidyverse)
library(readr)
library(Stack)
library(broom)
library(gridExtra) 
library(grid) 
library(png) 
library(downloader) 
library(grDevices)
library(ggmap)
crash_full1 <- read_csv("~/Downloads/stat_612_proj/coded_data/crash_full1.csv")
crash_full2 <- read_csv("~/Downloads/stat_612_proj/coded_data/crash_full2.csv")

# stack data sets 
crash <- Stack(crash_full1, crash_full2)

# check column names 
colnames(crash)

cols.remove <- c("x", "y", "object_id.x", "route_id", "street_seg_id",
                 "rdwy_seg_id", "xcor", "ycor", "event_id", "mpd_geo_y",
                 "mpd_geo_y", "object_id.y", "vehicle_id")

crash <- crash[, ! names(crash) %in% cols.remove, drop = F]

# subset to relevant years 
crash <- subset(crash, year > 2009)
crash <- subset(crash, year < 2020)


# general bar graph -------------------------------------------------------
 
crash$year_factor <- as.factor(crash$year)

ggplot(data = crash, mapping = aes(x = year_factor, color = year_factor,
                                   fill = year_factor)) +
  geom_bar() + 
  labs(title = "Crashes by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none")


# separated bar charts ----------------------------------------------------

fatal <- subset(crash, fatal == "Y")

p1 <- ggplot(data = fatal, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Fatal Crashes by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none") 

maj_inj <- subset(crash, maj_inj == "Y")

p2 <- ggplot(data = maj_inj, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Major Injuries in Crashes by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none") 

min_inj <- subset(crash, min_inj == "Y")

p3 <- ggplot(data = min_inj, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Minor Injuries in Crashes by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none") 

ticket <- subset(crash, ticket_issued == "Y")

p4 <- ggplot(data = ticket, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Tickets Issued for Crashes by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none") 

impaired <- subset(crash, impaired == "Y")

p5 <- ggplot(data = impaired, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Crashes with Impaired Drivers by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none")

speeding <- subset(crash, speeding == "Y")

p6 <- ggplot(data = speeding, 
       mapping = aes(x = year_factor, color = year_factor,
                     fill = year_factor)) +
  geom_bar() + 
  labs(title = "Crashes with Drivers who were Speeding by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none")

grid.arrange(p4, p1, p2, p3, p5, p6, ncol = 2)

# linear regression -------------------------------------------------------

crash_year <- crash %>% 
  count(year, name = "crash_count")

crash_multi <- crash %>% 
  count(year, fatal, maj_inj, min_inj, ticket_issued, impaired, speeding,
        name = "crash_count")

crash_year$year_dum <- NA
crash_multi$year_dum <- NA

for(i in 1:nrow(crash_year)){
  if(crash_year$year[i] > 2015){
    crash_year$year_dum[i] <- 1
  }else if(crash_year$year[i] == 2015){
    crash_year$year_dum[i] <- 1
  }else if(crash_year$year[i] < 2015){
    crash_year$year_dum[i] <- 0
  }
}

for(i in 1:nrow(crash_multi)){
  if(crash_multi$year[i] > 2015){
    crash_multi$year_dum[i] <- 1
  }else if(crash_multi$year[i] == 2015){
    crash_multi$year_dum[i] <- 1
  }else if(crash_multi$year[i] < 2015){
    crash_multi$year_dum[i] <- 0
  }
}

lmout1 <- lm(crash_count ~ year_dum, data = crash_year)

tidy(lmout1, conf.int = TRUE)

lmout2 <- lm(crash_count ~ year_dum + fatal + maj_inj + min_inj + ticket_issued + impaired + speeding, data = crash_multi)

tidy(lmout2, conf.int = TRUE)


fatal_count <- fatal %>% 
  count(year, name = "fatal_crash_count")

fatal_count$year_dum <- NA

for(i in 1:nrow(fatal_count)){
  if(fatal_count$year[i] > 2015){
    fatal_count$year_dum[i] <- 1
  }else if(fatal_count$year[i] == 2015){
    fatal_count$year_dum[i] <- 1
  }else if(fatal_count$year[i] < 2015){
    fatal_count$year_dum[i] <- 0
  }
}

lmout3 <- lm(fatal_crash_count ~ year_dum, data = fatal_count)

tidy(lmout3, conf.int = TRUE)

maj_inj_count <- maj_inj %>% 
  count(year, name = "maj_inj_crash_count")

maj_inj_count$year_dum <- NA

for(i in 1:nrow(maj_inj_count)){
  if(maj_inj_count$year[i] > 2015){
    maj_inj_count$year_dum[i] <- 1
  }else if(maj_inj_count$year[i] == 2015){
    maj_inj_count$year_dum[i] <- 1
  }else if(maj_inj_count$year[i] < 2015){
    maj_inj_count$year_dum[i] <- 0
  }
}

lmout4 <- lm(maj_inj_crash_count ~ year_dum, data = maj_inj_count)

tidy(lmout4, conf.int = TRUE)

# maps --------------------------------------------------------------------

crash18 <- subset(crash, year == 2018)
crash14 <- subset(crash, year == 2014)

qmplot(lon, lat, data = crash18, color = I('blue'), size = I(0.3), darken = 0.2, alpha = I(0.1))
qmplot(lon, lat, data = crash14, color = I('blue'), size = I(0.3), darken = 0.2, alpha = I(0.1))

fatal18 <- subset(crash18, fatal == "Y")
fatal14 <- subset(crash14, fatal == "Y")

qmplot(lon, lat, data = fatal18, color = I('blue'), size = I(3), darken = 0.2, alpha = I(1))
qmplot(lon, lat, data = fatal14, color = I('blue'), size = I(3), darken = 0.2, alpha = I(1))

maj_inj18 <- subset(crash18, maj_inj == "Y")
maj_inj14 <- subset(crash14, maj_inj == "Y")

qmplot(lon, lat, data = maj_inj18, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))
qmplot(lon, lat, data = maj_inj14, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))

min_inj18 <- subset(crash18, min_inj == "Y")
min_inj14 <- subset(crash14, min_inj == "Y")

qmplot(lon, lat, data = min_inj18, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))
qmplot(lon, lat, data = min_inj14, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))

speeding18 <- subset(crash18, speeding == "Y")
speeding14 <- subset(crash14, speeding == "Y")

qmplot(lon, lat, data = speeding18, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))
qmplot(lon, lat, data = speeding14, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))

