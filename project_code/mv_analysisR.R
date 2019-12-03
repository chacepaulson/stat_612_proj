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

moving_v1 <- read_csv("~/American University/Statistical Programming in R/Group project/mv1.csv")
moving_v2 <- read_csv("~/American University/Statistical Programming in R/Group project/mv2.csv")
moving_v3 <- read_csv("~/American University/Statistical Programming in R/Group project/mv3.csv")
moving_v4 <- read_csv("~/American University/Statistical Programming in R/Group project/mv4.csv")
moving_v5 <- read_csv("~/American University/Statistical Programming in R/Group project/mv5.csv")
moving_v6 <- read_csv("~/American University/Statistical Programming in R/Group project/mv6.csv")
moving_v7 <- read_csv("~/American University/Statistical Programming in R/Group project/mv7.csv")
moving_v8 <- read_csv("~/American University/Statistical Programming in R/Group project/mv8.csv")
moving_v9 <- read_csv("~/American University/Statistical Programming in R/Group project/mv9.csv")
moving_v10 <- read_csv("~/American University/Statistical Programming in R/Group project/mv10.csv")

# stack data sets 
moving_v <- Stack(moving_v1,moving_v2)
moving_v <- Stack(moving_v,moving_v3)
moving_v <- Stack(moving_v,moving_v4)
moving_v <- Stack(moving_v,moving_v5)
moving_v <- Stack(moving_v,moving_v6)
moving_v <- Stack(moving_v,moving_v7)
moving_v <- Stack(moving_v,moving_v8)
moving_v <- Stack(moving_v,moving_v9)
moving_v <- Stack(moving_v,moving_v10)

# check column names 
colnames(moving_v)

cols.remove <- c("X1", "object_id", "location", "iss_agency",
                 "iss_agency_short", "issue_date", "issue_time", "week_num", "time",
                 "rush", "value", "accident", "fine_amount")


moving_v <- moving_v[, ! names(moving_v) %in% cols.remove, drop = F]

moving_v

# subset to relevant years 
moving_v <- subset(moving_v, year > 2012)
moving_v <- subset(moving_v, year < 2020)


# general bar graph -------------------------------------------------------

moving_v$year_factor <- as.factor(moving_v$year)

ggplot(data = moving_v, mapping = aes(x = year_factor, color = year_factor,
                                   fill = year_factor)) +
  geom_bar() + 
  labs(title = "Moving Violations by Year",
       x = "Year", 
       y = "Count") + 
  theme(legend.position = "none") +
  scale_y_continuous("Count")


# linear regression -------------------------------------------------------

moving_v_year <- moving_v %>% 
  count(year, name = "moving_v_count")

#output is successfully a count of mv by year

moving_v_new <- subset(moving_v, year > 2015)

moving_v_lm <- moving_v_new %>%
  count(year, violation_type, name = "moving_v_count")

lmout1 <- lm(moving_v_count ~ year + violation_type, data = moving_v_lm)

tidy(lmout1, conf.int = TRUE)


# maps --------------------------------------------------------------------

moving_v_18 <- subset(moving_v, year == 2018)
moving_v_16 <- subset(moving_v, year == 2016)

qmplot(lon, lat, data = moving_v_18, color = I('blue'), size = I(0.3), darken = 0.2, alpha = I(0.1))
qmplot(lon, lat, data = moving_v_16, color = I('blue'), size = I(0.3), darken = 0.2, alpha = I(0.1))


fatal18 <- subset(crash18, fatal == "Y")
fatal14 <- subset(crash14, fatal == "Y")

qmplot(lon, lat, data = fatal18, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))
qmplot(lon, lat, data = fatal14, color = I('blue'), size = I(0.7), darken = 0.2, alpha = I(1))

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
