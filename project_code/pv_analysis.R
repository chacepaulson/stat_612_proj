library(readr)
library(Stack)
library(tidyverse)
library(broom)

pv1 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv1.csv")
pv2 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv2.csv")
pv3 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv3.csv")
pv4 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv4.csv")
pv5 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv5.csv")
pv6 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv6.csv")
pv7 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv7.csv")
pv8 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv8.csv")
pv9 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv9.csv")
pv10 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv10.csv")
pv11 <- read_csv("~/Downloads/stat_612_proj/coded_data/pv11.csv")

pv<-Stack(pv1,pv2)
pv<-Stack(pv,pv3)
pv<-Stack(pv,pv4)
pv<-Stack(pv,pv5)
pv<-Stack(pv,pv6)
pv<-Stack(pv,pv7)
pv<-Stack(pv,pv8)
pv<-Stack(pv,pv9)
pv<-Stack(pv,pv10)
pv<-Stack(pv,pv11)

pv$year_fac <- as.factor(pv$year)

ggplot(data = pv, mapping = aes(x = year_fac, color = violation_type,
                                fill = violation_type)) +
  geom_bar() + 
  labs(title = "Violation Type by Year",
       x = "Year",
       y = "Violation Count")

ggplot(data = pv, mapping = aes(x = year_fac, color = year_fac,
                                fill = year_fac)) +
  geom_bar() + 
  labs(title = "Violation Type by Year",
       x = "Year",
       y = "Violation Count") +
  facet_wrap(~violation_type, ncol = 2)

pv_sub <- subset(pv, year > 2015)

pv_lm <- pv_sub %>% 
  count(year, violation_type,
        name = "parking_count")

pv_lm$year_fac <- as.factor(pv_lm$year)

lmout1 <- lm(parking_count ~ year_fac, data = pv_lm)

tidy(lmout1, conf.int = TRUE)

unique(pv$violation_type)
