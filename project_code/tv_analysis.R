# load packages and data 
library(readr)
library(tidyverse)
library(broom)
tv <- read_csv("~/tv.csv")

colnames(tv)

# jitter plot -------------------------------------------------------------

ggplot(data = tv, mapping = aes(x = year, y = aadt)) + 
  geom_jitter() 


# linear regression -------------------------------------------------------

lmout1 <- lm(aadt ~ year, data = tv)

tidy(lmout1, conf.int = TRUE)

tv$year_dum <- NA

output <- character(nrow(tv))

for(i in 1:nrow(tv)){
  if(tv$year[i] > 2015){
    output[i] <- 1
  }else if(tv$year[i] == 2015){
    output[i] <- 1
  }else if(tv$year[i] < 2015){
    output[i] <- 0
  }
}

tv$year_dum <- output

lmout2 <- lm(aadt ~ year_dum, data = tv)

tidy(lmout2, conf.int = TRUE)
