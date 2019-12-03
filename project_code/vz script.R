# vz script
library(ggplot2)
library(tidyverse)
library(dplyr)
vz<-read.csv("vz.csv")

vz

# counts of requests type 

vz$request_id<-as.factor(vz$request_id)

ggplot(data = vz)+geom_bar(mapping = aes(x=request_type))# the name of x axis is overlapping 

# counts of overall requests by year

year_requests<- vz%>%
  count(year, name = 'requests_count')
year_requests

ggplot(data = year_requests)+
  geom_bar(mapping = aes(x = year, y = requests_count),stat = 'identity')+
  xlab('year')+ylab('overcall requests by year')+
  theme_bw()
# simple linear regression between year and overall requests by year

x<-ggplot(data = year_requests,mapping = aes(x = year, y = requests_count))+
  xlab('year')+ylab('requests by year')+
  theme_bw()+
  geom_smooth(method = "lm", fill = NA,color="black")
print(x+ggtitle("relation between overall requests and year"))



