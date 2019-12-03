pv1 <- read.csv("~/stat_612_proj/coded_data/pv1.csv")
pv2 <- read.csv("~/stat_612_proj/coded_data/pv2.csv")
pv3 <- read.csv("~/stat_612_proj/coded_data/pv3.csv")
pv4 <- read.csv("~/stat_612_proj/coded_data/pv4.csv")
pv5 <- read.csv("~/stat_612_proj/coded_data/pv5.csv")
pv6 <- read.csv("~/stat_612_proj/coded_data/pv6.csv")
pv7 <- read.csv("~/stat_612_proj/coded_data/pv7.csv")
pv8 <- read.csv("~/stat_612_proj/coded_data/pv8.csv")
pv9 <- read.csv("~/stat_612_proj/coded_data/pv9.csv")
pv10 <- read.csv("~/stat_612_proj/coded_data/pv10.csv")
pv11 <- read.csv("~/stat_612_proj/coded_data/pv11.csv")
install.packages("Stack")
library(Stack)
library(tidyverse)
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

#----------------------------------------------------------------------
View(pv)
sum(is.na(pv$lat))
sum(is.na(pv$rush))  #too much NA, dont use this colunm
row(pv)
unique(pv$violation_type)
#----------------------------------------------------------------------------
#year + time
pv_1<-pv%>%
  select(year,time)%>%
  unite(TIME,year,time,sep="-")%>%
  count(TIME)%>%
  separate(TIME,into = c("year","time"))
pv_1
ggplot(pv_1,aes(time,n,color=year))+
  geom_point()+
  geom_line(aes(group=year)) 

ggplot(pv_1,aes(year,n,color=time))+
  geom_point()+
  geom_line(aes(group=time)) #good picture!


#---------------------------------------------------------
#violation type
unique(pv$violation_type)

pv_type<-pv%>%
  select(year,violation_type)%>%
  unite(TYPE,year,violation_type,sep="-")%>%
  count(TYPE)%>%
  separate(TYPE,into = c("year","type"))
pv_type
options(scipen = 200)
ggplot(pv_type, mapping=aes(x=year,y=n,fill=type))+
  geom_bar(stat="identity",width=0.5,position='stack',size=5)+
  labs(x = '', y = 'violation type(%)') +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11))

pv_typeCOMMVEHICLE<-pv_type%>%
  filter(type=="COMMVEHICLE")
pv_typeIMPEDBIKEPED<-pv_type%>%
  filter(type=="IMPEDBIKEPED")
pv_typeMETERS<-pv_type%>%
  filter(type=="METERS")
pv_typeOTHER<-pv_type%>%
  filter(type=="OTHER")
pv_typeRESTRICTEDZONES<-pv_type%>%
  filter(type=="RESTRICTEDZONES")
pv_typeRPP<-pv_type%>%
  filter(type=="RPP")
pv_typeUNSAFEPARKING<-pv_type%>%
  filter(type=="UNSAFEPARKING")
pv_typeRUSHHOURVIOLATIONS<-pv_type%>%
  filter(type=="RUSHHOURVIOLATIONS")
ggplot(pv_typeCOMMVEHICLE, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="COMMVEHICLE",x = '', y = 'count') 
ggplot(pv_typeIMPEDBIKEPED, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="IMPEDBIKEPED",x = '', y = 'count') 
ggplot(pv_typeMETERS , mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="METERS",x = '', y = 'count') 
ggplot(pv_typeOTHER, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="OTHER",x = '', y = 'count') 
ggplot(pv_typeRESTRICTEDZONES, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="RESTRICTEDZONES",x = '', y = 'count') 
ggplot(pv_typeRPP , mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="RPP",x = '', y = 'count') 
ggplot(pv_typeRUSHHOURVIOLATIONS, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="RUSHHOURVIOLATIONS",x = '', y = 'count') 
ggplot(pv_typeUNSAFEPARKING, mapping=aes(x=year,y=n,fill=year))+
  geom_bar(stat="identity")+
  labs(title="UNSAFEPARKING",x = '', y = 'count') 


ggplot(pv_type, mapping=aes(x="type",fill=type))+
  geom_bar(stat="count",width=0.5,position='stack',size=5)+
  coord_polar("y", start=0)+
  blank_theme+
  geom_text(stat="count",aes(label = scales::percent((..count..)/sum(..count..))), 
            size=4, position=position_stack(vjust = 0.5))
View(pv)
#------------------------------
#Regression use
pv_0<-pv%>%
  select(year)%>%
  count(year)
pv_0
options(scipen = 200)
pv_00<-lm(pv_0)
pv_00
summary(pv_00)

pv_0$year_dum <- NA


for(i in 1:nrow(pv_0)){
  
  if(pv_0$year[i] > 2015){
    
    pv_0$year_dum[i] <- 1
    
  }else if(pv_0$year[i] == 2015){
    
    pv_0$year_dum[i] <- 1
    
  }else if(pv_0$year[i] < 2015){
    
    pv_0$year_dum[i] <- 0
    
  }
}

lmout1 <- lm(n ~ year_dum, data = pv_0)


View(pv11)
