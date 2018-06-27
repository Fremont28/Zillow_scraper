#6/18/18 
#zillow scraping (rvest)
library(rvest)
library(tidyverse)
library(stringr)
library(rworldmap)
library(ggmap)
library(ggplot2)
library(plyr)
library(dplyr)
library(outliers)

links=sprintf("https://www.zillow.com/homes/for_rent/Charlotte-NC/condo,apartment_duplex_type/24043_rid/35.507635,-80.163804,34.909584,-81.498642_rect/9_zm/2_p",1:25)
results<-map(links, ~ {
  
  houses<-read_html(.x) %>%
    html_nodes(".photo-cards li article")
  
  long=houses %>%
    html_attr("data-longitude")
  
  lat=houses %>%
    html_attr("data-latitude")
  
  z_id<- houses %>%
    html_attr("id")
  
  address<-houses %>%
    html_node(".zsg-photo-card-address") %>%
    html_text()
  
  price<-houses %>%
    html_node(".zsg-photo-card-address") %>%
    html_text() %>%
    readr::parse_number()
  
  params <- houses %>%
    html_node(".zsg-photo-card-info") %>%
    html_text() %>%
    strsplit("&middot;")
  
  house_area <- params %>%
    str_extract("[0-9,]+(?=\\+*\\s*sqft)") %>%
    str_replace(",", "") %>%
    as.numeric
  
  beds <- params %>%
    str_extract("\\d+(?=\\s*bds)") %>%
    as.numeric()
  
  data_frame(price=price,house_area=house_area,beds=beds,house=houses,long=long,lat=lat)
}) %>%
  bind_rows(.id='page-no')

results1=subset(results,select=c("price","house_area","beds","long","lat"))
results2=as.data.frame(results1)
test=substr(gsub("", "\\.", paste0(results2$lat, "0")), 1,5)
write.csv(results2,file="resultsX.csv")

#import scraped zillow dataset 
zillow_casa=read.csv("new_orleans.csv")

no=subset(zillow_casa,City=="New Orleans")
no$airbnb=0 
no1=subset(no,select=c("lat1","long1","price","airbnb"))

orleans_air=read.csv("new orleans.csv")
colnames(orleans_air)[7]="lat1"
colnames(orleans_air)[8]="long1"
orleans_air$airbnb=1 

#map (zillow vs. airbnb listings in new orleans)
map=get_map(location=c(-90.1,29.9),zoom=11) #or 12? 
ggmap(map)+geom_point(data=no,aes(x=long1,y=lat1,alpha=0.5,color=price))


room_price=tapply(orleans_air$thirty_day_price,orleans_air$room_type,mean)
orleans_air1=subset(orleans_air,select=c("lat1","long1","price","airbnb"))
orleans_air1$thirty_day_price=orleans_air1$price*30
orleans_air1$thirty_day_price
mean(orleans_air1$thirty_day_price) #$5,909

#combine zillow and airbnb dataframes
no_finals=rbind(orleans_air1,no1)
no_finals$airbnb=as.factor(no_finals$airbnb)

#zillow rental averages 
apt_prices= zillow_casa%>%
  group_by(City) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE),
                   mean_house_area=mean(house_area,na.rm=TRUE),
                   mean_beds=mean(beds,na.rm=TRUE),
                   mean_price_sq=mean(price_sq,na.rm=TRUE))
apt_prices 

#testing for outliers 
#cochran test (mean difference between groups)
chisq.out.test(zillow_casa$price) #15,708 is an outliers (p<0.05)

#ranking zillow prices 
rank(zillow_casa$house_area)
#one-way anava (prices)

price_sq_values=groupwiseMean(price_sq~City,
                              data=zillow_casa,
                              conf=0.95,
                              digits=3,
                              traditional=FALSE,
                              percentile=TRUE)
ggplot(zillow_casa,              
       aes(x = City,
           y = price_sq)) +
  geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                width = 0.05, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  ylab("Average Price Per Square Foot")

#price per sq ft
zillow_casa$price_sq=zillow_casa$house_area/zillow_casa$price
zillow_casa$price_sq

#outlier analysis 
grubbs.test(zillow_casa$price) #yes p<0.05
#remove outlier? 
zillow_casa1=rm.outlier(zillow_casa$price)
zillow_casa1=as.data.frame(zillow_casa1)

#zillow averages with outliers removed
apt_prices1= zillow_casa1%>%
  group_by(City) %>%
  dplyr::summarize(mean_price = mean(zillow_casa1, na.rm=TRUE)) 
apt_prices1 


