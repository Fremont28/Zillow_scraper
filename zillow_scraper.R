#6/10/18*********************
# zillow scraping with rvest (new orleans rentals)

#import libraries 
library(rvest)
library(tidyverse)
library(stringr)

links=sprintf("https://www.zillow.com/homes/for_rent/New-Orleans-LA/19594_rid/30.356879,-89.314041,29.71847,-90.451126_rect/9_zm/2_p",1:25)


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

describe(results$price) 
results1=subset(results,select=c("price","house_area","beds","long","lat"))
results2=as.data.frame(results1)

#source: https://stackoverflow.com/questions/49834051/web-scraping-with-rvest-filtering-through-paginanation
