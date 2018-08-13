#8/10/18 scraping moscow apartment rentals using Beautiful Soup 
import requests 
from bs4 import BeautifulSoup
import json
from lxml import html  
import argparse
from urllib.parse import urljoin

page_link="https://real-estate-moscow.com/en/flat-rent/rooms/1/valute/usd"

#fetch the url
page_response=requests.get(page_link,timeout=5)
page_content=BeautifulSoup(page_response.content,"html.parser")

#element extraction
rentals=page_content.find_all('div',{'class':'object-info'})

#1. rental prices
prices=page_content.find_all('span',{'class':'object-price'})

import sys
orig_stdout = sys.stdout
f=open('price_out.txt','w')
sys.stdout=f

for i in prices:
    print(i.string)

sys.stdout=orig_stdout
f.close() 

#2. Number of rooms (for each rental)
rooms=page_content.find_all('span',{'class':'object-rooms'})

orig_stdout = sys.stdout
f=open('room_out.txt','w')
sys.stdout=f

for i in rooms:
    print(i.string)

sys.stdout=orig_stdout
f.close() 

#3. room area 
area=page_content.find_all('span',{'class':'object-place_s'})

orig_stdout = sys.stdout
f=open('area_out.txt','w')
sys.stdout=f

for i in area:
    print(i.string)

sys.stdout=orig_stdout
f.close() 

