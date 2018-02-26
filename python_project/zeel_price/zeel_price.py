from bs4 import BeautifulSoup
from urllib import request
import re
import pandas as pd
import os

def crawlZeelprice():      
    url='https://www.zeel.com/zeel-massage-locations'
    page=request.urlopen(url).read().decode()
    soup=BeautifulSoup(page,'html.parser')
    class_name='z--column z-text-center-sm z-text-center-md z-text-center-lg'
    locations=soup.find('div',{'class':class_name}).find_all('a',href=True,text=True)
    market_price=dict()
    for loc in locations:
        try:
            if not loc.text in market_price:
                url_temp='https://www.zeel.com'+loc['href']
                #print(url_temp)
                page_temp=request.urlopen(url_temp).read().decode()
                soup_temp=BeautifulSoup(page_temp,'html.parser')
                string_temp=soup_temp.find('div',{'class':'col-xs-10'}).find('p').text
                market_price[loc.text]=int(re.findall(r'\$(\d*)',string_temp)[0])

        except:
            print('Error Url: ',url_temp)
            print('Error Market: ',loc.text)


    price_df=pd.DataFrame.from_dict(market_price,orient='index')
    price_df.index.name='City'
    price_df.columns=['Price']
    if os.path.exists('zeel_price.csv'):
        os.remove('zeel_price.csv')
    price_df.to_csv('zeel_price.csv')
    print('Export zeel_price.csv to {}'.format(os.getcwd()))


if __name__=='__main__':
    crawlZeelprice()