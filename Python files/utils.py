from bs4 import BeautifulSoup
from re import compile
from geopy.geocoders import ArcGIS
import pandas as pd

def clean_data(df):
    date = []
    for i in df['Sold Date']:
        date.append( 
            i.replace('Sold on ','') \
            .strip() \
                .replace(' ', '-')
                )
    
    price = []
    for i in df['Price']:
        
        rm = i.replace('$', '') \
            .replace(',', '')
        price.append (
            int(rm) if i != pd.NA else pd.NA
            )
    
    # Getting latitude and longitude of the houses
    latitude, longitude = [], []
    geolocator_arcgis = ArcGIS()
    for i in df['Address']:

        getloc = geolocator_arcgis.geocode(i)
        
        latitude.append(
            getloc.latitude
        )
        longitude.append(
            getloc.longitude
        )
    df[['Sold Date', 'Price', 'Latitude', 'Longitude']] = pd.DataFrame(
        list(zip(date, price, latitude, longitude)),
    columns=['Sold Date', 'Price', 'Latitude', 'Longitude']
    )
    df['Sold Date'] = pd.to_datetime(df['Sold Date'])
    return df


def data_extractor(temp):
    """
    Extracts all the houses data in a given page.

    Parameter:
    temp (Response): A request package object.

    Returns:
    Data of the given page of all sold houses
    """

    listings = BeautifulSoup(temp.text, 'html.parser')
    addresses = [tag.text.strip() for tag in listings.select('a > h4')]
    property_types = [property.text for property in listings.find_all('div', class_ = 'easy-block-v1-badge')]

    ul = listings.find_all('ul', class_ = 'list-unstyled')


    houses_data = []
    for address, i, property_type in zip(addresses, ul, property_types):

        try:
            sold_date = i.find_all('li')[0] \
                .find(string = compile('Sold')) \
                    .text
        except:
            sold_date = pd.NA

        try: 
           price =  i.find_all('code')[0] \
            .find(string = compile('$')) \
                .text
        except:
            price = pd.NA

        try: 
            beds = i.find('i', class_ = 'i-bed') \
                .find_next('big') \
                    .text
        except:
            beds = pd.NA
        
        try: 
            bath_tag = i.find('i', class_ = 'i-bath')
            if bath_tag:
                baths = bath_tag.nextSibling
            else:
                baths = pd.NA
        except:
            baths = pd.NA

        try: 
            garage_tag = i.find('i', class_ = 'i-car')
            if garage_tag:
                garages = garage_tag.nextSibling
            else:
                garages = pd.NA
        except:
            garages = pd.NA
        
        houses_data.append({
            'Address': address,
            'Sold Date': sold_date,
            'Price': price,
            'Beds': beds,
            'Baths': baths,
            'Garages': garages,
            'Property Type': property_type
        })

    return houses_data