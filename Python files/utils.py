from bs4 import BeautifulSoup
from re import compile
from geopy.geocoders import ArcGIS
import pandas as pd

# --------------------------------------------------------
# Function to clean raw scraped data into usable format
# Includes formatting dates, converting price to numeric,
# and geocoding address locations to lat/lon coordinates.
# --------------------------------------------------------

def clean_data(df):
    # Clean and standardize 'Sold Date' field
    date = []
    for i in df['Sold Date']:
        date.append(
            i.replace('Sold on ', '')  # Remove prefix
             .strip()                  # Remove leading/trailing spaces
             .replace(' ', '-')       # Change to YYYY-MMM-DD format (e.g., "12 Mar 2023" -> "12-Mar-2023")
        )

    # Convert 'Price' to integer format, remove '$' and commas
    price = []
    for i in df['Price']:
        rm = i.replace('$', '').replace(',', '')
        price.append(
            int(rm) if not pd.isna(i) else pd.NA  # Handle missing values safely
        )

    # Geocode the addresses to get Latitude and Longitude
    latitude, longitude = [], []
    geolocator_arcgis = ArcGIS()

    for i in df['Address']:
        try:
            getloc = geolocator_arcgis.geocode(i)
            if getloc:
                latitude.append(getloc.latitude)
                longitude.append(getloc.longitude)
            else:
                latitude.append(pd.NA)
                longitude.append(pd.NA)
        except:
            latitude.append(pd.NA)
            longitude.append(pd.NA)

    # Update the DataFrame with cleaned columns
    df[['Sold Date', 'Price', 'Latitude', 'Longitude']] = pd.DataFrame(
        list(zip(date, price, latitude, longitude)),
        columns=['Sold Date', 'Price', 'Latitude', 'Longitude']
    )

    # Convert Sold Date to datetime object
    df['Sold Date'] = pd.to_datetime(df['Sold Date'], errors='coerce')

    return df

# --------------------------------------------------------
# Function to extract all sold house listings from a page
# Parses HTML using BeautifulSoup and returns structured data
# --------------------------------------------------------

def data_extractor(temp):
    """
    Extracts all the houses data in a given page.

    Args:
        temp (Response): Response object returned by requests.get()

    Returns:
        list of dict: Extracted listings with keys like 'Address', 'Price', etc.
    """

    listings = BeautifulSoup(temp.text, 'html.parser')

    # Extract address titles (property headlines)
    addresses = [tag.text.strip() for tag in listings.select('a > h4')]

    # Extract property type badges (e.g., House, Apartment)
    property_types = [property.text for property in listings.find_all('div', class_='easy-block-v1-badge')]

    # Each listing has a <ul class="list-unstyled"> containing details like Sold Date, Price, Beds, etc.
    ul = listings.find_all('ul', class_='list-unstyled')

    houses_data = []
    for address, i, property_type in zip(addresses, ul, property_types):

        # Extract sold date
        try:
            sold_date = i.find_all('li')[0].find(string=compile('Sold'))
        except:
            sold_date = pd.NA

        # Extract price
        try:
            price = i.find_all('code')[0].find(string=compile(r'\$'))
        except:
            price = pd.NA

        # Extract number of bedrooms
        try:
            beds = i.find('i', class_='i-bed').find_next('big').text
        except:
            beds = pd.NA

        # Extract number of bathrooms
        try:
            bath_tag = i.find('i', class_='i-bath')
            if bath_tag:
                baths = bath_tag.nextSibling.strip() if bath_tag.nextSibling else pd.NA
            else:
                baths = pd.NA
        except:
            baths = pd.NA

        # Extract number of garages
        try:
            garage_tag = i.find('i', class_='i-car')
            if garage_tag:
                garages = garage_tag.nextSibling.strip() if garage_tag.nextSibling else pd.NA
            else:
                garages = pd.NA
        except:
            garages = pd.NA

        # Append the cleaned data into our results list
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
