# -------------------------------------------------------------
# Script to scrape sold house listings from AUHousePrices.com,
# clean and structure the data using BeautifulSoup, and prepare 
# it for modeling (e.g., predicting house prices).
# -------------------------------------------------------------

import requests
from bs4 import BeautifulSoup
import suburbs  # Custom module containing suburb info (not shown here)
from re import compile, findall
from math import ceil
import utils  # Custom module with data_extractor and clean_data functions
import pandas as pd

# Base URL for sold property listings
base_url = 'https://www.auhouseprices.com/sold/'

def making_url(suburb, postal_code, page_number, state):
    """
    Constructs the full URL for a given suburb and page number.

    Args:
        suburb (str): Name of the suburb (e.g., "Weston Creek")
        postal_code (int): Postal code of the suburb (e.g., 2611)
        page_number (int): Page number to fetch
        state (str): Australian state abbreviation (e.g., "ACT")

    Returns:
        str: A formatted URL to fetch sold property listings
    """
    if ' ' in suburb:
        suburb = suburb.replace(' ', '+')  # Replace space with '+' for valid URL
    url = f'{base_url}list/{state}/{postal_code}/{suburb}/{page_number}/'
    return url 


def getting_data(suburb, postal_code, state):
    """
    Collects and returns sold house listings as a cleaned pandas DataFrame.

    Args:
        suburb (list): List of suburb names
        postal_code (list): Corresponding list of postal codes
        state (str): Australian state abbreviation

    Returns:
        pd.DataFrame: Cleaned DataFrame containing sold listings
    """

    # Handle both single and multiple suburb input gracefully
    if len(suburb) == 1:
        l = 1 
    else:
        l = len(suburb) + 1  # This should probably just be len(suburb), see note below

    sold_houses_data = []

    for i in range(0, l):
        # Construct the first page URL to get the total number of pages
        temp_url = making_url(suburb[i], postal_code[i], 1, state)

        try:
            temp = requests.get(temp_url)
        except:
            temp = None  # If request fails, skip this suburb
        
        if temp is not None:
            soup = BeautifulSoup(temp.text, 'html.parser')

            # Find the section indicating how many listings are available
            try:
                tag = soup.find('h2', string=compile('Displaying')).text
            except:
                continue  # If structure doesn't match, skip this page

            # Extract the total number of listings from the text (e.g., "Displaying 1-12 of 134")
            total_pages = findall(r'\d+', tag)[-1]
            total_pages = ceil(int(total_pages) / 12)  # 12 listings per page

            # Iterate through each page to collect all listings
            for page in range(1, total_pages + 1):
                url = making_url(suburb[i], postal_code[i], page, state)

                try:
                    temp = requests.get(url)
                except:
                    temp = None

                if temp is not None:
                    # Extract raw listings from HTML using the utility function
                    data = utils.data_extractor(temp)
                    sold_houses_data.extend(data)

    # Convert the list of listing dictionaries into a DataFrame
    df = pd.DataFrame(sold_houses_data)

    # Clean and enrich the raw data using utility functions (e.g., fix dates, add coordinates)
    df = utils.clean_data(df)

    return df


# -------------------------------------------------------------------------
# Example usage: Get data for sold properties in Weston Creek, ACT (postcode 2611)
# -------------------------------------------------------------------------
data_extracted = getting_data(['Weston Creek'], [2611], 'ACT')


# Add a new column 'Sold year' by extracting the year from the sale date
data_extracted['Sold year'] = pd.DataFrame(data_extracted['Sold Date'].dt.year)


# Convert key columns to integers where possible (some values might be missing or malformed)
# 'Int64' type supports pandas NA values (i.e., nullable integers)
data_extracted[['Beds', 'Baths', 'Garages', 'Price']] = data_extracted[
    ['Beds', 'Baths', 'Garages', 'Price']
].apply(lambda col: pd.to_numeric(col, errors='coerce').astype('Int64'))



