import requests
from bs4 import BeautifulSoup
import suburbs
from re import (
    compile,
    findall
)
from math import ceil
import utils
import pandas as pd

base_url = 'https://www.auhouseprices.com/sold/'

def making_url(suburb, postal_code, page_number, state):
    """
    Give url of the required data.

    Parameters:
    suburb (str): suburb name
    postal_code (int): postal code of the suburb entered
    page_number (int): page number as sold houses of suburb can be on many pages and we need to extract them all
    state (str): Australian state where the suburb is.

    Returns:
    str: url of sold houses of given suburb(s)
    """

    if ' ' in suburb:
        suburb = suburb.replace(' ', '+')
        url = f'{base_url}list/{state}/{postal_code}/{suburb}/{page_number}/'
    else:
        url = f'{base_url}list/{state}/{postal_code}/{suburb}/{page_number}/'
    return url 


def getting_data(suburb, postal_code, state):
    """
    Give resulting data in a dictionary that can easily be converted to pandas data frame.

    Parameters:
    suburb (str): suburb name
    postal_code (int): postal code of the suburb entered
    state (str): Australian state where the suburb is.

    Returns:
    dict: gives data of all the houses of given suburb(s)
    """

    if len(suburb) == 1:
        l = 1 
    else:
        l = len(suburb)+1

    sold_houses_data = []

    for i in range(0, l):
        temp_url = making_url(suburb[i], postal_code[i], 1, state)

        try:
            temp = requests.get(temp_url)
        except:
            temp = None
        
        if temp != None:
            soup = BeautifulSoup(temp.text, 'html.parser')
            tag = soup.find('h2', string= compile('Displaying')).text

            total_pages = findall(r'\d+', tag)[-1]
            total_pages = ceil(int(total_pages)/12)

            for page in range(1, total_pages+1):

                url = making_url(suburb[i], postal_code[i], page, state)

                try:
                    temp = requests.get(url)
                except:
                    temp = None

                if temp != None:
                    data = utils.data_extractor(temp)
                    sold_houses_data.extend(data)
    
    df = pd.DataFrame(sold_houses_data)

    df = utils.clean_data(df)

    return df
                    



data_extracted = getting_data(['Weston Creek'], [2611], 'ACT')

data_extracted