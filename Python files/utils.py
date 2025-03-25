from bs4 import BeautifulSoup
from re import compile



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

    ul = listings.find_all('ul', class_ = 'list-unstyled')


    houses_data = []
    for address, i in zip(addresses, ul):

        try:
            sold_date = i.find_all('li')[0].find(string = compile('Sold')).text
        except:
            sold_date = 'N/A'

        try: 
           price =  i.find_all('code')[0].find(string = compile('$')).text
        except:
            price = 'N/A'

        try: 
            beds = i.find('i', class_ = 'i-bed').find_next('big').text
        except:
            beds = 'N/A'
        
        try: 
            bath_tag = i.find('i', class_ = 'i-bath')
            if bath_tag:
                baths = bath_tag.nextSibling
            else:
                baths = 'N/A'
        except:
            baths = 'N/A'

        try: 
            garage_tag = i.find('i', class_ = 'i-car')
            if garage_tag:
                garages = garage_tag.nextSibling
            else:
                garages = 'N/A'
        except:
            garages = 'N/A'
        
        houses_data.append({
            'Address': address,
            'Sold Date': sold_date,
            'Price': price,
            'Beds': beds,
            'Baths': baths,
            'Garages': garages
        })

    return houses_data