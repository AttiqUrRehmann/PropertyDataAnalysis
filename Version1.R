library(tidyverse)
library(data.table)
library(lubridate)
library(rvest)
library(zoo)
library(tidyr)


suburb <- c(
  "Acton", "Ainslie", "Amaroo", "Aranda", "Banks", "Barton", "Belconnen", "Bonner", 
  "Bonython", "Booroomba", "Braddon", "Bruce", "Calwell", "Campbell", "Casey", "Chapman", 
  "Charnwood", "Chifley", "Chisholm", "City", "Conder", "Cook", "Coombs", "Crace", 
  "Curtin", "Deakin", "Dickson", "Downer", "Duffy", "Dunlop", "Evatt", "Fadden", 
  "Farrer", "Fisher", "Florey", "Flynn", "Forde", "Forrest", "Franklin", "Fraser", 
  "Fyshwick", "Garran", "Gilmore", "Giralang", "Gordon", "Gowrie", "Greenway", 
  "Griffith", "Gungahlin", "Hackett", "Hall", "Harrison", "Hawker", "Higgins", 
  "Holder", "Holt", "Hughes", "Hume", "Isaacs", "Isabella Plains", "Jacka", 
  "Kaleen", "Kambah", "Kingston", "Latham", "Lawson", "Lyneham", "Lyons", 
  "Macarthur", "Macgregor", "Macquarie", "Mawson", "McKellar", "Melba", 
  "Mitchell", "Monash", "Moncrieff", "Narrabundah", "Ngunnawal", "Nicholls", 
  "Oaks Estate", "O'Connor", "O'Malley", "Oxley", "Page", "Palmerston", 
  "Parkes", "Pearce", "Phillip", "Pialligo", "Red Hill", "Reid", "Richardson", 
  "Rivett", "Russell", "Scullin", "Spence", "Stirling", "Swinger Hill", 
  "Symonston", "Tharwa", "Theodore", "Torrens", "Tuggeranong", "Turner", 
  "Wanniassa", "Waramanga", "Watson", "Weetangera", "Weston", "Weston Creek", 
  "Wright", "Yarralumla")



postal_code <- c(
  2601,  # Acton
  2602,  # Ainslie
  2914,  # Amaroo
  2614,  # Aranda
  2906,  # Banks
  2600,  # Barton
  2617,  # Belconnen
  2914,  # Bonner
  2905,  # Bonython
  2620,  # Booroomba
  2612,  # Braddon
  2617,  # Bruce
  2905,  # Calwell
  2612,  # Campbell
  2913,  # Casey
  2611,  # Chapman
  2615,  # Charnwood
  2606,  # Chifley
  2905,  # Chisholm
  2601,  # City
  2906,  # Conder
  2614,  # Cook
  2611,  # Coombs
  2911,  # Crace
  2605,  # Curtin
  2600,  # Deakin
  2602,  # Dickson
  2602,  # Downer
  2611,  # Duffy
  2615,  # Dunlop
  2617,  # Evatt
  2904,  # Fadden
  2607,  # Farrer
  2615,  # Fisher
  2615,  # Florey
  2615,  # Flynn
  2914,  # Forde
  2603,  # Forrest
  2913,  # Franklin
  2615,  # Fraser
  2609,  # Fyshwick
  2605,  # Garran
  2905,  # Gilmore
  2617,  # Giralang
  2906,  # Gordon
  2904,  # Gowrie
  2900,  # Greenway
  2603,  # Griffith
  2912,  # Gungahlin
  2602,  # Hackett
  2618,  # Hall
  2914,  # Harrison
  2614,  # Hawker
  2615,  # Higgins
  2611,  # Holder
  2615,  # Holt
  2605,  # Hughes
  2620,  # Hume
  2607,  # Isaacs
  2905,  # Isabella Plains
  2914,  # Jacka
  2617,  # Kaleen
  2902,  # Kambah
  2604,  # Kingston
  2615,  # Latham
  2617,  # Lawson
  2602,  # Lyneham
  2620,  # Lyons
  2615,  # Macarthur
  2615,  # Macgregor
  2614,  # Macquarie
  2607,  # Mawson
  2617,  # McKellar
  2615,  # Melba
  2911,  # Mitchell
  2904,  # Monash
  2914,  # Moncrieff
  2604,  # Narrabundah
  2913,  # Ngunnawal
  2913,  # Nicholls
  2620,  # Oaks Estate
  2602,  # O'Connor
  2606,  # O'Malley
  2903,  # Oxley
  2614,  # Page
  2913,  # Palmerston
  2600,  # Parkes
  2607,  # Pearce
  2606,  # Phillip
  2609,  # Pialligo
  2603,  # Red Hill
  2612,  # Reid
  2905,  # Richardson
  2611,  # Rivett
  2600,  # Russell
  2615,  # Scullin
  2615,  # Spence
  2611,  # Stirling
  2611,  # Swinger Hill
  2609,  # Symonston
  2620,  # Tharwa
  2905,  # Theodore
  2607,  # Torrens
  2901,  # Tuggeranong
  2612,  # Turner
  2903,  # Wanniassa
  2611,  # Waramanga
  2602,  # Watson
  2614,  # Weetangera
  2611,  # Weston
  2611,  # Weston Creek
  2611,  # Wright
  2600   # Yarralumla
)

# parameters ------------------------------------------------------------------

props_to_pull <- 50       # pages of apartments to loop through

# main - scrape ---------------------------------------------------------------
urls <- character() # Initialize as an empty character vector
total_pages <- NA

for (i in 1:length(suburb)) {
  encoded_suburb <- URLencode(suburb[i]) # URL encode the suburb name
  current_url <- paste0(
    "https://www.auhouseprices.com/sold/list/ACT/",
    postal_code[i],
    "/",
    encoded_suburb,
    "/",
    1:total_pages, # or a variable representing the page number
    "/"
  )


for (i in 1:length(urls)) {
  
  curr_url <- urls[[i]]
  print(paste0('getting ', i))
  temp <- read_html(curr_url)
  if (i == 1) {
    total_pages <- temp %>%
      html_nodes('div.headline h2') %>%
      html_text() %>%
      first() %>%                      # Get only the first element
      str_extract("\\d+$") %>%         # Extract the last number
      as.numeric() %>%                 # Convert to numeric
      `/`(12) %>%                      # Divide by 12
      round()  
  }
  
  # sleeping for 2 seconds so as not to bombard the server with requests
  print('sleeping')
  Sys.sleep(2)
  
  address <- temp %>%
    html_nodes('h4') %>%
    html_text() %>%
    str_extract_all("^\\d+[\\/\\w\\s]+") %>%
    unlist()
  
  date_sold <- temp %>%
    html_nodes("li") %>%
    html_text() %>%
    str_extract_all("Sold on \\d{1,2} [A-Za-z]{3} \\d{4}") %>%
    unlist() %>% 
    str_remove("Sold on ")
  
  Agency <- temp %>%
    html_nodes("li") %>%
    html_text() %>%
    str_extract_all("Agency: .*") %>%
    unlist() %>%
    str_remove("Agency: ")
  
  temp_second <- temp %>%
    html_nodes("li") %>%
    html_text() %>%
    str_extract_all("\\d+ \\d+ \\d+ \\$[0-9,]+") %>%
    unlist() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rename(Matches = ".") %>%  # Rename the default column name
    mutate(
      Address = address,
      Date_sold = date_sold,
      Agency = Agency,
      Beds = str_extract(Matches, "^\\d+"),
      Baths = str_extract(Matches, "\\d+(?= \\d+ \\$)"),
      Garage = str_extract(Matches, "\\d+(?= \\$)"),
      Price = as.numeric(gsub("[\\$,]", "", str_extract(Matches, "\\$[0-9,]+")))
    ) %>%
    select(Address, Date_sold, Agency,
           Beds, Baths, Garage, Price)
  
  sold_houses <- bind_rows(sold_houses, temp_second)
}

}