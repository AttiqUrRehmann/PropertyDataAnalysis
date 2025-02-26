library(tidyverse)
library(rvest)
library(stringr)

# Define suburbs and postal codes
suburb <- c(
  "Acton", "Ainslie", "Amaroo", "Aranda", "Banks", "Barton", "Belconnen", "Bonner", 
  "Bonython", "Booroomba", "Braddon", "Bruce", "Calwell", "Campbell", "Casey", "Chapman", 
  "Charnwood", "Chifley", "Chisholm", "City", "Conder", "Cook", "Coombs", "Crace", 
  "Curtin", "Deakin", "Dickson", "Downer", "Duffy", "Dunlop", "Evatt", "Fadden", 
  "Farrer", "Fisher", "Florey", "Flynn", "Forde", "Forrest", "Franklin", "Fraser", 
  "Fyshwick")

# c("Garran", "Gilmore", "Giralang", "Gordon", "Gowrie", "Greenway", 
#   "Griffith", "Gungahlin", "Hackett", "Hall", "Harrison", "Hawker", "Higgins", 
#   "Holder", "Holt", "Hughes", "Hume", "Isaacs", "Isabella Plains", "Jacka", 
#   "Kaleen", "Kambah", "Kingston", "Latham", "Lawson", "Lyneham", "Lyons", 
#   "Macarthur", "Macgregor", "Macquarie", "Mawson", "McKellar", "Melba", 
#   "Mitchell", "Monash", "Moncrieff", "Narrabundah", "Ngunnawal", "Nicholls", 
#   "Oaks Estate", "O'Connor", "O'Malley", "Oxley", "Page", "Palmerston", 
#   "Parkes", "Pearce", "Phillip", "Pialligo", "Red Hill", "Reid", "Richardson", 
#   "Rivett", "Russell", "Scullin", "Spence", "Stirling", "Swinger Hill", 
#   "Symonston", "Tharwa", "Theodore", "Torrens", "Tuggeranong", "Turner", 
#   "Wanniassa", "Waramanga", "Watson", "Weetangera", "Weston", "Weston Creek", 
#   "Wright", "Yarralumla")


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
  2609)  # Fyshwick


#   c(2605,  # Garran
#   2905,  # Gilmore
#   2617,  # Giralang
#   2906,  # Gordon
#   2904,  # Gowrie
#   2900,  # Greenway
#   2603,  # Griffith
#   2912,  # Gungahlin
#   2602,  # Hackett
#   2618,  # Hall
#   2914,  # Harrison
#   2614,  # Hawker
#   2615,  # Higgins
#   2611,  # Holder
#   2615,  # Holt
#   2605,  # Hughes
#   2620,  # Hume
#   2607,  # Isaacs
#   2905,  # Isabella Plains
#   2914,  # Jacka
#   2617,  # Kaleen
#   2902,  # Kambah
#   2604,  # Kingston
#   2615,  # Latham
#   2617,  # Lawson
#   2602,  # Lyneham
#   2620,  # Lyons
#   2615,  # Macarthur
#   2615,  # Macgregor
#   2614,  # Macquarie
#   2607,  # Mawson
#   2617,  # McKellar
#   2615,  # Melba
#   2911,  # Mitchell
#   2904,  # Monash
#   2914,  # Moncrieff
#   2604,  # Narrabundah
#   2913,  # Ngunnawal
#   2913,  # Nicholls
#   2620,  # Oaks Estate
#   2602,  # O'Connor
#   2606,  # O'Malley
#   2903,  # Oxley
#   2614,  # Page
#   2913,  # Palmerston
#   2600,  # Parkes
#   2607,  # Pearce
#   2606,  # Phillip
#   2609,  # Pialligo
#   2603,  # Red Hill
#   2612,  # Reid
#   2905,  # Richardson
#   2611,  # Rivett
#   2600,  # Russell
#   2615,  # Scullin
#   2615,  # Spence
#   2611,  # Stirling
#   2611,  # Swinger Hill
#   2609,  # Symonston
#   2620,  # Tharwa
#   2905,  # Theodore
#   2607,  # Torrens
#   2901,  # Tuggeranong
#   2612,  # Turner
#   2903,  # Wanniassa
#   2611,  # Waramanga
#   2602,  # Watson
#   2614,  # Weetangera
#   2611,  # Weston
#   2611,  # Weston Creek
#   2611,  # Wright
#   2600   # Yarralumla
# )

# Initialize results
sold_houses <- tibble()

extract_data <- function(temp, suburb_name) {
  tryCatch({
    listings <- temp
    
    tibble(
      suburb = suburb_name,
      Full_Address = listings %>%
        html_elements("h4") %>%
        html_text(trim = TRUE) %>%
        head(12),
      Date_Sold = listings %>%
        html_elements(".list-unstyled") %>%
        html_text(trim = TRUE) %>%
        head(12) %>% lapply(function(x) {
          list(
            sold_date = str_extract(x, "Sold on \\d{1,2} \\w+ \\d{4}") %>% 
              #str_remove("Sold on ") %>% 
              ifelse(. == "", NA, .),
            land_size = str_extract(x, "(?<=Land Size: )\\d+(?= m2)") %>%
              ifelse(. == "", NA, .),
            agency = str_extract(x, "(?<=Agency: ).*") %>%
              ifelse(. == "", NA, .)
          )
        }) %>% 
        bind_rows() %>%
        pull(1),
      Beds = listings %>% html_elements(".i-bed +big") %>% html_text(trim = TRUE) %>% as.integer(),
      Baths = listings %>%
        html_elements(".i-bed + big") %>%
        map_int(~ {
          # Check if .i-car exists after .i-bath
          car_element <- html_elements(.x, xpath = "following-sibling::i[contains(@class, 'i-bath')]")
          
          if (length(car_element) == 0) {
            NA  # If .i-car is missing, return NA
          } else {
            # Extract the following text (garage count)
            html_text(html_nodes(car_element, xpath = "following-sibling::text()[1]"), trim = TRUE) %>%
              as.integer()
          }
        }),
      Garage = listings %>%
        html_elements(".i-bed + big") %>%
        map_int(~ {
          # Check if .i-car exists after .i-bath
          car_element <- html_elements(.x, xpath = "following-sibling::i[contains(@class, 'i-car')]")
          
          if (length(car_element) == 0) {
            NA  # If .i-car is missing, return NA
          } else {
            # Extract the following text (garage count)
            html_text(html_nodes(car_element, xpath = "following-sibling::text()[1]"), trim = TRUE) %>%
              as.integer()
          }
        }),
      Price = listings %>% 
        html_elements(".pull-right code") %>% 
        html_text(trim = TRUE)  %>%
        str_remove_all("[^0-9]") %>% as.numeric(),
      Land_Size = listings %>%
        html_elements(".list-unstyled") %>%
        html_text(trim = TRUE) %>%
        head(12) %>% lapply(function(x) {
          list(
            sold_date = str_extract(x, "Sold on \\d{1,2} \\w+ \\d{4}") %>% 
              str_remove("Sold on ") %>% 
              ifelse(. == "", NA, .),
            land_size = str_extract(x, "(?<=Land Size: )\\d+(?= m2)") %>%
              ifelse(. == "", NA, .),
            agency = str_extract(x, "(?<=Agency: ).*") %>%
              ifelse(. == "", NA, .)
          )
        }) %>% 
        bind_rows() %>%
        pull(2),
      Agency = listings %>%
        html_elements(".list-unstyled") %>%
        html_text(trim = TRUE) %>%
        head(12) %>% lapply(function(x) {
          list(
            sold_date = str_extract(x, "Sold on \\d{1,2} \\w+ \\d{4}") %>% 
              str_remove("Sold on ") %>% 
              ifelse(. == "", NA, .),
            land_size = str_extract(x, "(?<=Land Size: )\\d+(?= m2)") %>%
              ifelse(. == "", NA, .),
            agency = str_extract(x, "(?<=Agency: ).*") %>%
              ifelse(. == "", NA, .)
          )
        }) %>% 
        bind_rows() %>%
        pull(3)
    )
  }, error = function(e) {
    message("Error extracting data for ", suburb_name, ": ", e$message)
    return(tibble())
  })
}

# Scraping loop
for (i in seq_along(suburb)) {
  encoded_suburb <- URLencode(suburb[i])
  first_url <- paste0("https://www.auhouseprices.com/sold/list/ACT/", postal_code[i], "/", encoded_suburb, "/1/")
  
  print(paste0("Fetching first page for ", suburb[i]))
  temp <- tryCatch(read_html(first_url), error = function(e) NULL)
  
  if (!is.null(temp)) {
    total_results <- temp %>%
      html_nodes('div.headline h2') %>%
      html_text() %>%
      first() %>%
      str_extract("\\d+$") %>%
      as.numeric() %>%
      `/`(12) %>%
      ceiling()
    
    print(paste0("Total pages for ", suburb[i], ": ", total_results))
    
    for (j in 1:total_results) {
      page_url <- paste0("https://www.auhouseprices.com/sold/list/ACT/", postal_code[i], "/", encoded_suburb, "/", j, "/")
      print(paste0("Fetching page ", j, " of ", total_results, " for ", suburb[i]))
      
      temp_page <- tryCatch(read_html(page_url), error = function(e) NULL)
      
      if (!is.null(temp_page)) {
        data <- extract_data(temp_page, suburb[i])
        sold_houses <- bind_rows(sold_houses, data)
      }
      
      Sys.sleep(runif(1, 2, 5))
      closeAllConnections()
    }
  }
}

# Save the results
print(head(sold_houses))
write_csv(sold_houses, "sold_houses_till_Dunlop.csv")
