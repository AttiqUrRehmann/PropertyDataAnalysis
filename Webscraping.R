options(warn = 2)
rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(zoo)

# parameters ------------------------------------------------------------------

prop_config <- c(2, 1, 1) # target apartment configuration
props_to_pull <- 50       # pages of apartments to loop through

# main - scrape ---------------------------------------------------------------

# set up a vector of URLs which we will be looping through
urls <- paste0('https://www.auhouseprices.com/rent/list/NSW/2131/Ashfield/',
               1:props_to_pull, '/?sort=date&type=apartment&bmin=',
               prop_config[1], '&bmax=', prop_config[1])

for (i in 1:length(urls)) {
  
  if(i == 1 & exists('rent_all')) rm(rent_all)
  
  curr_url <- urls[[i]]
  print(paste0('getting ', i))
  temp <- read_html(curr_url)
  
  # sleeping for 2 seconds so as not to bombard the server with requests
  print('sleeping')
  Sys.sleep(2)
  
  address <- temp %>%
    html_nodes('h4') %>%
    html_text() %>%
    .[which(. != ' Search Filter and Sorting ')]
  
  price_month <- temp %>%
    html_nodes('li') %>%
    html_text() %>%
    str_extract('^Rent.+/week.*\\d{4}$') %>%
    .[which(!is.na(.))]
  
  config <- temp %>%
    html_nodes('li') %>%
    html_text() %>%
    str_extract(' \\d \\d \\d*[ ]*$') %>%
    .[which(!is.na(.))]
  
  combined <- data.table(address, price_month, config)
  
  # append results of this iteration to our master data set
  if(!exists('rent_all')) {
    rent_all <- combined
  } else {
    rent_all <- rbind(rent_all, combined)
  }
}

# extract month
rent_all$month <- str_extract(rent_all$price_month, '[A-Z][a-z]{2} \\d{4}$')
rent_all$month <- dmy(paste0('01 ', rent_all$month))

# extract price
rent_all$price <- str_extract(rent_all$price_month, '(?<=Rent \\$).*(?=/week)')
rent_all$price <- as.numeric(rent_all$price)

# remove any dups
rent_all <- rent_all[!duplicated(rent_all)]

# subset to view only those matching property configuration specified above
pattern <- paste0(prop_config[[1]], '\\s', prop_config[[2]], '\\s'
                  ,prop_config[[3]])

# create our analytical dataset
ads <- rent_all[grepl(pattern, rent_all$config), ]

# analyse ---------------------------------------------------------------------

# pre-smoothing

ads %>%
  ggplot(aes(x = factor(format(month, '%b %Y')), y = price)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  coord_flip() +
  labs(x = 'Month rented', y = 'Weekly rent',
       title = 'Distribution of weekly rent')

# smoothing using rolling quarterly median

monthly_medians <- ads %>%
  group_by(month) %>%
  summarise(median_price = median(price))

rol_median <- rollmedian(monthly_medians$median_price, 3, na.pad = TRUE,
                         align = 'right')
names(rol_median) <- monthly_medians$month
rol_median <- data.table(month = as.Date(names(rol_median)),
                         rol_median = rol_median)
rol_median <- rol_median[!is.na(rol_median), ]

rol_median %>%
  ggplot(aes(x = month, y = rol_median)) +
  geom_bar(stat = 'identity') +
  coord_cartesian(ylim = c(400, 600)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = '1 month') +
  labs(x = 'Month rented', y = 'Smoothed weekly rent',
       title = 'Weekly rental prices in Ashfield',
       subtitle = 'Smoothed by rolling quarterly median')

# smoothing using LOESS

ads %>%
  ggplot(aes(x = month, y = price)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = 'loess', span = 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = '1 month') +
  labs(x = 'Month rented', y = 'Smoothed weekly rent',
       title = 'Weekly rental prices in Ashfield',
       subtitle = 'Smoothed using LOESS')