library(tidyverse) #to cleaning data
library(sf) #read shapefile containing geospatial data
library(tmap) #create thematic maps

#download and unzip the shapefile
download.file("https://www.aec.gov.au/electorates/gis/files/act-july-2018-esri.zip", destfile = "Electorate_Boundaries.zip") 
unzip("Electorate_Boundaries.zip")

download.file("https://www.aec.gov.au/electorates/gis/files/act-july-2018-esri.zip")

#read the shapefile using the `rgdal` library
aus_electorate_map <- st_read("E_ACT_18_region.shp")

aus_electorate_map <- st_crop(aus_electorate_map, 
                              xmin = 148, xmax = 150, 
                              ymin = -36, ymax = -35)

aus_electorate_map <- st_make_valid(aus_electorate_map)

yearly_data <- clean_data %>%
  mutate(year_sold = year(sold_date)) %>%
  filter(year_sold == 2025)

real_estate_sf <- st_as_sf(yearly_data, coords = c("longitude", "latitude"), crs = 4326)

real_estate_sf <- st_transform(real_estate_sf, st_crs(aus_electorate_map))


tmap_mode("view")

tm_shape(aus_electorate_map) + 
  #tm_polygons() +  # Base map for electorate regions
  tm_shape(real_estate_sf) + 
  tm_symbols(col = "price", palette = "viridis", title.col = "Property Price")+
  tm_legend(show = TRUE)


tmap_save(
  tm_shape(aus_electorate_map) + 
    tm_shape(real_estate_sf) + 
    tm_symbols(col = "price", palette = "viridis", title.col = "Property Price"),
  filename = "real_estate_map.html"
)
