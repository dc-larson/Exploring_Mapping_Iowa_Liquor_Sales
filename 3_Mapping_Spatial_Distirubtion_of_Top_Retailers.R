## Mapping Top Performing Locations


# In th previous exercise we found that Black Velvet and Hawkeye Vodka were the top
# seling liquor brands in Iowa.
# It would be interesting to plot the spatial distribution of these stores to see if any illustrative spatial patterns emerge.
# In addition, we have a hypothesis we would like to test.

#Hypothesis: If stores are top sellers of Back Velvet or Hawkeye Vodka, then 
# they will be located near college campuses.

# Install/Load Packages
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)


# Data -  load data
# In order to do this we must extract data from the IPEDS database containing information
# on all 4-year colleges within the U.S.
# The dataset can be found here : https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx

ipeds <- read_csv("./Data/IPEDS2018.csv")
ipeds <- as.data.frame(ipeds)
# Load in Top Selling stores data from previous exercise

hawkeye <- read_csv("./Data/hvdc_latlon.csv")
blackvelvet <- read_csv("./Data/bvdc_latlon.csv")

# Inspect the data

glimpse(hawkeye)
glimpse(blackvelvet)
glimpse(ipeds)

# IPEDS contain a lot of data we don't really need. Subset data related to insitution name and location.

ipeds_iowa <- ipeds %>% 
      select(INSTNM,IALIAS,ADDR,CITY,STABBR,ICLEVEL,ZIP,LONGITUD,LATITUDE) %>% 
      dplyr::filter(STABBR == "IA", ICLEVEL == '1') %>% 
      rename(lon = LONGITUD, lat = LATITUDE)

# I filtered for schools in Iowa that were four years or more. I wanted to try to remove
# Community colleges and vocational schools as they are less likely to have a student population 
# residing on or near campus.


# Let's start fresh by just plotting colleges first and calling that our base map.
# First we need to generate some icons for universities. Some pretty neat icons can be generated at
#[Font Awesome](https://fontawesome.com/icons/university?style=light)
college_icons <- awesomeIcons(icon='university',
                              library = 'fa',
                              markerColor = 'orange')

# This is our base map
map<- leaflet() %>% 
      addProviderTiles("CartoDB")

# Let's save out one for college to making plotting easier later.

college_map <- map

#So now we have our base map. Now let's add the icons we made.

college_map <- college_map %>% 

  addAwesomeMarkers(data=ipeds_iowa,icon =college_icons, 
                   popup = ~INSTNM,group='Schools',
                   clusterOptions = markerClusterOptions())  
                   


# The clusterOptions argument clusters them in regions so the map is
# More readable

# 
# Add a color pallette

pal <- colorFactor(palette = c("blue","red"),
                   domain = c("Black Velvet", "Hawkeye Vodka"))

liquor_collegemap <- college_map %>%
                    addCircleMarkers(data = blackvelvet,radius = 2,
                     color= ~pal(Item_Description),
                     group = "Black Velvet",
                      label = ~Store_Name) %>% 
  
                  addCircleMarkers(data = hawkeye,radius = 2,
                   color= ~pal(Item_Description),
                   group = "Hawkeye Vodka",
                   label = ~Store_Name) %>%
  
                  addLayersControl(
                    overlayGroups = c("Black Velvet",
                                      "Hawkeye Vodka")) %>% 
                  addLegend("bottomright", colors= c("blue", "red"), 
                    labels=c("Black Velvet", "Hawkeye")) %>% 
                  addMeasure()
            


# Conclusions
#From this mapping exercise we can refute our hypothesis that 
#the best selling stores will be near college campuses. It appears the 
#more likely explanation is that the best selling store tend to be 
#located in larger population centers near major roads and receive more 
#customer traffic. Consider adding radius to colleges.


