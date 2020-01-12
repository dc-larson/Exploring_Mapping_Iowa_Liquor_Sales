# Section 2: Exploratory Data Analysis (EDA)

#Questions:
#1) Which counties are the top 10 in sales?
#2) What are the top 10 most valuable products in terms of sales?
#3) What are the most popular liqour brands



# Load Packages
library(tidyverse)
library(lubridate)
library(forcats)

load("./data/retail_cleaned.RData")

# Inspect the Data

glimpse(retail_cleaned)

# Summarize the data by the counties that have the most sales.


county_mostsales<-retail_cleaned %>% 
  group_by(County) %>% 
  summarize(countywise_sales = sum(Sale_Dollars)/1000000) %>% 
  arrange(desc(countywise_sales))

## Identify Top 10 Counties

by_top10_counties <- county_mostsales %>% 
  top_n(n = 10, wt = countywise_sales)

# Visualize the top 10 counties as per total sales.

 
  by_top10_counties %>% 
    mutate(County = fct_reorder(County, countywise_sales)) %>% 
    ggplot(aes(County, countywise_sales))+
    geom_bar(stat = "identity",fill = "green4")+
  theme_minimal()+
  ggtitle('Top 10 Liquor Selling Counties')+
  coord_flip()+
  ylab('Total Sales ($M)')+
  theme(plot.title = element_text(hjust = 0.5))
 

## So Polk county has the most sales by an order of magnitude.

# Polk county contains the state capital, Des Moines, and is the most populous
# county in Iowa according to World Population Review http://worldpopulationreview.com/us-counties/ia/

#Let's remove Polk county and then look at the rest of the data in the top 10 counties

county_mostsales_without_polk <-  county_mostsales[-1,]  
  
by_top10_counties_without_polk <- county_mostsales_without_polk %>% 
  top_n(n=10, wt = countywise_sales )



by_top10_counties_without_polk %>% 
  mutate(County = fct_reorder(County, countywise_sales)) %>% 
  ggplot(aes(County, countywise_sales))+
  geom_bar(stat = "identity",fill = "green4")+
  theme_minimal()+
  ggtitle('Top 10 Liquor Selling Counties')+
  coord_flip()+
  ylab('Total Sales ($M)')+
  theme(plot.title = element_text(hjust = 0.5))

# So without Polk county, Linn county is the second leader in Liquor sales in the state of Iowa
# With $125M in sales for the reporting period.


# What are the most valued products?

mostvalued_product <- retail_cleaned %>% 
  group_by(Item_Description) %>%
  summarize(value_of_product = sum(Sale_Dollars)/1000000) %>% 
  arrange(desc(value_of_product)) 

# Top 10 most valuable products

top10_mostvalued_products <- mostvalued_product %>% 
  top_n(n = 10, wt = value_of_product)

# The categorical variables "Item Description" does not have an inherent order.
# Reorder it as an increasing count.

# Visualize Top 10 Best Selling Products

  top10_mostvalued_products %>% 
  mutate(Item_Description = fct_reorder(Item_Description, value_of_product)) %>% 
  
  ggplot(aes(Item_Description, value_of_product))+
   
    geom_bar(stat = "identity",fill = "green4")+
    theme_minimal()+
    ggtitle('Top 10 Most Popular Products')+
    coord_flip()+
    ylab('Total Sales ($M)')+
    theme(plot.title = element_text(hjust = 0.5))
 

# Interesetingly Canadian Whisky is the most valued Liquor product in America's Heartland.
# Followed by Jack Daniels, then Captain Morgan.

# What are the best selling liquor brands in Iowa

mostsold_brand <- retail_cleaned %>% 
  group_by(Item_Description) %>% 
  summarize(total_units_sold = sum(Bottles_Sold)/1000000) %>% 
  arrange(desc(total_units_sold))

# What are the top 10

top10_mostsold_brand <- mostsold_brand %>% 
  top_n(n=10,wt=total_units_sold)

most_sold<- top10_mostsold_brand %>% 
  mutate(Item_Description = fct_reorder(Item_Description, total_units_sold)) %>% 
  ggplot(aes(Item_Description, total_units_sold))+
  geom_bar(stat = "identity",fill = "green4")+
  theme_minimal()+
  ggtitle('Top 10 Most Sold Brands')+
  coord_flip()+
  ylab('Total Sales ($M)')+
  theme(plot.title = element_text(hjust = 0.5))



# Black Velvet again. It is the most sold and accounts for the most value followed
# by Hawkeye Vodka (This seems logical given their passion for college football) and Captain Morgan Spiced Rum.

# Figure out which stores sold the most Black Velvet and Hawkeye Vodka.

# First we must remove empty spaces in Item Description names
 

blackvelvet <- retail_cleaned %>% 
   filter(Item_Description == "Black Velvet") %>% 
  group_by(Store_Number) %>% 
  summarize(allSold = sum(Bottles_Sold),
            totalSales = sum(Sale_Dollars),
            totalvolGal = sum(Volume_Sold_Gallons)) %>% 
  arrange(desc(allSold)) %>%
  top_n(n=10, wt = allSold)



# Use store number for the join. This is the "key"
# Create data frame just with locations and pertinent summary information.

top10bvstores<- retail_cleaned %>% 
  select(Store_Number,Store_Name,Address,City,Zip_Code,Store_Location,
         County_Number,Category_Name,Vendor_Name,Item_Description) %>% 
  filter(Item_Description == "Black Velvet") %>% 
  arrange(desc(Store_Number)) %>% 
  distinct(Store_Number,.keep_all = TRUE)
  
bv_most_pop<- top10bvstores %>% 
  inner_join(blackvelvet,top10bvstores, by = "Store_Number")

#That's it. Now find way to break out spatial coordinates to stores
##---> Turn string into data list, then dataframe
# Then repeat for Hawkeye Vodka.


bv_data_clean <- c("7205 MILLS CIVIC PKWY\nWEST DES MOINES 50266\n(41.561342, -93.806489)",
                  "305 AIRPORT RD\nAMES 50010\n(42.001123, -93.61365)",                   
                  "210 EAST TOWER PARK DR\nWATERLOO 50702\n(42.456362, -92.352552)",
                  "4201 S. YORK ST.\nSIOUX CITY 51106\n(42.433711, -96.370146)",          
                  "1101 73RD STREET\nWINDSOR HEIGHTS 50311\n(41.599172, -93.718027)",     
                  "1610 OKOBOJI AVENUE\nMILFORD 51351\n(43.331525, -95.149955)",          
                  "2605 BLAIRS FERRY RD NE\nCEDAR RAPIDS 52402\n(42.034737, -91.679406)", 
                  "1511 2ND AVE NORTH\nFORT DODGE 50501\n(42.508344, -94.177165)",        
                  "3221 SE 14TH ST\nDES MOINES 50320\n(41.554101, -93.596754)",           
                  "1201 12TH AVE SW\nLEMARS 51031\n(42.778257, -96.18335)"               
                  )      
     
bv_data_clean <- as.data.frame(bv_data_clean)


#Separate out lat and long and bind them to original Black Velvet Top 20 Dataset.
#Then now write it to a .csv

bvdc_latlon <-bv_data_clean%>%
  mutate(bv_data_clean = gsub('\n', '', bv_data_clean)) %>%
  extract(bv_data_clean, into = c('address', 'lat', 'lon'), 
          regex = '(.*)\\((.*),\\s+(.*)\\)', convert = TRUE) %>% 
          bind_cols(bv_most_pop) %>% 
          write.csv(.,file = "bvdc_latlon.csv")

# Good. Now complete the same steps for Hawkeye Vodka

hawkeye <- retail_cleaned %>% 
  filter(Item_Description == "Hawkeye Vodka") %>% 
  group_by(Store_Number) %>% 
  summarize(allSold = sum(Bottles_Sold),
            totalSales = sum(Sale_Dollars),
            totalvolGal = sum(Volume_Sold_Gallons)) %>% 
  arrange(desc(allSold)) %>%
  top_n(n=10, wt = allSold)


top10hvstores<- retail_cleaned %>% 
  select(Store_Number,Store_Name,Address,City,Zip_Code,Store_Location,
         County_Number,Category_Name,Vendor_Name,Item_Description) %>% 
  filter(Item_Description == "Hawkeye Vodka") %>% 
  arrange(desc(Store_Number)) %>% 
  distinct(Store_Number,.keep_all = TRUE)

hv_most_pop<- top10hvstores %>% 
  inner_join(hawkeye,top10hvstores, by = "Store_Number")

hv_data_clean <- c( "1501 MICHIGAN AVE\nDES MOINES 50314\n(41.605561, -93.613738)",          
                    "1373 PIERCE ST\nSIOUX CITY 51105\n(42.504732, -96.405013)",             
                    "507 W 19th St\nSIOUX CITY 51103\n(42.510535, -96.420193)",              
                    "7205 MILLS CIVIC PKWY\nWEST DES MOINES 50266\n(41.561342, -93.806489)", 
                    "1101 73RD STREET\nWINDSOR HEIGHTS 50311\n(41.599172, -93.718027)",      
                    "1511 2ND AVE NORTH\nFORT DODGE 50501\n(42.508344, -94.177165)",         
                    "3221 SE 14TH ST\nDES MOINES 50320\n(41.554101, -93.596754)",            
                    "4100 UNIVERSITY AVE\nDES MOINES 50311\n(41.600361, -93.673223)",        
                    "2001 BLAIRS FERRY ROAD NE\nCEDAR RAPIDS 52402\n(42.034799, -91.668909)",
                    "551 S ILLINOIS AVE\nMASON CITY 50401\n(43.14623, -93.17114)"           
                    )

hv_data_clean <- as.data.frame(hv_data_clean)

hvdc_latlon <-hv_data_clean%>%
  mutate(hv_data_clean = gsub('\n', '', hv_data_clean)) %>%
  extract(hv_data_clean, into = c('address', 'lat', 'lon'), 
          regex = '(.*)\\((.*),\\s+(.*)\\)', convert = TRUE) %>% 
  bind_cols(hv_most_pop) %>% 
  write.csv(.,file = "hvdc_latlon.csv")

# Good. Now both datasets have been exported and can be imported for spatial
# manipulation in a subsequent analysis. 










