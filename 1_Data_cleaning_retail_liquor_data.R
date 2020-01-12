# Section 1: Data preparation and cleaning

##Install packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('Hmisc')
library(tidyverse)
library(lubridate)
library(Hmisc)

# Bring in the data

setwd("C:/Users/dlars/Dropbox/Data_Science/Projects/EDA_Liquor_Retail_Data")

retail_dataimport <- read_csv("./data/Iowa_Liquor_Sales.csv")

# Create a fresh copy of the data so it can be reverted back to easily

retail <- retail_dataimport

# Get a sense of the variables in the dataset and their structure.
glimpse(retail)

# Cleaning - Check for missing data. Identify which columns have missing values and how many

retail %>% 
  map(.,~sum(is.na(.)))

#We ignore the entire row(ie observation), if any column has a missing value
retail <- retail[complete.cases(retail), ]

# Check to see if we have any NA's

retail %>% 
  map(., ~sum(is.na(.)))

## Boom yes! We have considerably reduced misssing values.

## Data Cleaning

glimpse(retail)

# Need to change Date from "chr" to "mdy"
# Need to change Item_Description, County, Invoice Number, Category Name, to a Factor for analysis.
# Will also need to make some minor manipulations using "gsub" to remove underscores and slashes
# in variable names

#replace spaces in variable names with underscores. This will throw errors.
names(retail) <- gsub("/", "_", names(retail))
names(retail) <- gsub(" ", "_", names(retail))

# Also remove the slashes between dates, they make formatting difficult.
retail$Date <- gsub("/","",retail$Date)

names(retail) <- gsub("[()]", "", names(retail))
retail$Sale_Dollars <- as.numeric(gsub("\\$", "", retail$Sale_Dollars))
retail$State_Bottle_Cost <- as.numeric(gsub("\\$", "", retail$State_Bottle_Cost))
retail$State_Bottle_Retail <- as.numeric(gsub("\\$", "", retail$State_Bottle_Retail))

# The last step is to convert county names to uppercase. They were entered incorrectly with upper and lowercase.


retail <- retail %>% mutate_each(funs(toupper),County)

glimpse(retail)

retail_cleaned <- retail %>%
  mutate(Date = mdy(Date)) %>% #coerces InvoiceDate in a Date Time format
  mutate(Item_Description = factor(Item_Description, levels = unique(Item_Description))) %>% 
  #coerces Description as a factor with each item as individual level of a factor
  
  
  mutate(County = factor(County, levels = unique(County)))%>%
  mutate(Invoice_Item_Number = factor(Invoice_Item_Number, levels = unique(Invoice_Item_Number))) %>%
  mutate(Category_Name = factor(Category_Name, levels = unique(Category_Name)))

glimpse(retail_cleaned)

## Save as an Rdata file to make it easy to import for the EDA.

save(retail_cleaned, file = "retail_cleaned.RData")

