# load libraries for creating maps in R

library(ggplot2)
library(ggmap)
library(readr)  # this is just a library for better reading/writing csvs
library(RCurl)  # this is just if you are using the sample dataset

# set working directory
setwd("C:/Users/pawlusm/Desktop")

## read in csv (if you have it downloaded)
mapdata <- read_csv("map_data.csv")

## or read in the sample data from GitHub
x <- getURL("https://raw.githubusercontent.com/michaelpawlus/fundraising_analytics/master/mapping/map_data.csv")
mapdata <- read_csv(text = x)

# change binary attribute from numeric to factor variable
mapdata$mg_donor <- as.factor(mapdata$mg_donor)   

# change numeric to factor variable (and reorder so it is still in numeric order)
mapdata$capacity <- as.factor(as.character(mapdata$capacity))
mapdata <- within(mapdata, capacity <- reorder(capacity, as.numeric(as.character(capacity))))

# create a calculated field summing the total number of fiscal years where the constituient has made a gift out of the last 5
# a score of 6 is possible here because the current fiscal year is also included
mapdata$rf <- rowSums(mapdata[,6:11])

# get a map of using the Google API 
# (Change location to wherever you want the map to be and adjust zoom from 10 - 14 to get the size you want)
# (check this link for more easy options: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf)
egr.map.code <- get_map(location = "East Grand Rapids, MI", source = 'google', maptype = 'roadmap', zoom = 13)

# obtain long/lat for all addresses in the area and then add the new columns to mapdata
mapdata.ll <- geocode(mapdata$addr, source = "google")
mapdata <- cbind(mapdata, mapdata.ll)

# plot points on a map
# you have three visual elements that you can set to variables: shape, size and color
ggmap(egr.map.code) + 
  geom_point(alpha = 0.5, aes(lon,lat, shape=mg_donor, color=capacity, size=rf), data = mapdata) + 
  scale_size(range=c(4,7))

# If you see any area of interest, you can draw a shape around it to highlight
# In this example, I see one major donor and two constituents with higher capacity
# I wonder if my major donor knows these others and can help us cultivate a relationship
ggmap(egr.map.code) + 
  geom_point(alpha = 0.5, aes(lon,lat, shape=mg_donor, color=capacity, size=rf), data = mapdata) + 
  scale_size(range=c(4,7)) + 
  geom_polygon(aes(x = c(-85.62,-85.61,-85.61,-85.62), y = c(42.955,42.955,42.942,42.942)), color = "black", alpha = 0.05)

# Assuming this is a group you want to work with, you can subset these records
prospects <- mapdata[abs(mapdata$lon) < 85.62 & abs(mapdata$lon) > 85.61 & mapdata$lat > 42.942 & mapdata$lat < 42.955,]

# Then, output these prospects to a csv
write_csv(prospects,"prospects.csv")

# The function below checks geocode query balance 
# (you can do 2500 of these every 24 hours -- this shows how many are remaining)
geocodeQueryCheck()
