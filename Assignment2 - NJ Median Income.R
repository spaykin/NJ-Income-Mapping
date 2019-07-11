library(rgdal)
library(rgeos)
library(tidyverse)
library(tmap)
library(leaflet)
library(sf)
library(RColorBrewer)

#set working directory
setwd("~/Desktop/UChicago Harris/Spring 2019/GIS & Spatial Analysis /Assignment 2")

#load tract data - 2015
census.data <- read.csv("NJ Tract ACS_15_5YR_DP03/ACS_15_5YR_DP03.csv")

#load county data - 2015
county.census.data <- read.csv("NJ County ACS_15_5YR_DP03/ACS_15_5YR_DP03.csv")

#load tract shapefile
output.area <- readOGR(".", "NJ_Tract")

#load county shapefile
county.output.area <- output.area <- readOGR(".", "NJ_County")

plot(output.area)

#merge tract data
NJ <- merge(output.area, census.data, by.x="GEOID", by.y="GEO.id2")

#merge county data
NJ_County <- merge(county.output.area, county.census.data, by.x="FIPSSTCO", by.y="GEO.id2")
head(NJ_County@data)

#simple chloropleth map by tract median household income
qtm(NJ, fill = "HC01_VC85")

#simple chloropleth map by county median household income 
qtm(NJ_County, fill = "HC01_VC85")

# quantilemap of tract median income
MedIncome2015_quant <-
  tm_shape(NJ) + 
  tm_fill("HC01_VC85", 
          pallette = "Greens",
          style = "quantile",
          title = "Tract Median Income") +
  tm_borders(alpha=.3) + 
  tm_compass() + 
  tm_layout(legend.text.size = 0.9, 
            legend.title.size = 1.0, 
            legend.outside = TRUE,
            legend.position = c("right", "top"), 
            frame = FALSE)

MedIncome2015_quant

# natural breaks map of tract median income
MedIncome2015_tracts <-
  tm_shape(NJ) + 
  tm_fill("HC01_VC85", 
          pallette = "Greens",
          style = "jenks",
          title = "Tract Median Income") +
  tm_borders(alpha=.3) + 
  tm_layout(legend.text.size = 0.9, 
            legend.title.size = 1.0, 
            legend.outside = TRUE,
            legend.position = c("right", "top"), 
            frame = FALSE)

MedIncome2015_tracts

# natural breaks map of county median income
MedIncome2015_county <- 
  tm_shape(NJ_County) + 
  tm_fill("HC01_VC85", 
          pallette = "Greens",
          style = "jenks",
          title = "County Median Income") +
  tm_borders(alpha=.3) + 
  tm_layout(legend.text.size = 0.9, 
            legend.title.size = 1.0, 
            legend.outside = TRUE,
            legend.position = c("right", "top"), 
            frame = FALSE)

MedIncome2015_county

# view interactive maps side-by-side
current.mode <- tmap_mode("view")
tmap_arrange(MedIncome2015_county, MedIncome2015_tracts)
