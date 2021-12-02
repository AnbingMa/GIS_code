#install.packages("Rcpp")
install.packages("raster")
library(Rcpp)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(raster)
library(sf)
library(rgdal)
library(geojsonio)
library(readr)
library(tidyverse)
library(janitor)

#Download map from internet
#EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")
LondonData <- read_csv(here::here("LondonData.csv"), locale = locale(encoding = "latin1"),na = "n/a")
EW <- st_read(here::here('data','Local_Authority_Districts_(December_2020)_UK_BGC','Local_Authority_Districts_(December_2020)_UK_BGC.shp'))
head(EW)
#Plot map
London_Map <- EW %>%
  filter(str_detect(LAD20CD, "^E09"))
qtm(London_Map)

#Add some attribute data
LondonData <- clean_names(LondonData)
Borough_Data_Map <- EW %>%
  clean_names() %>%
  filter(str_detect(lad20cd,"^E09")) %>%
  merge(.,
        LondonData,
        by.x="lad20cd", 
        by.y="new_code",
        no.dups = TRUE) %>%
  distinct(.,lad20cd, 
           .keep_all = TRUE)
qtm(Borough_Data_Map)

#Create a choropleth map
library(tmap)
library(tmaptools)

tmap_mode("view")
qtm(Borough_Data_Map, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

#Add a basemap
library(Rcpp)
library(downloader)
library(OpenStreetMap)
tmaplondon <- Borough_Data_Map %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")
tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Borough_Data_Map) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

library(shiny)
library(shinyjs)
palette_explorer()

Life_expectancy <- LondonData  %>%
  clean_names() %>%
  filter(str_detect(new_code,"^E09")) %>%
  distinct() %>%
  dplyr::rename(borough=`ward_name`)%>%
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))
  
  
Life_expectancy4 <- Life_expectancy %>%
  mutate(UK_diff = averagelifeexpectancy-81.16) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  mutate(across(UK_diff, round, 0))

Life_expectancy4map <- EW %>%
  clean_names() %>%
  merge(.,
        Life_expectancy4, 
        by.x="lad20cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad20cd, 
           .keep_all = TRUE)

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UK_diff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))
