library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)
library(Rcpp)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(readr)
library(tidyverse)
library(janitor)
library(OpenStreetMap)

#读数据
HDI <- read_csv(here::here("Data", "Gender Inequality Index (GII).csv"),
                locale = locale(encoding = "latin1"),
                na = "..", skip=0)
World <- st_read(here::here("Data", "World_Countries__Generalized_.shp"))

#提取需要的列、转换国家名为国家代码
HDIcols<- HDI %>%
  clean_names()%>%
  select(country, x2019, x2010)%>%
  mutate(difference=x2019-x2010)%>%
  #country是被转换的国家名，origin是被转换数据的类型(在这里是国家名)，destination是想转换成的类型
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))

#将文本数据加入到边界数据中
HDI_join <- World %>%
  clean_names() %>%
  left_join(., HDIcols, by = c('aff_iso' = 'iso_code'))

HDI_join_tidy <- HDI_join %>%
  filter(difference != 'NA') %>%
  filter(country.x != 'Tonga')









