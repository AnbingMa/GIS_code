#这是一个将练习规范化之后的R语言脚本，可以在未来重复使用

#加载需要的package
library(rgdal)
library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

#读取伦敦边界文件
London_boundary <- st_read("/Users/maanbing/Documents/UCL/CASA0005/practical/week1/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
#读取伦敦flytipping文件
London_csv <- read_csv("/Users/maanbing/Documents/UCL/CASA0005/practical/week1/fly_tipping_borough_new.csv")
#将csv文件join到边界文件中
London_boundary_join <- London_boundary%>%
  merge(.,
        London_csv,
        by.x="GSS_CODE", 
        by.y="code")
#把专题地图显示在plot中
tmap_mode("plot")
London_boundary_join %>%
  qtm(.,fill = "2011-12")
#将做好的专题地图写入到GeoPackage中
London_boundary_join %>%
  st_write(.,"/Users/maanbing/Documents/UCL/CASA0005/practical/week1/R_wk1_gepkg.gpkg",
           "R_london_boroughs_fly_tipping_Script",
           delete_layer=TRUE)
#连接GeoPackage
con <- dbConnect(RSQLite::SQLite(),dbname="/Users/maanbing/Documents/UCL/CASA0005/practical/week1/R_wk1_gepkg.gpkg")
#查看GeoPackage里的文件
con %>%
  dbListTables()
#加入我的csv
con %>%
  dbWriteTable(.,
               "original_csv_Script",
               London_csv,
               overwrite=TRUE)
#断开连接
con %>% 
  dbDisconnect()

