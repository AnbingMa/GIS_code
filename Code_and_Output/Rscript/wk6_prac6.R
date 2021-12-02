library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)
library(fpc)
library(dbscan)
library(ggplot2)
library(OpenStreetMap)

#Read London borough data
LondonBoroughs <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson")

#Select E09(London Boroughs) 
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(lad15cd,"^E09")) %>% 
  st_transform(.,27700)
qtm(BoroughMap)
summary(BoroughMap)

#Read the Blue Plaques
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")
BluePlaques <- BluePlaques %>%
  st_transform(.,27700)
summary(BluePlaques)

#Plot the blue plaques in the city
tmap_mode("plot") #创建一个静态图
tm_shape(BoroughMap) + #添加形状
  tm_polygons(col = NA, alpha = 0.5) + #设置多边形的属性,无颜色，透明度为0.5
tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#删除重复内容
BluePlaques <- distinct(BluePlaques)

#用伦敦地图范围去裁切点
BluePlaquesSub <- BluePlaques[BoroughMap, ]
#检查裁切是否完成
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#提取出研究区域
Harrow <- BoroughMap %>%
  filter(., lad15nm=="Harrow")
#检查研究区域是否被提取
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)
#用Harrow区来裁切POI
BluePlaquesHarrowSub <- BluePlaques[Harrow,]
BluePlaquesHarrowSub <- distinct(BluePlaquesHarrowSub)
#检查裁切是否完成
tmap_mode("plot")
tm_shape(Harrow) + 
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(BluePlaquesHarrowSub) +
  tm_dots(col = "blue")

#创建观察窗口
window <- as.owin(Harrow)
plot(window)

#将之前裁切的点数据转换成适用于spatstat的对象，创建sp对象
BluePlaquesHarrowSub <- BluePlaquesHarrowSub %>%
  as(., "Spatial")
#创建点模式（ppp）对象
BluePlaquesHarrowSub.ppp <- ppp(x = BluePlaquesHarrowSub@coords[,1],
                                y = BluePlaquesHarrowSub@coords[,2],
                                window = window) #给x,y坐标赋值
#画出ppp点对象
BluePlaquesHarrowSub.ppp %>%
  #pch代表点的样式，cex代表点的大小
  plot(., pch=16, cex=0.5, main = "Blue Plaques Harrow")

#核密度估计
BluePlaquesHarrowSub.ppp %>%
  #sigma值表示内核的直径
  density(., sigma = 500) %>%
  plot
BluePlaquesHarrowSub.ppp %>%
  #sigma值表示内核的直径
  density(., sigma = 1000) %>%
  plot

#二次方分析
BluePlaquesHarrowSub.ppp %>%
  plot(., pch = 16, cex = 0.5, main = "Blue Plaques Harrow")
#计算每个6*6正方形内的点的数量
BluePlaquesHarrowSub.ppp %>%
  quadratcount(., nx = 6, ny = 6) %>%
  plot(., add = T, col = "red")
#计算每个6*6正方形内点的数量并将结果保存到表格
Qcount <- BluePlaquesHarrowSub.ppp %>%
  quadratcount(., nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1 = Freq) %>% #对点出现的次数计数
  dplyr::rename(Freqquadratcount = n) #重命名列名

#检查数据格式是否为数值
Qcount %>%
  summarise_all(class)

#基于泊松分布计算期望概率
#1 计算blue plaques的总数
sums <- Qcount %>%
  #生成一个新列，用来存放每个区域内的点的总数
  mutate(total = Var1 * Freqquadratcount) %>%
  #across代表对多列进行同样的操作
  dplyr::summarise(across(everything(), sum)) %>%
  #对Var1进行反选，相当于剔除Var1这一列
  dplyr::select(-Var1)

#2 计算lambda（泊松分布公式中一个值）
lambda <- Qcount %>%
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda = total / Freqquadratcount) %>%
  dplyr::select(lambda) %>%
  pull(lambda)

#3 计算期望
QCountTable <- Qcount %>%
  #根据公式计算期望概率
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #计算每个正方形区域内POI个数的期望值
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#4 画图，对比实际值和期望值
#画一张空图
plot(c(1,7), c(0,13), type = "n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)",
     ylab="Frequency of Occurances")
#画观测值
points(QCountTable$Freqquadratcount,
       col = "Red", type = "o", lwd = 3)
#画期望值
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

#5 进行卡方验证，如果p<0.05，则拒绝Null假设，那么点的分布具有空间模式；
#  如果p>0.05，则接受Null假设，那么点的分布是随机的
ChiTest <- quadrat.test(BluePlaquesHarrowSub.ppp, nx = 6, ny = 6)
plot(BluePlaquesHarrowSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(ChiTest, add=T, col = "red")

#6 进行Ripley's K检验
K <- BluePlaquesHarrowSub.ppp %>%
  Kest(., correction="border") %>%
  plot()


#对POI进行DBSCAN分析，查看POI的分布是否存在集群
#检查底图的坐标参考系
st_geometry(BoroughMap)

#DBSCAN要输入两个参数：
#1 Epsilon：算法搜索集群的半径
#2 MinPts：应该被视为集群的最小点数

#根据之前的K检验的结果，聚类半径约为1200m，最大凸起在700m左右

#1 提取出POI坐标并存入表格
BluePlaquesSubPoints <- BluePlaquesHarrowSub %>%
  coordinates(.)%>%
  as.data.frame()

#2 进行聚类分析
DB <- BluePlaquesSubPoints %>%
  fpc::dbscan(., eps = 700, MinPts = 4)

#3 画图,frame = F代表没有边框
plot(DB, BluePlaquesSubPoints,main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add = T)

#4 寻找最佳的EPS距离
BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

#5 将集群信息插入POI坐标信息表格中
BluePlaquesSubPoints <- BluePlaquesSubPoints %>%
  mutate(DBcluster = DB$cluster)

#6 创建凸包来环绕集群中的点
Chulls <- BluePlaquesSubPoints %>%
  group_by(DBcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#7 删除cluster为0的值，因为它们不属于集群
Chulls <- Chulls %>%
  filter(DBcluster >= 1)

#8 画图
#创建一个画图对象
DBplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=DBcluster, fill=DBcluster)) 

#画点
DBplot <- DBplot + geom_point()

#画凸包
DBplot <- DBplot + geom_polygon(data = Chulls, 
                                aes(coords.x1,coords.x2, group=DBcluster), 
                                alpha = 0.5) 
#出图
DBplot + theme_bw() + coord_equal()

#9 添加底图
#为Harrow获取一个bbox底图
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326) %>%
  st_bbox()

#将底图转换为英国国家格网
basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

#出图
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=DBcluster, 
                 fill=DBcluster)) + 
  geom_polygon(data = Chulls, 
               aes(coords.x1,coords.x2, 
                   group=DBcluster,
                   fill=DBcluster), 
               alpha = 0.5) 


#分析空间自相关

library(here)
library(janitor)
library(dplyr)
library(spdep)

#1 读数据
LondonWards <- st_read(here::here("data", "LondonWards", "London_Ward.shp"))
LondonWardsMerged <- st_read(here::here("data", 
                                        "statistical-gis-boundaries-london",
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

WardData <- read_csv("data/ward-profiles-excel-version.csv", na = c("NA", "n/a")) %>% 
  clean_names()

#2 左连接数据
LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

#3检查投影
st_crs(LondonWardsMerged)

#4 画图
tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

#5 保留London范围内的点，删除范围外的
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#6 计算每个ward中蓝色点的数量和密度
points_sf_joined <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  add_count(ward_name)%>%
  janitor::clean_names()%>%
  #计算区域面积
  mutate(area = st_area(.)) %>%
  #计算每个ward的蓝色的密度
  mutate(density = n / area) %>%
  #选取参数
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

#7 绘制密度等值线图
points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))
tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

#8 定义空间权重矩阵
  #计算伦敦所有ward的质心
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW,axes=TRUE)

  #创建neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)
  #绘制
plot(LWard_nb, st_geometry(coordsW), col="red")
plot(points_sf_joined$geometry, add=T)

  #创建空间权重矩阵
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")
sum(Lward.lw)

  #创建空间权重列表
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

#9 计算莫兰指数I,越接近于1越聚集，越接近-1越离散
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_Density

#10 计算Geary'C，判断聚集的值的相似性
C_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_Density

#11 计算Getis Ord General G,
#If G > Expected = High values clustering; 
#If G < expected = low values clustering
G_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_Density

#12 计算每个ward的莫兰指数I和
I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#13 将第1列和第4列数据提取出来添加到最原始的Londonwards底图中
points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

#14 根据莫兰指数的意义手动设置断点
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

#15 反装莫兰指数的值，值越大颜色越红
MoranColours<- rev(brewer.pal(8, "RdGy"))

#16 绘图
tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

#17 计算每个ward的Getis Ord G
Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

#18 提取关键数据Gi_LWard_Local_Density
points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

#19  绘图
  #反转
GIColours<- rev(brewer.pal(8, "RdBu"))
  #绘图
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")

#20 探究其他数据
  #删除几何，打印每列的类
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

#21 探究GCSE的莫兰指数，没有在分析蓝色点了，分析高分和低分的空间聚集
I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))

#绘图
tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")

#22 the Gi* statistic to look at clusters of high and low scores
  #步骤同上
G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))
  #绘图
tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")

