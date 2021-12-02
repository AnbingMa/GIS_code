#install.packages("here")
library(sf)
library(here)
library(raster)
library(ggplot2)

#读取gpkg文件
st_layers(here::here("prac3_data","gadm36_AUS.gpkg"))

#读取gpkg中的图层
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), layer = 'gadm36_AUS_0')
#检查图层的参考系
print(Ausoutline)
st_crs(Ausoutline)$proj4string

# 给图层设置地理参考系
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(., 3112)
print(AusoutlinePROJECTED)

#加载数据
#install.packages("fs")
library(fs)
#打开文件夹
dir_info("prac3_data/wc2.1_5m_tavg/")

library(tidyverse)
#读取文件夹里所有的tif文件
listfiles <- dir_info("prac3_data/wc2.1_5m_tavg/") %>% 
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path) %>%
  pull()

#将数据读取到栅格堆栈中
worldclim_temp <- listfiles %>%
  stack()
#读取堆栈中的单个层
worldclim_temp[[1]]
#重命名堆栈中的层
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(worldclim_temp) <- month

#提取选定城市的栅格数据
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#将上述城市放到一个列表里
samples <- data.frame(site, lon, lat, row.names="site")
#提取上述城市的栅格数据 
AU_city_temp <- raster::extract(worldclim_temp, samples)
#加入城市名
AU_city_temp_named <- AU_city_temp %>%
  as_tibble() %>%
  add_column(Site = site, .before = "Jan")

#开始进行描述性统计
#提取Perth市的数据
Perth_temp <- AU_city_temp_named %>%
  filter(site == "Perth")
#画直方图
hist(as.numeric(Perth_temp))
userbreak <- c(8,10,12,14,16,18,20,22,24,26,28,30,32,34)
hist(as.numeric(Perth_temp),
     breaks=userbreak,
     col="yellow",
     main="Histogram of Perth Temp",
     xlab="Temp",
     ylab="Frequency")
#查看直方图的具体信息
hist_info <- Perth_temp %>%
  as.numeric() %>%
  hist(.)

#将温度分布用在地图上表现出来
#加载边界数据并将其简化
#install.packages("rmapshaper")
library(rmapshaper)
Ausoutline_simple <- Ausoutline %>%
  ms_simplify(., keep=0.05)
plot(Ausoutline_simple$geom)
#将栅格数据放到边界数据中
Aus_temp <- Ausoutline_simple %>%
  crop(worldclim_temp,.)
plot(Aus_temp, axes=FALSE)
#将栅格数据从边界数据中提取出来
exact_Aus <- Aus_temp %>%
  mask(., Ausoutline_simple, na.rm=TRUE)
hist(raster::subset(exact_Aus, "Mar"), col="yellow", main="March Temp")

#用ggplot画直方图
#将栅格数据转化为dataframe
exactAusdf <- exact_Aus %>%
  as.data.frame()
exactAusdf
#画直方图
gghist <- ggplot(exactAusdf,aes(x=Mar))+
          geom_histogram(color="black",fill="white")+
          labs(title="Ggplot2 histogram of Australian March temperatures",
               x="Temperature",
               y="Frequency")
#添加一条竖线表示平均气温
gghist + geom_vline(aes(xintercept=mean(Mar, na.rm=TRUE)),
                        color="red",
                        linetype="dashed",
                        size=1) +
  theme(plot.title = element_text(hjust = 0.5))
#将12个月的气温数据放到一个数据集
squishdata <- exactAusdf %>%
  pivot_longer(cols = 1:12,
               names_to = "Month",
               values_to = "Temp")
#提取出两个月的数据
twomonths <- squishdata %>%
  filter(., Month == "Jan" | Month == "Jun")
#计算两个月各自的平均气温
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm = TRUE))
#开始画图并设置直方图的各项参数
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  #设置堆积模式为”identity(重叠)“
  geom_histogram(position="identity", alpha=0.5)+
  #添加一条竖线表示平均气温
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  #添加标题
  labs(title="Ggplot2 histogram of Australian Feb and Aug
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#整理数据,重新画图
data_complete_cases <- squishdata %>%
  drop_na() %>% #剔除NA数据
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec"))) #将月份按照1月-12月排序
#绘制多面直方图
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  #按照月份将图表数据分面，并且纵向排列；facet_grid(. ~Month)也是分面，不过是横向排列
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

#交互式直方图
library(plotly)
#拆分数据
jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")
#给x、y轴命名
x <- list(title = "Temperature")
y <- list(title = "Frequency")
#设置直方图条带的宽度
xbinsno <- list(start=0,end=40,size=2.5)
#画图
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)
ihist


