### ggplot multiple year mean and SD of rzsm 
## 1: ERA5
## 2: MERRA2
## 3: GLDAS 

library(raster)
library(ncdf4)
library(ggplot2)
library(grDevices)
library(RColorBrewer)
library(colorRamps)
library(ggpubr)
library(reshape2)
library(sysfonts)
library(showtext)
library(grid)
library(dplyr)
library(ggpubr)
library(extrafont)
library(rgdal)

### MODIS Landcover-----------------------------------------------------------
setwd("E:/New PHD/Study data/MODIS_Landcover/Halfgegree/")

lc.list <- list.files(pattern = ".tif")
lc.list

lc.tif <- stack(lapply(lc.list, brick))
lc.tif

### 15 Snow and ice <- NA
lc.tif[lc.tif[[1:37]][] == 15 | lc.tif[[1:37]][] == 0] <- NA
# 1	Evergreen Needleleaf forest  ###ENF 
# 2	Evergreen Broadleaf forest  # EBF
# 3	Deciduous Needleleaf forest  ##DNF
# 4	Deciduous Broadleaf forest  # DBF
# 5	Mixed forest    # BNMF
# 6	Closed shrublands ## SL
# 7	Open shrublands   ## SL
# 8	Woody savannas    ## GL 
# 9	Savannas         ## GL  cbind(8,9)
# 10	Grasslands     ## GL
# 11	Permanent wetlands   ## WL
# 12	Croplands   ## CL
# 13	Urban and built-up   ## CL
# 14	Cropland/Natural vegetation mosaic   ## CL
# 15	Snow and ice
# 16	Barren or sparsely vegetated
# 0	Unclassified
# 17 water bodies have been masked




## yearly rzsm----------------------------------------------------------------------------------------------
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")
ERA5 <- brick("ERA5_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
MERRA2 <- brick("MERRA2_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
GLDAS <- brick("GLDAS_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3


## 多年平均
mean.ERA5 <- calc(ERA5, function(x)mean(x))
mean.MERRA2 <- calc(MERRA2, function(x)mean(x))
mean.GLDAS <- calc(GLDAS, function(x)mean(x))
spplot(mean.ERA5)

#w <- area(mean.ERA5, na.rm=T) / area(mean.ERA5, na.rm=T) %>% cellStats("sum")
#cellStats(mean.GLDAS*w, "sum")

## sd
sd.ERA5 <- calc(ERA5, function(x)sd(x))
sd.MERRA2 <- calc(MERRA2, function(x)sd(x))
sd.GLDAS <- calc(GLDAS, function(x)sd(x))
spplot(sd.GLDAS)



### ggplot multiple mean and sd-----------------------------------------------------

#font_import()
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()


mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        # legend.position = "bottom",
        legend.position = c(0.5,-0.35),#"bottom",
        legend.direction = "horizontal",
        legend.key.width=unit(1,'cm'),legend.key.size=unit(0.25,'cm'), # 图例长度与高度
        legend.title = element_text(size=11, family="Times New Roman"),  # 图例名称字体大小
        legend.text = element_text(size = 11, family="Times New Roman"),
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.key = element_blank(), ## 图例背景去掉
        axis.text = element_text(size = 12, family="Times New Roman"),
        axis.title = element_text(size = 11, family="Times New Roman"),
        #plot.title = element_text(hjust = 0.5, vjust = 0, size = 10,  family="Times New Roman"),
        plot.margin = unit(c(0.5,1,0.5,0.5),"lines") # (top, right, bottom, left)
  )


my_scale_x_continuous <-  scale_x_continuous(name= NULL, # 设置X轴的刻度(经度)
                                             expand = c(0,0),
                                             breaks = c(-180,-120,-60,0,60,120,180), #经度刻度间隔
                                             labels = c(expression(paste('180',degree,W)),expression(paste('120',degree,W)), #经度刻度的标签
                                                        expression(paste('60',degree,W)),expression(paste('0',degree)),
                                                        expression(paste('60',degree,E)),expression(paste('120',degree,E)),
                                                        expression(paste('180',degree,E))))
my_scale_y_continuous <- scale_y_continuous(expand = c(0,0),
                                            # limits = c(-60,90),
                                            breaks = c(-60,-30,0,30,60),   #纬度刻度间隔
                                            labels = c(expression(paste('60',degree,S)),expression(paste('30',degree,S)), #纬度刻度的标签
                                                       expression(paste('0',degree)),expression(paste('30',degree,N)),
                                                       expression(paste('60',degree,N))))


mean_scale_fill_gradientn <- scale_fill_gradientn(colors= paletteer_c("grDevices::rainbow", 30),
                                                  name = expression(paste(m^3," ",m^-3)),  # 图例名称
                                                  na.value = "white", 
                                                  breaks= c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7),
                                                  labels=c('0','0.1','0.2','0.3','0.4','0.5','0.6','0.7'),  # 图例标签显示  # 图例标签显示
                                                  limits=c(0,0.7))


sd_scale_fill_gradientn <- scale_fill_gradientn(colors=brewer.pal(n = 9, name = "YlGn"), #brewer.pal(n=10,name = "RdYlGn"),#RdYlGn, #brewer.pal(n = 10, name = "BrBG"), # 图例颜色 brewer.pal(n = 11, name = "RdYlGn")
                                                name = expression(paste(m^3," ",m^-3)),  # 图例名称
                                                na.value = "white", 
                                                breaks= c(0,0.02,0.04,0.06, 0.08),
                                                labels=c('0','0.02','0.04','0.06', '0.08'),  # 图例标签显示  # 图例标签显示
                                                limits=c(0,0.08))


my_guides <-  guides(fill = guide_colourbar(title.position = "right",  # 图例名称位置
                                            title.hjust = 0,   # 图例名称水平方向位置
                                            title.vjust = 1.2, # 图例名称垂直方向位置
                                            ticks.colour = "black",  # #ticks线的颜色
                                            ticks.linewidth = 0.5 ,  # ticks线的宽度
                                            frame.colour = "black" ,   # 图例边框颜色;  frame.linewidth = 0.5 图例边框宽度
                                            reverse = F ))
shpfile <- readOGR("E:/New PHD/Study data/Shp/Continent_boundary/Continent_boundary_polygon.shp") %>% fortify()
#plot(shp)#geom_sf()




## ggplot ERA5
mean.ERA5.df <- as.data.frame(mean.ERA5, xy=T)
summary(mean.ERA5.df)  #0: 0.7

sd.ERA5.df <- as.data.frame(sd.ERA5, xy=T)
summary(sd.ERA5.df)  #0: 0.05

##
gg.mean.ERA5 <- ggplot() +
  geom_raster(mean.ERA5.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(y ="ERA5", title = NULL)

gg.mean.ERA5.sum <- gg.mean.ERA5 + mytheme + my_scale_x_continuous + my_scale_y_continuous + mean_scale_fill_gradientn + my_guides
gg.mean.ERA5.sum              



##
gg.sd.ERA5 <- ggplot() +
  geom_raster(sd.ERA5.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))  +
  labs(title = NULL, y="")


gg.sd.ERA5.sum <- gg.sd.ERA5 + mytheme + my_scale_x_continuous + my_scale_y_continuous + sd_scale_fill_gradientn + my_guides
gg.sd.ERA5.sum 






### MERRA2
mean.MERRA2.df <- as.data.frame(mean.MERRA2, xy=T)
summary(mean.MERRA2.df)  #0.03: 0.46

sd.MERRA2.df <- as.data.frame(sd.MERRA2, xy=T)
summary(sd.MERRA2.df)  #0: 0.08


##
gg.mean.MERRA2 <- ggplot() +
  geom_raster(mean.MERRA2.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(y = "MERRA-2")

gg.mean.MERRA2.sum <- gg.mean.MERRA2 + mytheme + my_scale_x_continuous + my_scale_y_continuous + mean_scale_fill_gradientn + my_guides
gg.mean.MERRA2.sum              



##
gg.sd.MERRA2 <- ggplot() +
  geom_raster(sd.MERRA2.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))  +
  labs(y="")


gg.sd.MERRA2.sum <- gg.sd.MERRA2 + mytheme + my_scale_x_continuous + my_scale_y_continuous + sd_scale_fill_gradientn + my_guides
gg.sd.MERRA2.sum 







### GLDAS
mean.GLDAS.df <- as.data.frame(mean.GLDAS, xy=T)
summary(mean.GLDAS.df)  #0.01: 0.46

sd.GLDAS.df <- as.data.frame(sd.GLDAS, xy=T)
summary(sd.GLDAS.df)  #0: 0.05



##
gg.mean.GLDAS <- ggplot() +
  geom_raster(mean.GLDAS.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(y ="GLDAS", title = NULL)

gg.mean.GLDAS.sum <- gg.mean.GLDAS + mytheme + my_scale_x_continuous + my_scale_y_continuous + mean_scale_fill_gradientn + my_guides
gg.mean.GLDAS.sum              



##
gg.sd.GLDAS <- ggplot() +
  geom_raster(sd.GLDAS.df, mapping = aes(x=x, y=y, fill = layer), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))  +
  labs( y="", title = NULL)


gg.sd.GLDAS.sum <- gg.sd.GLDAS + mytheme + my_scale_x_continuous + my_scale_y_continuous + sd_scale_fill_gradientn + my_guides
gg.sd.GLDAS.sum 





###########################
gg.mean.sum <- ggarrange(gg.mean.ERA5.sum, gg.mean.GLDAS.sum, gg.mean.MERRA2.sum,
                         ncol=1, nrow = 3,
                         labels = c("(a)", "(c)", "(e)"),
                         font.label = list(size = 11, color = "black", face = "plain", family = "Times New Roman"),
                         hjust = -4.7, vjust = 2.5,
                         common.legend = T, legend = "bottom")
gg.mean.sum


gg.sd.sum <- ggarrange(gg.sd.ERA5.sum, gg.sd.GLDAS.sum, gg.sd.MERRA2.sum,
                       ncol=1, nrow = 3,
                       labels = c("(b)", "(d)", " (f)"),
                       font.label = list(size = 11, color = "black", face = "plain", family = "Times New Roman"),
                       hjust = -4.6, vjust = 2.5, align = "hv",
                       common.legend = T, legend = "bottom")
gg.sd.sum



## 
gg.sum <- ggarrange(gg.mean.sum, gg.sd.sum,
                    ncol=2, nrow = 1)

gg.sum

ggsave('D:/LuoXR/SCI_02/submit/Journal of Hydrology/一修/output/ERA5_MERRA2_GLDAS_RZSM_mean_SD.pdf',
       dpi = 10000,
       width = 8, height = 6)



