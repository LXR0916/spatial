## CMP RZSM between ERA5, GLDAS, MERRA-2
## 1: ERA5 and GLDAS
## 2: ERA5 and MERRA2
## 3: GLDAS and MERRA2
## 4: ggplot
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
library(spatialEco)
library(pals)
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


####  change the oder of #13 and #14
lc.tif[lc.tif[]== 13] <- 18
lc.tif[lc.tif[]== 14] <- 13
lc.tif[lc.tif[]== 18] <- 14

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
# 13	Cropland/Natural vegetation mosaic   ## CL
# 14	Urban and built-up   ## CL
# 15	Snow and ice
# 16	Barren or sparsely vegetated
# 0	Unclassified
# 17 water bodies have been masked



## yearly rzsm----------------------------------------------------------------------------------------------
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")
ERA5 <- brick("ERA5_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
MERRA2 <- brick("MERRA2_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
GLDAS <- brick("GLDAS_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3


## mutiple-year average
mean.ERA5 <- calc(ERA5, function(x)mean(x))
mean.MERRA2 <- calc(MERRA2, function(x)mean(x))
mean.GLDAS <- calc(GLDAS, function(x)mean(x))
spplot(mean.GLDAS)



## 1: CMP between ERA5 and GLDAS----------------------------------------------------------------
# WINDOW size
LIST.SCALE = seq(3,41,2)

### calculate the absolute distance 

cal.Abs.Distance <- function(mean.ERA5, mean.GLDAS){
  m.1 <- as.matrix(mean.ERA5)
  m.2 <- as.matrix(mean.GLDAS)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        mean.x = mean(m.1[i:(i + size), (j: (j + size))], na.rm = T)
        mean.y = mean(m.2[i:(i + size), (j: (j + size))], na.rm = T)
        distance = abs(mean.x - mean.y)
        matrix.t[i:(i + size), (j: (j + size))] = distance
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.ERA5)
    crs(r) <- crs(mean.ERA5)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

### calculate the cross-correlation 

cal.Cross.Correlation <- function(mean.ERA5, mean.GLDAS){
  m.1 <- as.matrix(mean.ERA5)
  m.2 <- as.matrix(mean.GLDAS)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        
        m.t1 = m.1[i:(i + size), (j: (j + size))]
        m.t2 = m.2[i:(i + size), (j: (j + size))]
        
        t.1 = (m.t1 - mean(m.t1, na.rm = T))
        std.1 <- 1 / (size ^ 4 - 1) * sum(t.1 ^ 2, na.rm = T)
        std.1 <- sqrt(std.1)
        
        t.2 = (m.t2 - mean(m.t2, na.rm = T))
        std.2 <- 1 / (size ^ 4 - 1) * sum(t.2 ^ 2, na.rm = T)
        std.2 <- sqrt(std.2)
        
        cc = 1 / (size ^ 4) / (std.1 * std.2) * sum(t.1 * t.2, na.rm = T)
        
        
        matrix.t[i:(i + size), (j: (j + size))] = cc
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.ERA5)
    crs(r) <- crs(mean.ERA5)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

D.era5.gldas <- cal.Abs.Distance(mean.ERA5, mean.GLDAS)
CC.era5.gldas <- cal.Cross.Correlation(mean.ERA5, mean.GLDAS)


spplot(D.era5.gldas)
spplot(CC.era5.gldas)

writeRaster(D.era5.gldas, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_D_RZSM_ERA5_GLDAS.tif", overwrite = T)
writeRaster(CC.era5.gldas, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_CC_RZSM_ERA5_GLDAS.tif", overwrite = T)








# 2: CMP between ERA5 and MERRA2----------------------------------------------------------------
# WINDOW size
LIST.SCALE = seq(3,41,2)

### calculate the absolute distance 

cal.Abs.Distance <- function(mean.ERA5, mean.MERRA2){
  m.1 <- as.matrix(mean.ERA5)
  m.2 <- as.matrix(mean.MERRA2)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        mean.x = mean(m.1[i:(i + size), (j: (j + size))], na.rm = T)
        mean.y = mean(m.2[i:(i + size), (j: (j + size))], na.rm = T)
        distance = abs(mean.x - mean.y)
        matrix.t[i:(i + size), (j: (j + size))] = distance
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.ERA5)
    crs(r) <- crs(mean.ERA5)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

### calculate the cross-correlation 

cal.Cross.Correlation <- function(mean.ERA5, mean.MERRA2){
  m.1 <- as.matrix(mean.ERA5)
  m.2 <- as.matrix(mean.MERRA2)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        
        m.t1 = m.1[i:(i + size), (j: (j + size))]
        m.t2 = m.2[i:(i + size), (j: (j + size))]
        
        t.1 = (m.t1 - mean(m.t1, na.rm = T))
        std.1 <- 1 / (size ^ 4 - 1) * sum(t.1 ^ 2, na.rm = T)
        std.1 <- sqrt(std.1)
        
        t.2 = (m.t2 - mean(m.t2, na.rm = T))
        std.2 <- 1 / (size ^ 4 - 1) * sum(t.2 ^ 2, na.rm = T)
        std.2 <- sqrt(std.2)
        
        cc = 1 / (size ^ 4) / (std.1 * std.2) * sum(t.1 * t.2, na.rm = T)
        
        
        matrix.t[i:(i + size), (j: (j + size))] = cc
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.ERA5)
    crs(r) <- crs(mean.ERA5)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

D.era5.MERRA2 <- cal.Abs.Distance(mean.ERA5, mean.MERRA2)
CC.era5.MERRA2 <- cal.Cross.Correlation(mean.ERA5, mean.MERRA2)


spplot(D.era5.MERRA2)
spplot(CC.era5.MERRA2)

writeRaster(D.era5.MERRA2, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_D_RZSM_ERA5_MERRA2.tif", overwrite = T)
writeRaster(CC.era5.MERRA2, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_CC_RZSM_ERA5_MERRA2.tif", overwrite = T)







# 3: CMP between GLDAS and MERRA2----------------------------------------------------------------
# WINDOW size
LIST.SCALE = seq(3,41,2)

### calculate the absolute distance 

cal.Abs.Distance <- function(mean.GLDAS, mean.MERRA2){
  m.1 <- as.matrix(mean.GLDAS)
  m.2 <- as.matrix(mean.MERRA2)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        mean.x = mean(m.1[i:(i + size), (j: (j + size))], na.rm = T)
        mean.y = mean(m.2[i:(i + size), (j: (j + size))], na.rm = T)
        distance = abs(mean.x - mean.y)
        matrix.t[i:(i + size), (j: (j + size))] = distance
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.GLDAS)
    crs(r) <- crs(mean.GLDAS)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

### calculate the cross-correlation 

cal.Cross.Correlation <- function(mean.GLDAS, mean.MERRA2){
  m.1 <- as.matrix(mean.GLDAS)
  m.2 <- as.matrix(mean.MERRA2)
  
  r.stack <- stack()
  for (i.scale in 1:length(LIST.SCALE)){
    size = LIST.SCALE[i.scale]
    print(paste0("Times: ", i.scale))
    print(paste0("window size: ", size, " * ", size))
    
    matrix.t = matrix(nrow = nrow(m.1), ncol = ncol(m.1))
    
    for (i in 1: (nrow(m.1) - size)){
      for (j in 1 : (ncol(m.1) - size)){
        
        m.t1 = m.1[i:(i + size), (j: (j + size))]
        m.t2 = m.2[i:(i + size), (j: (j + size))]
        
        t.1 = (m.t1 - mean(m.t1, na.rm = T))
        std.1 <- 1 / (size ^ 4 - 1) * sum(t.1 ^ 2, na.rm = T)
        std.1 <- sqrt(std.1)
        
        t.2 = (m.t2 - mean(m.t2, na.rm = T))
        std.2 <- 1 / (size ^ 4 - 1) * sum(t.2 ^ 2, na.rm = T)
        std.2 <- sqrt(std.2)
        
        cc = 1 / (size ^ 4) / (std.1 * std.2) * sum(t.1 * t.2, na.rm = T)
        
        
        matrix.t[i:(i + size), (j: (j + size))] = cc
      }
    }  
    
    r <- raster(matrix.t)
    extent(r) <- extent(mean.GLDAS)
    crs(r) <- crs(mean.GLDAS)
    r.stack <- stack(r.stack, r)
  }
  
  r.mean <- calc(r.stack, mean)
  return(r.mean)
}

D.GLDAS.MERRA2 <- cal.Abs.Distance(mean.GLDAS, mean.MERRA2)
CC.GLDAS.MERRA2 <- cal.Cross.Correlation(mean.GLDAS, mean.MERRA2)


spplot(D.GLDAS.MERRA2)
spplot(CC.GLDAS.MERRA2)

writeRaster(D.GLDAS.MERRA2, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_D_RZSM_GLDAS_MERRA2.tif", overwrite = T)
writeRaster(CC.GLDAS.MERRA2, "F:/New PHD results/SM_variations_result_end/CMP_result/CMP_CC_RZSM_GLDAS_MERRA2.tif", overwrite = T)


################################################################################3






## ggplot CMP result


setwd("F:/New PHD results/SM_variations_result_end/CMP_result")

CMP.D.list <- list.files(pattern = "CMP_D")
CMP.D.list

CMP.D.tif <- stack(lapply(CMP.D.list, stack))
CMP.D.tif
names(CMP.D.tif)


##
CMP.CC.list <- list.files(pattern = "CMP_CC")
CMP.CC.list

CMP.CC.tif <- stack(lapply(CMP.CC.list, stack))
CMP.CC.tif
names(CMP.CC.tif)

#w <- area(CMP.CC.tif[[1]], na.rm=T) / area(CMP.CC.tif[[1]], na.rm=T) %>% cellStats("sum")
#cellStats(CMP.CC.tif[[3]]*w, "sum")


##ggplot'

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
        axis.text = element_text(size = 11, family="Times New Roman"),
        axis.title = element_text(size = 11, family="Times New Roman"),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 10,  family="Times New Roman"),
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


D_scale_fill_gradientn <- scale_fill_gradientn(colors= rev(brewer.pal(n = 11, name = "Spectral")), #brewer.pal(n=10,name = "RdYlGn"),#RdYlGn, #brewer.pal(n = 10, name = "BrBG"), # 图例颜色 brewer.pal(n = 11, name = "RdYlGn")
                                                  name = expression(paste(m^3," ",m^-3)),  # 图例名称
                                                  na.value = "white", 
                                                  breaks= c( 0.1,0.2,0.3,0.4),
                                                  labels=c('0.1','0.2','0.3','0.4'),  # 图例标签显示  # 图例标签显示
                                                  limits=c(0,0.5))


CC_scale_fill_gradientn <- scale_fill_gradientn(colors=rev(brewer.pal(n = 11, name = "RdYlGn")), #brewer.pal(n=10,name = "RdYlGn"),#RdYlGn, #brewer.pal(n = 10, name = "BrBG"), # 图例颜色 brewer.pal(n = 11, name = "RdYlGn")
                                                name = NULL,  # 图例名称
                                                na.value = "white", 
                                                breaks= c(-1, -0.5, 0, 0.5, 1),
                                                labels=c('-1','-0.5','0','0.5', '1'),  # 图例标签显示  # 图例标签显示
                                                limits=c(-1,1))


my_guides <-  guides(fill = guide_colourbar(title.position = "right",  # 图例名称位置
                                            title.hjust = 0,   # 图例名称水平方向位置
                                            title.vjust = 1.2, # 图例名称垂直方向位置
                                            ticks.colour = "black",  # #ticks线的颜色
                                            ticks.linewidth = 0.5 ,  # ticks线的宽度
                                            frame.colour = "black" ,   # 图例边框颜色;  frame.linewidth = 0.5 图例边框宽度
                                            reverse = F ))
shpfile <- readOGR("E:/New PHD/Study data/Shp/Continent_boundary/Continent_boundary_polygon.shp") %>% fortify()
#plot(shp)#geom_sf()



##
ERA5.GLDAS.D.df <- as.data.frame(CMP.D.tif[[1]], xy=T)
summary(ERA5.GLDAS.D.df) #0: 0.46

gg.ERA5.GLDAS.D <- ggplot() +
  geom_raster(ERA5.GLDAS.D.df, mapping = aes(x=x, y=y, fill = CMP_D_RZSM_ERA5_GLDAS), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "Absolute Distance (D)")


gg.ERA5.GLDAS.D.sum <- gg.ERA5.GLDAS.D + mytheme + my_scale_x_continuous + my_scale_y_continuous +
                       D_scale_fill_gradientn + my_guides
gg.ERA5.GLDAS.D.sum 





##
ERA5.MERRA2.D.df <- as.data.frame(CMP.D.tif[[2]], xy=T)
summary(ERA5.MERRA2.D.df) #0: 0.33

gg.ERA5.MERRA2.D <- ggplot() +
  geom_raster(ERA5.MERRA2.D.df, mapping = aes(x=x, y=y, fill = CMP_D_RZSM_ERA5_MERRA2), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "")


gg.ERA5.MERRA2.D.sum <- gg.ERA5.MERRA2.D + mytheme + my_scale_x_continuous + my_scale_y_continuous +
  D_scale_fill_gradientn + my_guides
gg.ERA5.MERRA2.D.sum 



##
GLDAS.MERRA2.D.df <- as.data.frame(CMP.D.tif[[3]], xy=T)
summary(GLDAS.MERRA2.D.df) #0: 0.3

gg.GLDAS.MERRA2.D <- ggplot() +
  geom_raster(GLDAS.MERRA2.D.df, mapping = aes(x=x, y=y, fill = CMP_D_RZSM_GLDAS_MERRA2), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "")


gg.GLDAS.MERRA2.D.sum <- gg.GLDAS.MERRA2.D + mytheme + my_scale_x_continuous + my_scale_y_continuous +
  D_scale_fill_gradientn + my_guides
gg.GLDAS.MERRA2.D.sum 


###
gg.D.sum <- ggarrange(gg.ERA5.GLDAS.D.sum, gg.ERA5.MERRA2.D.sum, gg.GLDAS.MERRA2.D.sum,
                      ncol=1, nrow = 3,
                      labels = c("(a)", "(c)", "(e)"),
                      font.label = list(size = 13, color = "black", face = "plain", family = "Times New Roman"),
                      hjust = -1.5, vjust = 2.5,
                      common.legend = T, legend = "bottom")
gg.D.sum

###################################3





##
ERA5.GLDAS.CC.df <- as.data.frame(CMP.CC.tif[[1]], xy=T)
summary(ERA5.GLDAS.CC.df) #-1: 1

gg.ERA5.GLDAS.CC <- ggplot() +
  geom_raster(ERA5.GLDAS.CC.df, mapping = aes(x=x, y=y, fill = CMP_CC_RZSM_ERA5_GLDAS), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "Cross-correlation (CC)", hjust=0.5)

  
  gg.ERA5.GLDAS.CC.sum <- gg.ERA5.GLDAS.CC + mytheme + my_scale_x_continuous + my_scale_y_continuous +
  CC_scale_fill_gradientn + my_guides
gg.ERA5.GLDAS.CC.sum 





##
ERA5.MERRA2.CC.df <- as.data.frame(CMP.CC.tif[[2]], xy=T)
summary(ERA5.MERRA2.CC.df) #-1: 1

gg.ERA5.MERRA2.CC <- ggplot() +
  geom_raster(ERA5.MERRA2.CC.df, mapping = aes(x=x, y=y, fill = CMP_CC_RZSM_ERA5_MERRA2), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "")

  
  gg.ERA5.MERRA2.CC.sum <- gg.ERA5.MERRA2.CC + mytheme + my_scale_x_continuous + my_scale_y_continuous +
  CC_scale_fill_gradientn + my_guides
gg.ERA5.MERRA2.CC.sum 



##
GLDAS.MERRA2.CC.df <- as.data.frame(CMP.CC.tif[[3]], xy=T)
summary(GLDAS.MERRA2.CC.df) #-1: 1

gg.GLDAS.MERRA2.CC <- ggplot() +
  geom_raster(GLDAS.MERRA2.CC.df, mapping = aes(x=x, y=y, fill = CMP_CC_RZSM_GLDAS_MERRA2), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x=NULL, y=NULL, title = "")
  
  gg.GLDAS.MERRA2.CC.sum <- gg.GLDAS.MERRA2.CC + mytheme + my_scale_x_continuous + my_scale_y_continuous +
  CC_scale_fill_gradientn + my_guides
gg.GLDAS.MERRA2.CC.sum 


###
gg.CC.sum <- ggarrange(gg.ERA5.GLDAS.CC.sum, gg.ERA5.MERRA2.CC.sum, gg.GLDAS.MERRA2.CC.sum,
                       ncol=1, nrow = 3,
                       labels = c("(b)", "(d)", "(f)"),
                       font.label = list(size = 13, color = "black", face = "plain", family = "Times New Roman"),
                       hjust = -1.5, vjust = 2.5,
                       common.legend = T, legend = "bottom")
gg.CC.sum





## 
gg.sum <- ggarrange(gg.D.sum, gg.CC.sum,
                    ncol=2, nrow = 1)

gg.sum

ggsave('F:/New PHD results/SM_variations_result_end/CMP_result/CMP_RZSM_ERA5_GLDAS_MERRA2.pdf',
       dpi = 10000,
       width = 7, height = 6) #width = 8, height = 6
