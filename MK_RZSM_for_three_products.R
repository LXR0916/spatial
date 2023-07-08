### MK RZSM of ERA5, GLDAS, MERRA2, Model mean
## 1: MK test
## 2: ggplot 

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
library(scales)
library(patchwork)


## 1: MK test-----------------------------------------------------------------------------------------

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
# 17 Water Boides has been masked




## yearly RZSM
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")

ERA5 <- brick("ERA5_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
GLDAS <- brick("GLDAS_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA)
MERRA2 <- brick("MERRA2_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA)
spplot(MERRA2[[1]])

Model.mean <- ERA5
Model.mean[] <- NA
for (i in 1:nlayers(Model.mean)) {
  Model.mean[[i]] <- calc(stack(ERA5[[i]], GLDAS[[i]], MERRA2[[i]]), function(x)mean(x)) 
}



## MK
ERA5.MK <- raster.kendall (ERA5, p.value = TRUE, z.value = FALSE, confidence = FALSE, tau = FALSE)
writeRaster(ERA5.MK, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/MK.ERA5.RZSM.tif", overwrite=T)

GLDAS.MK <- raster.kendall (GLDAS, p.value = TRUE, z.value = FALSE, confidence = FALSE, tau = FALSE)
writeRaster(GLDAS.MK, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/MK.GLDAS.RZSM.tif", overwrite=T)

MERRA2.MK <- raster.kendall (MERRA2, p.value = TRUE, z.value = FALSE, confidence = FALSE, tau = FALSE)
writeRaster(MERRA2.MK, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/MK.MERRA2.RZSM.tif", overwrite=T)

Model.mean.MK <- raster.kendall (Model.mean, p.value = TRUE, z.value = FALSE, confidence = FALSE, tau = FALSE)
writeRaster(Model.mean.MK, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/MK.Model.mean.RZSM.tif", overwrite=T)

##############################################################################################3
##############################################################################################3
##############################################################################################3



## 2: ggplot MK 空间分布-----------------------------------------------------------------------------------------

##############
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

ERA5.MK.slope <- brick("MK.ERA5.RZSM.tif")[[1]]
ERA5.MK.p <- brick("MK.ERA5.RZSM.tif")[[2]]

##
ERA5.sig <- ERA5.MK.p
ERA5.sig[ERA5.sig[] > 0.05] <- NA
spplot(ERA5.sig)

ERA5.insig <- ERA5.MK.p
ERA5.insig[ERA5.insig[] < 0.05 | ERA5.insig[] == 0.05] <- NA
spplot(ERA5.insig)



##
ERA5.de <- ERA5.MK.slope
ERA5.de[ERA5.de[] > 0 | ERA5.de[] == 0 ] <- NA
spplot(ERA5.de)

ERA5.de.sig <- mask(ERA5.de, ERA5.sig)  #-2
spplot(ERA5.de.sig)
ERA5.de.sig[ERA5.de.sig[] < 0] <- -2 # significant decrease


ERA5.de.insig <- mask(ERA5.de, ERA5.insig) #-1
spplot(ERA5.de.insig)
ERA5.de.insig[ERA5.de.insig[] < 0] <- -1 # insignificant decrease



##
ERA5.in <- ERA5.MK.slope
ERA5.in[ERA5.in[] < 0 | ERA5.in[] == 0 ] <- NA
spplot(ERA5.in)

ERA5.in.sig <- mask(ERA5.in, ERA5.sig)  #-2
spplot(ERA5.in.sig)
ERA5.in.sig[ERA5.in.sig[] > 0] <- 2 # significant increase


ERA5.in.insig <- mask(ERA5.in, ERA5.insig) #-1
spplot(ERA5.in.insig)
ERA5.in.insig[ERA5.in.insig[] > 0] <- 1 # insignificant increase

ERA5.stable <- ERA5.MK.slope
ERA5.stable[ERA5.stable[] > 0 | ERA5.stable[] < 0 ] <- NA
spplot(ERA5.stable)

## merge
x.ERA5 <- list(ERA5.de.sig, ERA5.de.insig, ERA5.in.insig, ERA5.in.sig) #ERA5.stable,
names(x.ERA5) <- c("x", "y",  "a", "b")
x.ERA5$filename <- "ERA5.merge"
x.ERA5$overwrite <- TRUE
mer.ERA5 <- do.call(merge, x.ERA5)
spplot(mer.ERA5)





##############
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

GLDAS.MK.slope <- brick("MK.GLDAS.RZSM.tif")[[1]]
GLDAS.MK.p <- brick("MK.GLDAS.RZSM.tif")[[2]]

##
GLDAS.sig <- GLDAS.MK.p
GLDAS.sig[GLDAS.sig[] > 0.05] <- NA
spplot(GLDAS.sig)

GLDAS.insig <- GLDAS.MK.p
GLDAS.insig[GLDAS.insig[] < 0.05 | GLDAS.insig[] == 0.05] <- NA
spplot(GLDAS.insig)



##
GLDAS.de <- GLDAS.MK.slope
GLDAS.de[GLDAS.de[] > 0 | GLDAS.de[] == 0 ] <- NA
spplot(GLDAS.de)

GLDAS.de.sig <- mask(GLDAS.de, GLDAS.sig)  #-2
spplot(GLDAS.de.sig)
GLDAS.de.sig[GLDAS.de.sig[] < 0] <- -2 # significant decrease


GLDAS.de.insig <- mask(GLDAS.de, GLDAS.insig) #-1
spplot(GLDAS.de.insig)
GLDAS.de.insig[GLDAS.de.insig[] < 0] <- -1 # insignificant decrease



##
GLDAS.in <- GLDAS.MK.slope
GLDAS.in[GLDAS.in[] < 0 | GLDAS.in[] == 0 ] <- NA
spplot(GLDAS.in)

GLDAS.in.sig <- mask(GLDAS.in, GLDAS.sig)  #-2
spplot(GLDAS.in.sig)
GLDAS.in.sig[GLDAS.in.sig[] > 0] <- 2 # significant increase


GLDAS.in.insig <- mask(GLDAS.in, GLDAS.insig) #-1
spplot(GLDAS.in.insig)
GLDAS.in.insig[GLDAS.in.insig[] > 0] <- 1 # insignificant increase

GLDAS.stable <- GLDAS.MK.slope
GLDAS.stable[GLDAS.stable[] > 0 | GLDAS.stable[] < 0 ] <- NA
spplot(GLDAS.stable)

## merge
x.GLDAS <- list(GLDAS.de.sig, GLDAS.de.insig, GLDAS.in.insig, GLDAS.in.sig) #GLDAS.stable,
names(x.GLDAS) <- c("x", "y",  "a", "b")
x.GLDAS$filename <- "GLDAS.merge"
x.GLDAS$overwrite <- TRUE
mer.GLDAS <- do.call(merge, x.GLDAS)
spplot(mer.GLDAS)
#writeRaster(mer.GLDAS, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/Merge.GLDAS.MK.slope.tif",overwrite=T)






########################
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

MERRA2.MK.slope <- brick("MK.MERRA2.RZSM.tif")[[1]]
MERRA2.MK.p <- brick("MK.MERRA2.RZSM.tif")[[2]]

##
MERRA2.sig <- MERRA2.MK.p
MERRA2.sig[MERRA2.sig[] > 0.05] <- NA
spplot(MERRA2.sig)

MERRA2.insig <- MERRA2.MK.p
MERRA2.insig[MERRA2.insig[] < 0.05 | MERRA2.insig[] == 0.05] <- NA
spplot(MERRA2.insig)



##
MERRA2.de <- MERRA2.MK.slope
MERRA2.de[MERRA2.de[] > 0 | MERRA2.de[] == 0 ] <- NA
spplot(MERRA2.de)

MERRA2.de.sig <- mask(MERRA2.de, MERRA2.sig)  #-2
spplot(MERRA2.de.sig)
MERRA2.de.sig[MERRA2.de.sig[] < 0] <- -2 # significant decrease

MERRA2.de.insig <- mask(MERRA2.de, MERRA2.insig) #-1
spplot(MERRA2.de.insig)
MERRA2.de.insig[MERRA2.de.insig[] < 0] <- -1 # insignificant decrease



##
MERRA2.in <- MERRA2.MK.slope
MERRA2.in[MERRA2.in[] < 0 | MERRA2.in[] == 0 ] <- NA
spplot(MERRA2.in)

MERRA2.in.sig <- mask(MERRA2.in, MERRA2.sig)  #-2
spplot(MERRA2.in.sig)
MERRA2.in.sig[MERRA2.in.sig[] > 0] <- 2 # significant increase

MERRA2.in.insig <- mask(MERRA2.in, MERRA2.insig) #-1
spplot(MERRA2.in.insig)
MERRA2.in.insig[MERRA2.in.insig[] > 0] <- 1 # insignificant increase

MERRA2.stable <- MERRA2.MK.slope
MERRA2.stable[MERRA2.stable[] > 0 | MERRA2.stable[] < 0 ] <- NA
spplot(MERRA2.stable)


## merge
x.MERRA2 <- list(MERRA2.de.sig, MERRA2.de.insig, MERRA2.in.insig, MERRA2.in.sig) # MERRA2.stable,
names(x.MERRA2) <- c("x", "y",  "a", "b") #"z",
x.MERRA2$filename <- "MERRA2.merge"
x.MERRA2$overwrite <- TRUE
mer.MERRA2 <- do.call(merge, x.MERRA2)
spplot(mer.MERRA2)
#writeRaster(mer.MERRA2, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/Merge.MERRA2.MK.slope.tif",overwrite=T)






#############################
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

Model.mean.MK.slope <- brick("MK.Model.mean.RZSM.tif")[[1]]
Model.mean.MK.p <- brick("MK.Model.mean.RZSM.tif")[[2]]

##
Model.mean.sig <- Model.mean.MK.p
Model.mean.sig[Model.mean.sig[] > 0.05] <- NA
spplot(Model.mean.sig)

Model.mean.insig <- Model.mean.MK.p
Model.mean.insig[Model.mean.insig[] < 0.05 | Model.mean.insig[] == 0.05] <- NA
spplot(Model.mean.insig)



##
Model.mean.de <- Model.mean.MK.slope
Model.mean.de[Model.mean.de[] > 0 | Model.mean.de[] == 0 ] <- NA
spplot(Model.mean.de)

Model.mean.de.sig <- mask(Model.mean.de, Model.mean.sig)  #-2
spplot(Model.mean.de.sig)
Model.mean.de.sig[Model.mean.de.sig[] < 0] <- -2 # significant decrease

Model.mean.de.insig <- mask(Model.mean.de, Model.mean.insig) #-1
spplot(Model.mean.de.insig)
Model.mean.de.insig[Model.mean.de.insig[] < 0] <- -1 # insignificant decrease



##
Model.mean.in <- Model.mean.MK.slope
Model.mean.in[Model.mean.in[] < 0 | Model.mean.in[] == 0 ] <- NA
spplot(Model.mean.in)

Model.mean.in.sig <- mask(Model.mean.in, Model.mean.sig)  #-2
spplot(Model.mean.in.sig)
Model.mean.in.sig[Model.mean.in.sig[] > 0] <- 2 # significant increase

Model.mean.in.insig <- mask(Model.mean.in, Model.mean.insig) #-1
spplot(Model.mean.in.insig)
Model.mean.in.insig[Model.mean.in.insig[] > 0] <- 1 # insignificant increase




## merge
x.Model.mean <- list(Model.mean.de.sig, Model.mean.de.insig, Model.mean.in.insig, Model.mean.in.sig)# Model.mean.stable,
names(x.Model.mean) <- c("x", "y",  "a", "b") #"z",
x.Model.mean$filename <- "Model.mean.merge"
x.Model.mean$overwrite <- TRUE
mer.Model.mean <- do.call(merge, x.Model.mean)
spplot(mer.Model.mean)
#writeRaster(mer.Model.mean, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/Merge.Model.mean.MK.slope.tif",overwrite=T)


##########







### ggplot slope and slope.significant

#font_import("timesi.ttf")
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
#font_add("Italic Times New Roman", "timesi.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()
#windowsFonts()


slope.theme <- theme_bw() + 
  theme(panel.grid = element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.5,-0.25),#"bottom",
        legend.direction = "horizontal",
        legend.key.width=unit(1.6,'cm'),legend.key.size=unit(0.18,'cm'), # 图例长度与高度
        legend.title = element_text(size=12, family="Times New Roman"),  # 图例名称字体大小
        legend.text = element_text(size = 12, family="Times New Roman"),
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.key = element_blank(), ## 图例背景去掉
        axis.text = element_text(size = 15, family="Times New Roman"),
        axis.title = element_text(size = 15, family="Times New Roman"),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 15,  family="Times New Roman"),
        plot.margin = unit(c(0.2,1.5,0.2,1),"lines") # (top, right, bottom, left)
  )



sig.theme <- theme_bw() + 
  theme(panel.grid = element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        #legend.position = "bottom",
        legend.position = c(0.5,-0.25),#"bottom",
        legend.direction = "horizontal",
        legend.spacing = unit(0.6, 'cm'),
        legend.key.width=unit(0.8,'cm'),legend.key.size=unit(0.25,'cm'), # 图例长度与高度
        legend.title = element_text(size=12, family="Times New Roman"),  # 图例名称字体大小
        legend.text = element_text(size = 12, family="Times New Roman"),
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.key = element_blank(), ## 图例背景去掉
        axis.text = element_text(size = 15, family="Times New Roman"),
        axis.title = element_text(size = 15, family="Times New Roman"),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 15,  family="Times New Roman"),
        plot.margin = unit(c(0.1,1.5,0.1,1),"lines") # (top, right, bottom, left)
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


slope_scale_fill_gradientn <- scale_fill_gradientn(colors= rev(matlab.like(8)),#brewer.pal(n=9, name = "RdYlBu"),#, #brewer.pal(n = 10, name = "BrBG")
                                                   name = expression(paste(m^3," ",m^-3," ", yr^-1)),  # 图例名称
                                                   na.value = "white", 
                                                   breaks= c(-0.004, -0.002, 0, 0.002, 0.004),
                                                   labels=c('-0.004','-0.002','0','0.002','0.004'),  # 图例标签显示  # 图例标签显示
                                                   limits=c(-0.005,0.005))


mycolors <- c("#FF7F7E", "#FEBEBE", "#A3FF74", "#38A700") # "#E1E1E1", #c("#C51B7D", "#F1B6DA", "grey", "#B8E186", "#4D9221")
sig_scale_fill_manual <- scale_fill_manual(name=NULL, values = mycolors,
                                           labels=c("Significant decreased", "Insignificant decreased",
                                                    "Insignificant increased ","Significant increased"))

my_guides <- guides(fill = guide_colourbar(title.position = "right",  # 图例名称位置
                                           title.hjust = 0,   # 图例名称水平方向位置
                                           title.vjust = 1.2, # 图例名称垂直方向位置
                                           ticks.colour = "black",  # #ticks线的颜色
                                           ticks.linewidth = 0.5 ,  # ticks线的宽度
                                           frame.colour = "black" ,   # 图例尺边框颜色;  frame.linewidth = 0.5 图例边框宽度
                                           reverse = F ))   # 图例反向


shpfile <- readOGR("E:/New PHD/Study data/Shp/Continent_boundary/Continent_boundary_polygon.shp") %>% fortify()


#display.brewer.pal(11, "PiYG")
#brewer.pal(11, "PiYG")




### ERA5---------------------------------------------------------------------------------------------
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

ERA5.MK.slope <- brick("MK.ERA5.RZSM.tif")[[1]]
ERA5.MK.p <- brick("MK.ERA5.RZSM.tif")[[2]]

##
ERA5.sig <- ERA5.MK.p
ERA5.sig[ERA5.sig[] > 0.05] <- NA
spplot(ERA5.sig)

ERA5.insig <- ERA5.MK.p
ERA5.insig[ERA5.insig[] < 0.05 | ERA5.insig[] == 0.05] <- NA
spplot(ERA5.insig)



##
ERA5.de <- ERA5.MK.slope
ERA5.de[ERA5.de[] > 0 | ERA5.de[] == 0 ] <- NA
spplot(ERA5.de)

ERA5.de.sig <- mask(ERA5.de, ERA5.sig)  #-2
spplot(ERA5.de.sig)
ERA5.de.sig[ERA5.de.sig[] < 0] <- -2 # significant decrease


ERA5.de.insig <- mask(ERA5.de, ERA5.insig) #-1
spplot(ERA5.de.insig)
ERA5.de.insig[ERA5.de.insig[] < 0] <- -1 # insignificant decrease



##
ERA5.in <- ERA5.MK.slope
ERA5.in[ERA5.in[] < 0 | ERA5.in[] == 0 ] <- NA
spplot(ERA5.in)

ERA5.in.sig <- mask(ERA5.in, ERA5.sig)  #-2
spplot(ERA5.in.sig)
ERA5.in.sig[ERA5.in.sig[] > 0] <- 2 # significant increase


ERA5.in.insig <- mask(ERA5.in, ERA5.insig) #-1
spplot(ERA5.in.insig)
ERA5.in.insig[ERA5.in.insig[] > 0] <- 1 # insignificant increase

ERA5.stable <- ERA5.MK.slope
ERA5.stable[ERA5.stable[] > 0 | ERA5.stable[] < 0 ] <- NA
spplot(ERA5.stable)



## merge
x.ERA5 <- list(ERA5.de.sig, ERA5.de.insig, ERA5.in.insig, ERA5.in.sig)
names(x.ERA5) <- c("x", "y",  "a", "b") #"z",
x.ERA5$filename <- "ERA5.merge"
x.ERA5$overwrite <- TRUE
mer.ERA5 <- do.call(merge, x.ERA5)
spplot(mer.ERA5)
#writeRaster(mer.ERA5, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/Merge.ERA5.MK.slope.tif",overwrite=T)
################



## percentage
###
ERA5.de.sig <- mer.ERA5
ERA5.de.sig[ERA5.de.sig[] == -1 | ERA5.de.sig[] == 1 | ERA5.de.sig[]==2 ] <- NA
ERA5.de.sig.per <- area(ERA5.de.sig, na.rm=T) %>% cellStats("sum") / area(mer.ERA5, na.rm=T) %>% cellStats("sum")

ERA5.de.insig <- mer.ERA5
ERA5.de.insig[ERA5.de.insig[] == -2 | ERA5.de.insig[] == 1 | ERA5.de.insig[]==2] <- NA
ERA5.de.insig.per <- area(ERA5.de.insig, na.rm=T) %>% cellStats("sum") / area(mer.ERA5, na.rm=T) %>% cellStats("sum")


ERA5.in.sig <- mer.ERA5
ERA5.in.sig[ERA5.in.sig[] == -2 | ERA5.in.sig[] == -1 | ERA5.in.sig[]==1 ] <- NA
ERA5.in.sig.per <- area(ERA5.in.sig, na.rm=T) %>% cellStats("sum") / area(mer.ERA5, na.rm=T) %>% cellStats("sum")

ERA5.in.insig <- mer.ERA5
ERA5.in.insig[ERA5.in.insig[] == -2 | ERA5.in.insig[] == -1 | ERA5.in.insig[]==2 ] <- NA
ERA5.in.insig.per <- area(ERA5.in.insig, na.rm=T) %>% cellStats("sum") / area(mer.ERA5, na.rm=T) %>% cellStats("sum")


## percentage
ERA5.percent.df <- rbind(ERA5.de.sig.per, ERA5.de.insig.per,
                         ERA5.in.insig.per, ERA5.in.sig.per)  %>%  as.data.frame()
ERA5.percent.df <- ERA5.percent.df*100 
names(ERA5.percent.df) <- "ERA5.per"

ERA5.percent.df$Type <- c("Significant decreased", "Insignificant decreased",
                          "Insignificant increased ","Significant increased") %>%
  factor(levels = c("Significant decreased", "Insignificant decreased",
                    "Insignificant increased ","Significant increased"))



####ggplot-------------------------------------------
###
ERA5.slope.df <- as.data.frame(ERA5.MK.slope, xy=T)
summary(ERA5.slope.df) # -0.003: 0.0030

gg.ERA5.slope <- ggplot() +
  geom_raster(ERA5.slope.df, mapping = aes(x=x, y=y, fill = MK.ERA5.RZSM.1), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x =NULL, y = NULL, title = "(a)")
# theme( plot.title = element_text(hjust = 0.5, vjust = 0, size =3,  family="Times New Roman"))


gg.ERA5.slope.sum <- gg.ERA5.slope + slope.theme + my_scale_x_continuous + my_scale_y_continuous + 
  my_guides + slope_scale_fill_gradientn
gg.ERA5.slope.sum

##############



####
mer.ERA5.df <- as.data.frame(mer.ERA5, xy=T) %>% na.omit()
summary(mer.ERA5.df)

gg.mer.ERA5 <- ggplot()+
  geom_raster(mer.ERA5.df, mapping = aes(x=x, y=y, fill =  factor(layer)), show.legend =T) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) + 
  labs(x =NULL, y = NULL, title = "(b)") +
  guides(fill = guide_legend(ncol = 2, byrow = T))


gg.mer.ERA5.sum <- gg.mer.ERA5 + sig.theme + my_scale_x_continuous + my_scale_y_continuous +
  sig_scale_fill_manual
gg.mer.ERA5.sum
############################


##ggplot percentage   ERA5.percent.df
gg.per.ERA5 <- ggplot(ERA5.percent.df, aes(x="", y=ERA5.per, fill=Type), show.legend =F) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(ERA5.per %>% round(digits = 0), "%")),
            position =position_stack(vjust = 0.5), #"stack",
            colour="black", size=3,  family="Times New Roman") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=mycolors)+
  theme_void()+
  theme(legend.position = "none")

gg.per.ERA5



###
p1 <- gg.mer.ERA5.sum + inset_element(gg.per.ERA5, left = -0.15, bottom = 0, right = 0.4, top = 0.4)

gg.ERA5.slope.sum / p1 +
  plot_layout(ncol=2)

ggsave("F:/SCI_02_专家回复/MK_RZSM_出图/MK.ERA5.pdf",
       width = 10,height = 3.5,
       dpi = 1200)




###################################################################
###################################################################
###################################################################








### GLDAS---------------------------------------------------------------------------------------------
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

GLDAS.MK.slope <- brick("MK.GLDAS.RZSM.tif")[[1]]
GLDAS.MK.p <- brick("MK.GLDAS.RZSM.tif")[[2]]

##
GLDAS.sig <- GLDAS.MK.p
GLDAS.sig[GLDAS.sig[] > 0.05] <- NA
spplot(GLDAS.sig)

GLDAS.insig <- GLDAS.MK.p
GLDAS.insig[GLDAS.insig[] < 0.05 | GLDAS.insig[] == 0.05] <- NA
spplot(GLDAS.insig)



##
GLDAS.de <- GLDAS.MK.slope
GLDAS.de[GLDAS.de[] > 0 | GLDAS.de[] == 0 ] <- NA
spplot(GLDAS.de)

GLDAS.de.sig <- mask(GLDAS.de, GLDAS.sig)  #-2
spplot(GLDAS.de.sig)
GLDAS.de.sig[GLDAS.de.sig[] < 0] <- -2 # significant decrease


GLDAS.de.insig <- mask(GLDAS.de, GLDAS.insig) #-1
spplot(GLDAS.de.insig)
GLDAS.de.insig[GLDAS.de.insig[] < 0] <- -1 # insignificant decrease



##
GLDAS.in <- GLDAS.MK.slope
GLDAS.in[GLDAS.in[] < 0 | GLDAS.in[] == 0 ] <- NA
spplot(GLDAS.in)

GLDAS.in.sig <- mask(GLDAS.in, GLDAS.sig)  #-2
spplot(GLDAS.in.sig)
GLDAS.in.sig[GLDAS.in.sig[] > 0] <- 2 # significant increase


GLDAS.in.insig <- mask(GLDAS.in, GLDAS.insig) #-1
spplot(GLDAS.in.insig)
GLDAS.in.insig[GLDAS.in.insig[] > 0] <- 1 # insignificant increase

GLDAS.stable <- GLDAS.MK.slope
GLDAS.stable[GLDAS.stable[] > 0 | GLDAS.stable[] < 0 ] <- NA
spplot(GLDAS.stable)



## merge
x.GLDAS <- list(GLDAS.de.sig, GLDAS.de.insig, GLDAS.in.insig, GLDAS.in.sig)
names(x.GLDAS) <- c("x", "y",  "a", "b") #"z",
x.GLDAS$filename <- "GLDAS.merge"
x.GLDAS$overwrite <- TRUE
mer.GLDAS <- do.call(merge, x.GLDAS)
spplot(mer.GLDAS)
#writeRaster(mer.GLDAS, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_GLDAS_GLDAS_MERRA2_MK/Merge.GLDAS.MK.slope.tif",overwrite=T)
################



## percentage
###
GLDAS.de.sig <- mer.GLDAS
GLDAS.de.sig[GLDAS.de.sig[] == -1 | GLDAS.de.sig[] == 1 | GLDAS.de.sig[]==2 ] <- NA
GLDAS.de.sig.per <- area(GLDAS.de.sig, na.rm=T) %>% cellStats("sum") / area(mer.GLDAS, na.rm=T) %>% cellStats("sum")

GLDAS.de.insig <- mer.GLDAS
GLDAS.de.insig[GLDAS.de.insig[] == -2 | GLDAS.de.insig[] == 1 | GLDAS.de.insig[]==2] <- NA
GLDAS.de.insig.per <- area(GLDAS.de.insig, na.rm=T) %>% cellStats("sum") / area(mer.GLDAS, na.rm=T) %>% cellStats("sum")


GLDAS.in.sig <- mer.GLDAS
GLDAS.in.sig[GLDAS.in.sig[] == -2 | GLDAS.in.sig[] == -1 | GLDAS.in.sig[]==1 ] <- NA
GLDAS.in.sig.per <- area(GLDAS.in.sig, na.rm=T) %>% cellStats("sum") / area(mer.GLDAS, na.rm=T) %>% cellStats("sum")

GLDAS.in.insig <- mer.GLDAS
GLDAS.in.insig[GLDAS.in.insig[] == -2 | GLDAS.in.insig[] == -1 | GLDAS.in.insig[]==2 ] <- NA
GLDAS.in.insig.per <- area(GLDAS.in.insig, na.rm=T) %>% cellStats("sum") / area(mer.GLDAS, na.rm=T) %>% cellStats("sum")


## percentage
GLDAS.percent.df <- rbind(GLDAS.de.sig.per, GLDAS.de.insig.per,
                          GLDAS.in.insig.per, GLDAS.in.sig.per)  %>%  as.data.frame()
GLDAS.percent.df <- GLDAS.percent.df*100 
names(GLDAS.percent.df) <- "GLDAS.per"

GLDAS.percent.df$Type <- c("Significant decreased", "Insignificant decreased",
                           "Insignificant increased ","Significant increased") %>%
  factor(levels = c("Significant decreased", "Insignificant decreased",
                    "Insignificant increased ","Significant increased"))



####ggplot-------------------------------------------
###
GLDAS.slope.df <- as.data.frame(GLDAS.MK.slope, xy=T)
summary(GLDAS.slope.df) # -0.003: 0.0030

gg.GLDAS.slope <- ggplot() +
  geom_raster(GLDAS.slope.df, mapping = aes(x=x, y=y, fill = MK.GLDAS.RZSM.1), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x =NULL, y = NULL, title = "(c)")
# theme( plot.title = element_text(hjust = 0.5, vjust = 0, size =3,  family="Times New Roman"))


gg.GLDAS.slope.sum <- gg.GLDAS.slope + slope.theme + my_scale_x_continuous + my_scale_y_continuous + 
  my_guides + slope_scale_fill_gradientn
gg.GLDAS.slope.sum

##############



####
mer.GLDAS.df <- as.data.frame(mer.GLDAS, xy=T) %>% na.omit()
summary(mer.GLDAS.df)

gg.mer.GLDAS <- ggplot()+
  geom_raster(mer.GLDAS.df, mapping = aes(x=x, y=y, fill =  factor(layer)), show.legend =T) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) + 
  labs(x =NULL, y = NULL, title = "(d)")+
  guides(fill = guide_legend(ncol = 2, byrow = T))


gg.mer.GLDAS.sum <- gg.mer.GLDAS + sig.theme + my_scale_x_continuous + my_scale_y_continuous +
  sig_scale_fill_manual
gg.mer.GLDAS.sum
############################


##ggplot percentage   GLDAS.percent.df
gg.per.GLDAS <- ggplot(GLDAS.percent.df, aes(x="", y=GLDAS.per, fill=Type), show.legend =F) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(GLDAS.per %>% round(digits = 0), "%")),
            position =position_stack(vjust = 0.5), #"stack",
            colour="black", size=3,  family="Times New Roman") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=mycolors)+
  theme_void()+
  theme(legend.position = "none")

gg.per.GLDAS



###
p1 <- gg.mer.GLDAS.sum + inset_element(gg.per.GLDAS, left = -0.15, bottom = 0, right = 0.4, top = 0.4)


gg.GLDAS.slope.sum / p1 +
  plot_layout(ncol=2)

ggsave("F:/SCI_02_专家回复/MK_RZSM_出图/MK.GLDAS.pdf",
       width = 10,height = 3.5,
       dpi = 1200)

###################################################################
###################################################################
###################################################################






### MERRA2---------------------------------------------------------------------------------------------
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

MERRA2.MK.slope <- brick("MK.MERRA2.RZSM.tif")[[1]]
MERRA2.MK.p <- brick("MK.MERRA2.RZSM.tif")[[2]]

##
MERRA2.sig <- MERRA2.MK.p
MERRA2.sig[MERRA2.sig[] > 0.05] <- NA
spplot(MERRA2.sig)

MERRA2.insig <- MERRA2.MK.p
MERRA2.insig[MERRA2.insig[] < 0.05 | MERRA2.insig[] == 0.05] <- NA
spplot(MERRA2.insig)



##
MERRA2.de <- MERRA2.MK.slope
MERRA2.de[MERRA2.de[] > 0 | MERRA2.de[] == 0 ] <- NA
spplot(MERRA2.de)

MERRA2.de.sig <- mask(MERRA2.de, MERRA2.sig)  #-2
spplot(MERRA2.de.sig)
MERRA2.de.sig[MERRA2.de.sig[] < 0] <- -2 # significant decrease


MERRA2.de.insig <- mask(MERRA2.de, MERRA2.insig) #-1
spplot(MERRA2.de.insig)
MERRA2.de.insig[MERRA2.de.insig[] < 0] <- -1 # insignificant decrease



##
MERRA2.in <- MERRA2.MK.slope
MERRA2.in[MERRA2.in[] < 0 | MERRA2.in[] == 0 ] <- NA
spplot(MERRA2.in)

MERRA2.in.sig <- mask(MERRA2.in, MERRA2.sig)  #-2
spplot(MERRA2.in.sig)
MERRA2.in.sig[MERRA2.in.sig[] > 0] <- 2 # significant increase


MERRA2.in.insig <- mask(MERRA2.in, MERRA2.insig) #-1
spplot(MERRA2.in.insig)
MERRA2.in.insig[MERRA2.in.insig[] > 0] <- 1 # insignificant increase

MERRA2.stable <- MERRA2.MK.slope
MERRA2.stable[MERRA2.stable[] > 0 | MERRA2.stable[] < 0 ] <- NA
spplot(MERRA2.stable)



## merge
x.MERRA2 <- list(MERRA2.de.sig, MERRA2.de.insig, MERRA2.in.insig, MERRA2.in.sig)
names(x.MERRA2) <- c("x", "y",  "a", "b") #"z",
x.MERRA2$filename <- "MERRA2.merge"
x.MERRA2$overwrite <- TRUE
mer.MERRA2 <- do.call(merge, x.MERRA2)
spplot(mer.MERRA2)
#writeRaster(mer.MERRA2, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_MERRA2_MERRA2_MERRA2_MK/Merge.MERRA2.MK.slope.tif",overwrite=T)
################



## percentage
###
MERRA2.de.sig <- mer.MERRA2
MERRA2.de.sig[MERRA2.de.sig[] == -1 | MERRA2.de.sig[] == 1 | MERRA2.de.sig[]==2 ] <- NA
MERRA2.de.sig.per <- area(MERRA2.de.sig, na.rm=T) %>% cellStats("sum") / area(mer.MERRA2, na.rm=T) %>% cellStats("sum")

MERRA2.de.insig <- mer.MERRA2
MERRA2.de.insig[MERRA2.de.insig[] == -2 | MERRA2.de.insig[] == 1 | MERRA2.de.insig[]==2] <- NA
MERRA2.de.insig.per <- area(MERRA2.de.insig, na.rm=T) %>% cellStats("sum") / area(mer.MERRA2, na.rm=T) %>% cellStats("sum")


MERRA2.in.sig <- mer.MERRA2
MERRA2.in.sig[MERRA2.in.sig[] == -2 | MERRA2.in.sig[] == -1 | MERRA2.in.sig[]==1 ] <- NA
MERRA2.in.sig.per <- area(MERRA2.in.sig, na.rm=T) %>% cellStats("sum") / area(mer.MERRA2, na.rm=T) %>% cellStats("sum")

MERRA2.in.insig <- mer.MERRA2
MERRA2.in.insig[MERRA2.in.insig[] == -2 | MERRA2.in.insig[] == -1 | MERRA2.in.insig[]==2 ] <- NA
MERRA2.in.insig.per <- area(MERRA2.in.insig, na.rm=T) %>% cellStats("sum") / area(mer.MERRA2, na.rm=T) %>% cellStats("sum")


## percentage
MERRA2.percent.df <- rbind(MERRA2.de.sig.per, MERRA2.de.insig.per,
                           MERRA2.in.insig.per, MERRA2.in.sig.per)  %>%  as.data.frame()
MERRA2.percent.df <- MERRA2.percent.df*100 
names(MERRA2.percent.df) <- "MERRA2.per"

MERRA2.percent.df$Type <- c("Significant decreased", "Insignificant decreased",
                            "Insignificant increased ","Significant increased") %>%
  factor(levels = c("Significant decreased", "Insignificant decreased",
                    "Insignificant increased ","Significant increased"))



####ggplot-------------------------------------------
###
MERRA2.slope.df <- as.data.frame(MERRA2.MK.slope, xy=T)
summary(MERRA2.slope.df) # -0.003: 0.0030

gg.MERRA2.slope <- ggplot() +
  geom_raster(MERRA2.slope.df, mapping = aes(x=x, y=y, fill = MK.MERRA2.RZSM.1), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x =NULL, y = NULL, title = "(e)")
# theme( plot.title = element_text(hjust = 0.5, vjust = 0, size =3,  family="Times New Roman"))


gg.MERRA2.slope.sum <- gg.MERRA2.slope + slope.theme + my_scale_x_continuous + my_scale_y_continuous + 
  my_guides + slope_scale_fill_gradientn
gg.MERRA2.slope.sum

##############



####
mer.MERRA2.df <- as.data.frame(mer.MERRA2, xy=T) %>% na.omit()
summary(mer.MERRA2.df)

gg.mer.MERRA2 <- ggplot()+
  geom_raster(mer.MERRA2.df, mapping = aes(x=x, y=y, fill =  factor(layer)), show.legend =T) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) + 
  labs(x =NULL, y = NULL, title = "(f)") +
  guides(fill = guide_legend(ncol = 2, byrow = T))



gg.mer.MERRA2.sum <- gg.mer.MERRA2 + sig.theme + my_scale_x_continuous + my_scale_y_continuous +
  sig_scale_fill_manual
gg.mer.MERRA2.sum
############################


##ggplot percentage   MERRA2.percent.df
gg.per.MERRA2 <- ggplot(MERRA2.percent.df, aes(x="", y=MERRA2.per, fill=Type), show.legend =F) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(MERRA2.per %>% round(digits = 0), "%")),
            position =position_stack(vjust = 0.5), #"stack",
            colour="black", size=3,  family="Times New Roman") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=mycolors)+
  theme_void()+
  theme(legend.position = "none")

gg.per.MERRA2



###
p1 <- gg.mer.MERRA2.sum + inset_element(gg.per.MERRA2, left = -0.15, bottom = 0, right = 0.4, top = 0.4)


gg.MERRA2.slope.sum / p1 +
  plot_layout(ncol=2)

ggsave("F:/SCI_02_专家回复/MK_RZSM_出图/MK.MERRA2.pdf",
       width = 10,height = 3.5,
       dpi = 1200)

###################################################################
###################################################################
###################################################################






### Model.mean---------------------------------------------------------------------------------------------
setwd("F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_ERA5_GLDAS_MERRA2_MK/")

Model.mean.MK.slope <- brick("MK.Model.mean.RZSM.tif")[[1]]
Model.mean.MK.p <- brick("MK.Model.mean.RZSM.tif")[[2]]

##
Model.mean.sig <- Model.mean.MK.p
Model.mean.sig[Model.mean.sig[] > 0.05] <- NA
spplot(Model.mean.sig)

Model.mean.insig <- Model.mean.MK.p
Model.mean.insig[Model.mean.insig[] < 0.05 | Model.mean.insig[] == 0.05] <- NA
spplot(Model.mean.insig)



##
Model.mean.de <- Model.mean.MK.slope
Model.mean.de[Model.mean.de[] > 0 | Model.mean.de[] == 0 ] <- NA
spplot(Model.mean.de)

Model.mean.de.sig <- mask(Model.mean.de, Model.mean.sig)  #-2
spplot(Model.mean.de.sig)
Model.mean.de.sig[Model.mean.de.sig[] < 0] <- -2 # significant decrease


Model.mean.de.insig <- mask(Model.mean.de, Model.mean.insig) #-1
spplot(Model.mean.de.insig)
Model.mean.de.insig[Model.mean.de.insig[] < 0] <- -1 # insignificant decrease



##
Model.mean.in <- Model.mean.MK.slope
Model.mean.in[Model.mean.in[] < 0 | Model.mean.in[] == 0 ] <- NA
spplot(Model.mean.in)

Model.mean.in.sig <- mask(Model.mean.in, Model.mean.sig)  #-2
spplot(Model.mean.in.sig)
Model.mean.in.sig[Model.mean.in.sig[] > 0] <- 2 # significant increase


Model.mean.in.insig <- mask(Model.mean.in, Model.mean.insig) #-1
spplot(Model.mean.in.insig)
Model.mean.in.insig[Model.mean.in.insig[] > 0] <- 1 # insignificant increase

Model.mean.stable <- Model.mean.MK.slope
Model.mean.stable[Model.mean.stable[] > 0 | Model.mean.stable[] < 0 ] <- NA
spplot(Model.mean.stable)



## merge
x.Model.mean <- list(Model.mean.de.sig, Model.mean.de.insig, Model.mean.in.insig, Model.mean.in.sig)
names(x.Model.mean) <- c("x", "y",  "a", "b") #"z",
x.Model.mean$filename <- "Model.mean.merge"
x.Model.mean$overwrite <- TRUE
mer.Model.mean <- do.call(merge, x.Model.mean)
spplot(mer.Model.mean)
#writeRaster(mer.Model.mean, "F:/New PHD results/SM_variations_result_end/SM_MK_test/RZSM_Model.mean_Model.mean_Model.mean_MK/Merge.Model.mean.MK.slope.tif",overwrite=T)
################



## percentage
###
Model.mean.de.sig <- mer.Model.mean
Model.mean.de.sig[Model.mean.de.sig[] == -1 | Model.mean.de.sig[] == 1 | Model.mean.de.sig[]==2 ] <- NA
Model.mean.de.sig.per <- area(Model.mean.de.sig, na.rm=T) %>% cellStats("sum") / area(mer.Model.mean, na.rm=T) %>% cellStats("sum")

Model.mean.de.insig <- mer.Model.mean
Model.mean.de.insig[Model.mean.de.insig[] == -2 | Model.mean.de.insig[] == 1 | Model.mean.de.insig[]==2] <- NA
Model.mean.de.insig.per <- area(Model.mean.de.insig, na.rm=T) %>% cellStats("sum") / area(mer.Model.mean, na.rm=T) %>% cellStats("sum")


Model.mean.in.sig <- mer.Model.mean
Model.mean.in.sig[Model.mean.in.sig[] == -2 | Model.mean.in.sig[] == -1 | Model.mean.in.sig[]==1 ] <- NA
Model.mean.in.sig.per <- area(Model.mean.in.sig, na.rm=T) %>% cellStats("sum") / area(mer.Model.mean, na.rm=T) %>% cellStats("sum")

Model.mean.in.insig <- mer.Model.mean
Model.mean.in.insig[Model.mean.in.insig[] == -2 | Model.mean.in.insig[] == -1 | Model.mean.in.insig[]==2 ] <- NA
Model.mean.in.insig.per <- area(Model.mean.in.insig, na.rm=T) %>% cellStats("sum") / area(mer.Model.mean, na.rm=T) %>% cellStats("sum")


## percentage
Model.mean.percent.df <- rbind(Model.mean.de.sig.per, Model.mean.de.insig.per,
                               Model.mean.in.insig.per, Model.mean.in.sig.per)  %>%  as.data.frame()
Model.mean.percent.df <- Model.mean.percent.df*100 
names(Model.mean.percent.df) <- "Model.mean.per"

Model.mean.percent.df$Type <- c("Significant decreased", "Insignificant decreased",
                                "Insignificant increased ","Significant increased") %>%
  factor(levels = c("Significant decreased", "Insignificant decreased",
                    "Insignificant increased ","Significant increased"))



####ggplot-------------------------------------------
###
Model.mean.slope.df <- as.data.frame(Model.mean.MK.slope, xy=T)
summary(Model.mean.slope.df) # -0.003: 0.0030

gg.Model.mean.slope <- ggplot() +
  geom_raster(Model.mean.slope.df, mapping = aes(x=x, y=y, fill = MK.Model.mean.RZSM.1), show.legend =T)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90)) +
  labs(x =NULL, y = NULL, title = "(g)")
# theme( plot.title = element_text(hjust = 0.5, vjust = 0, size =3,  family="Times New Roman"))


gg.Model.mean.slope.sum <- gg.Model.mean.slope + slope.theme + my_scale_x_continuous + my_scale_y_continuous + 
  my_guides + slope_scale_fill_gradientn
gg.Model.mean.slope.sum

##############



####
mer.Model.mean.df <- as.data.frame(mer.Model.mean, xy=T) %>% na.omit()
summary(mer.Model.mean.df)

gg.mer.Model.mean <- ggplot()+
  geom_raster(mer.Model.mean.df, mapping = aes(x=x, y=y, fill =  factor(layer)), show.legend =T) +
  coord_equal(ratio=1,expand=T,ylim=c(-90,90))+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) + 
  labs(x =NULL, y = NULL, title = "(h)")+  
  guides(fill = guide_legend(ncol = 2, byrow = T))



gg.mer.Model.mean.sum <- gg.mer.Model.mean + sig.theme + my_scale_x_continuous + my_scale_y_continuous +
  sig_scale_fill_manual
gg.mer.Model.mean.sum
############################


##ggplot percentage   Model.mean.percent.df
gg.per.Model.mean <- ggplot(Model.mean.percent.df, aes(x="", y=Model.mean.per, fill=Type), show.legend =F) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label=paste0(Model.mean.per %>% round(digits = 0), "%")),
            position =position_stack(vjust = 0.5), #"stack",
            colour="black", size=3,  family="Times New Roman") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=mycolors)+
  theme_void()+
  theme(legend.position = "none")

gg.per.Model.mean



###
p1 <- gg.mer.Model.mean.sum + inset_element(gg.per.Model.mean, left = -0.15, bottom = 0, right = 0.4, top = 0.4)


gg.Model.mean.slope.sum / p1 +
  plot_layout(ncol=2)

ggsave("F:/SCI_02_专家回复/MK_RZSM_出图/MK.Model.mean.pdf",
       width = 10,height = 3.5,
       dpi = 1200)

###################################################################
###################################################################
###################################################################

