## ggplot latitude RZSM 

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


mean.ERA5.df <- calc(ERA5, function(x)mean(x)) %>% as.data.frame(xy=T)
mean.GLDAS.df <- calc(GLDAS, function(x)mean(x)) %>% as.data.frame(xy=T)
mean.MERRA2.df <- calc(MERRA2, function(x)mean(x)) %>% as.data.frame(xy=T)
mean.Model.mean.df <- calc(Model.mean, function(x)mean(x)) %>% as.data.frame(xy=T)

mean.df <- cbind(mean.ERA5.df, mean.GLDAS.df[,3], mean.MERRA2.df[,3], mean.Model.mean.df[,3]) %>% na.omit
names(mean.df) <- c("Long", "Lat", "ERA5", "GLDAS", "MERRA2", "Model.mean")


Lat.data.aggre <- aggregate(cbind(mean.df$ERA5, mean.df$GLDAS, 
                                  mean.df$MERRA2, mean.df$Model.mean) ~ Lat, mean.df, FUN = "mean") 
names(Lat.data.aggre) <- c("Lat", "Lat.ERA5", "Lat.GLDAS", "Lat.MERRA2", "Lat.Model.mean")
write.csv(Lat.data.aggre, "F:/New PHD results/SM_variations_result_end/SM_latitude_gradient/RZSM_latitude.csv")



## ggplot
font_import("timesi.ttf")
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
#font_add("Italic Times New Roman", "timesi.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()


Lat.data.aggre.long <- melt(Lat.data.aggre, id="Lat")
names(Lat.data.aggre.long)

gg.rzsm <- ggplot(data = Lat.data.aggre.long, aes(x=value, y=Lat)) + theme_bw() +
  geom_line(aes(color = variable), orientation = 'y', lwd = 0.4) +
  scale_color_manual(values = c("Cyan2","#0099FF","Green", "#FF6633"),
                     labels=c("ERA5", "GLDAS", "MERRA-2", "Model mean")) +
  labs(x= expression(paste("(", m^3," ", m^-3, ")")),
       y= expression(paste('Latitude (',degree,')'))) +
  theme(legend.title = element_blank(), # 图例名称为空
        legend.position = c(0.68, 0.38),legend.justification = c("left","top"),
        legend.key.size = unit(1, "lines"), # 垂直组合放置时，改变上下间隔，同时线的长短；水平组合放置时，图例中线的长短
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.key = element_blank(),
        legend.text = element_text(size=12, family="Times New Roman"), # 图例标签字体大小
        legend.text.align = 0,  #图例标签左对齐
        axis.title.x=element_text(size=17, family="Times New Roman"), axis.title.y=element_text(size=17, family="Times New Roman"),
        axis.text.x=element_text(size=17, family="Times New Roman"), axis.text.y=element_text(size=17, family="Times New Roman"),
        plot.margin = unit(c(1,1,0.5,0.5),"lines") #( top, right, bottom, left )
        
  ) +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5),
                     labels = c("0","0.1","0.2","0.3","0.4","0.5")) +
  scale_y_continuous(breaks = seq(-60,80,20)) +
  theme(axis.line = element_line(color='black'), # 去掉背景grid lines
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

gg.rzsm


ggsave('F:/New PHD results/SM_variations_result_end/SM_latitude_gradient/Latitude_gradient_RZSM.pdf',
       dpi = 10000,
       height = 4, width = 5,
       device=cairo_pdf)

