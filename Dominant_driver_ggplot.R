## ggplot partial correlation sm and climates (ERA5, GLDAS, MERRA2, Model mean)
## 1: dominate drivers 
## 2: percentage of dominate drivers


library(raster)
library(grid)
library(gridExtra)
library(ggplot2)
library(RStoolbox)
library(grid)
library(latticeExtra)
library(RColorBrewer)
require(png)
library(ggpubr)
library(reshape2)
library(sysfonts)
library(showtext)
library(rgdal)



#font_import("timesi.ttf")
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
#font_add("Italic Times New Roman", "timesi.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()
#windowsFonts()


mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        # legend.position = "bottom",
        legend.position = c(0.5,-0.35),#"bottom",
        legend.direction = "horizontal",
        legend.key.width=unit(1.3,'cm'),legend.key.size=unit(0.25,'cm'), # 图例长度与高度
        legend.title = element_text(size=11, family="Times New Roman"),  # 图例名称字体大小
        legend.text = element_text(size = 11, family="Times New Roman"),
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.key = element_blank(), ## 图例背景去掉
        axis.text = element_text(size = 12, family="Times New Roman"),
        axis.title = element_text(size = 12, family="Times New Roman"),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 12,  family="Times New Roman"),
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




shpfile <- readOGR("E:/New PHD/Study data/Shp/Continent_boundary/Continent_boundary_polygon.shp") %>% fortify()


##load RGB legend
rgb.legend <- readPNG("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/legend_MAT_MAP_AET.png")

g_pic <- rasterGrob(rgb.legend, 
                    #height = 150, 
                    width = 0.75,
                    interpolate=TRUE)


###
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/")

ERA5.MAP.r <- raster("ERA5.RZSM.MAP.coef.tif") 
ERA5.MAT.r <- raster("ERA5.RZSM.MAT.coef.tif")
ERA5.AET.r <- raster("ERA5.RZSM.AET.coef.tif")

ERA5.r <- stack(ERA5.MAP.r, ERA5.MAT.r, ERA5.AET.r)


gg.ERA5.RGB <- ggRGB(ERA5.r, r = 1, g = 2, b = 3, scale = 5000000, maxpixels = 5000000,
                     stretch = "hist", 
                     ext = NULL, 
                     limits = NULL,
                     clipValues = "limits", 
                     ggObj = T,
                     ggLayer = F, alpha = 1, 
                     coord_equal = TRUE,
                     geom_raster = T, 
                     nullValue = 0)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  labs(y =NULL, title = "ERA5") +
  annotation_custom(g_pic, xmin=-180, xmax=-60, ymin=-70, ymax=-30)

gg.ERA5.RGB.sum <- gg.ERA5.RGB + mytheme + my_scale_x_continuous + my_scale_y_continuous
gg.ERA5.RGB.sum






###
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/")

GLDAS.MAP.r <- raster("GLDAS.RZSM.MAP.coef.tif") 
GLDAS.MAT.r <- raster("GLDAS.RZSM.MAT.coef.tif")
GLDAS.AET.r <- raster("GLDAS.RZSM.AET.coef.tif")

GLDAS.r <- stack(GLDAS.MAP.r, GLDAS.MAT.r, GLDAS.AET.r)


gg.GLDAS.RGB <- ggRGB(GLDAS.r, r = 1, g = 2, b = 3, scale = 5000000, maxpixels = 5000000,
                      stretch = "hist", 
                      ext = NULL, 
                      limits = NULL,
                      clipValues = "limits", 
                      ggObj = T,
                      ggLayer = F, alpha = 1, 
                      coord_equal = TRUE,
                      geom_raster = T, 
                      nullValue = 0)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  labs(y =NULL, title = "GLDAS") +
  annotation_custom(g_pic, xmin=-180, xmax=-60, ymin=-70, ymax=-30)

gg.GLDAS.RGB.sum <- gg.GLDAS.RGB + mytheme + my_scale_x_continuous + my_scale_y_continuous
gg.GLDAS.RGB.sum






#####
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/")

MERRA2.MAP.r <- raster("MERRA2.RZSM.MAP.coef.tif") 
MERRA2.MAT.r <- raster("MERRA2.RZSM.MAT.coef.tif")
MERRA2.AET.r <- raster("MERRA2.RZSM.AET.coef.tif")

MERRA2.r <- stack(MERRA2.MAP.r, MERRA2.MAT.r, MERRA2.AET.r)


gg.MERRA2.RGB <- ggRGB(MERRA2.r, r = 1, g = 2, b = 3, scale = 5000000, maxpixels = 5000000,
                       stretch = "hist", 
                       ext = NULL, 
                       limits = NULL,
                       clipValues = "limits", 
                       ggObj = T,
                       ggLayer = F, alpha = 1, 
                       coord_equal = TRUE,
                       geom_raster = T, 
                       nullValue = 0)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  labs(y =NULL, title = "MERRA-2") +
  annotation_custom(g_pic, xmin=-180, xmax=-60, ymin=-70, ymax=-30)

gg.MERRA2.RGB.sum <- gg.MERRA2.RGB + mytheme + my_scale_x_continuous + my_scale_y_continuous
gg.MERRA2.RGB.sum





#####
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/")

Model.mean.MAP.r <- raster("Model.mean.RZSM.MAP.coef.tif") 
Model.mean.MAT.r <- raster("Model.mean.RZSM.MAT.coef.tif")
Model.mean.AET.r <- raster("Model.mean.RZSM.AET.coef.tif")

Model.mean.r <- stack(Model.mean.MAP.r, Model.mean.MAT.r, Model.mean.AET.r)


gg.Model.mean.RGB <- ggRGB(Model.mean.r, r = 1, g = 2, b = 3, scale = 5000000, maxpixels = 5000000,
                           stretch = "hist", 
                           ext = NULL, 
                           limits = NULL,
                           clipValues = "limits", 
                           ggObj = T,
                           ggLayer = F, alpha = 1, 
                           coord_equal = TRUE,
                           geom_raster = T, 
                           nullValue = 0)+
  geom_polygon(data=subset(shpfile, lat > -60), aes(x = long, y = lat, group = group), 
               colour = "black", size = 0.000005, fill = "white", alpha = 0) +
  labs(y =NULL, title = "Model mean") +
  annotation_custom(g_pic, xmin=-180, xmax=-60, ymin=-70, ymax=-30)

gg.Model.mean.RGB.sum <- gg.Model.mean.RGB + mytheme + my_scale_x_continuous + my_scale_y_continuous
gg.Model.mean.RGB.sum




###
gg.sum <- ggarrange(gg.ERA5.RGB.sum, gg.GLDAS.RGB.sum, gg.MERRA2.RGB.sum, gg.Model.mean.RGB.sum,
                    ncol=2, nrow = 2,
                    labels = c("(a)", "(b)", "(c)", "(d)"),
                    font.label = list(size = 12, color = "black", face = "plain", family = "Times New Roman"),
                    hjust = -4.6, vjust = 2, align = "hv"
)
gg.sum

ggsave('F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/Partial_ERA5_GLDAS_MERRA2_Model.mean.pdf',
       dpi=10000,
       width = 8, height = 4)

##################################################################################################################











###### calculating percentage of dominate factors of global RZSM for Mean, ERA5, GLDAS, MERRA-2, respectively ----------------------------------------------------------------
###
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation")

ERA5.MAP.coef <- raster("ERA5.RZSM.MAP.coef.tif")
ERA5.MAT.coef <- raster("ERA5.RZSM.MAT.coef.tif")
ERA5.AET.coef <- raster("ERA5.RZSM.AET.coef.tif")

ERA5.sm.map.df <- as.data.frame(ERA5.MAP.coef, xy = T)
head(ERA5.sm.map.df)
nrow(ERA5.sm.map.df)

ERA5.sm.mat.df <- as.data.frame(ERA5.MAT.coef, xy = T)
head(ERA5.sm.mat.df)
nrow(ERA5.sm.mat.df)

ERA5.sm.aet.df <- as.data.frame(ERA5.AET.coef, xy = T)
head(ERA5.sm.aet.df)
nrow(ERA5.sm.aet.df)

##
area.df <- as.data.frame(area(ERA5.MAP.coef), xy = T)
head(area.df)
nrow(area.df)

ERA5.sm.df <- cbind(ERA5.sm.map.df, ERA5.sm.mat.df, ERA5.sm.aet.df, area.df) %>% na.omit
#nrow(ERA5.sm.df)
ERA5.sm.df.sub <- ERA5.sm.df[,c(1:3, 6, 9, 12)]
names(ERA5.sm.df.sub) <- c("Lon", "Lat", "ERA5.sm.MAP", "ERA5.sm.MAT", "ERA5.SM.AET", "area")


for(i in seq(nrow(ERA5.sm.df.sub))) {
  ERA5.sm.df.sub$sm.MAP.D[i] <- max(abs(ERA5.sm.df.sub[i, 3:5]))/abs(ERA5.sm.df.sub[i,3])
  ERA5.sm.df.sub$sm.MAT.D[i] <- max(abs(ERA5.sm.df.sub[i, 3:5]))/abs(ERA5.sm.df.sub[i,4])
  ERA5.sm.df.sub$SM.AET.D[i] <- max(abs(ERA5.sm.df.sub[i, 3:5]))/abs(ERA5.sm.df.sub[i,5])
}

### for precipitation dominance area percent of RZSM
ERA5.map.domi <- subset(ERA5.sm.df.sub, ERA5.sm.df.sub$sm.MAP.D == "1")
ERA5.MAP.p <- sum(ERA5.map.domi$area)/sum(ERA5.sm.df.sub$area)  #### 0.3626378
ERA5.MAP.p

### for temperature dominance area percent of sm
ERA5.mat.domi <- subset(ERA5.sm.df.sub, ERA5.sm.df.sub$sm.MAT.D == "1")
ERA5.MAT.p <- sum(ERA5.mat.domi$area)/sum(ERA5.sm.df.sub$area)  #### 0.1983615
ERA5.MAT.p

### for aet dominance area percent of sm
ERA5.aet.domi <- subset(ERA5.sm.df.sub, ERA5.sm.df.sub$SM.AET.D == "1")
ERA5.AET.p <- sum(ERA5.aet.domi$area)/sum(ERA5.sm.df.sub$area)  #### 0.4390007
ERA5.AET.p


ERA5.par.per.df <- c(ERA5.MAP.p*100, ERA5.MAT.p*100, ERA5.AET.p*100)  %>% as.data.frame() 
colnames(ERA5.par.per.df) <- "Percentage" 

climates.type <- c("MAP", "MAT", "AET") %>% factor(levels = c("MAP", "MAT", "AET"))
ERA5.par.per.df$climates.type <- climates.type
###############################33333




#####
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation")
GLDAS.MAP.coef <- raster("GLDAS.RZSM.MAP.coef.tif")
GLDAS.MAT.coef <- raster("GLDAS.RZSM.MAT.coef.tif")
GLDAS.AET.coef <- raster("GLDAS.RZSM.AET.coef.tif")

GLDAS.sm.map.df <- as.data.frame(GLDAS.MAP.coef, xy = T)
head(GLDAS.sm.map.df)
nrow(GLDAS.sm.map.df)

GLDAS.sm.mat.df <- as.data.frame(GLDAS.MAT.coef, xy = T)
head(GLDAS.sm.mat.df)
nrow(GLDAS.sm.mat.df)

GLDAS.sm.aet.df <- as.data.frame(GLDAS.AET.coef, xy = T)
head(GLDAS.sm.aet.df)
nrow(GLDAS.sm.aet.df)

##
area.df <- as.data.frame(area(GLDAS.MAP.coef), xy = T)
head(area.df)
nrow(area.df)

GLDAS.sm.df <- cbind(GLDAS.sm.map.df, GLDAS.sm.mat.df, GLDAS.sm.aet.df, area.df) %>% na.omit
#nrow(GLDAS.sm.df)
GLDAS.sm.df.sub <- GLDAS.sm.df[,c(1:3, 6, 9, 12)]
names(GLDAS.sm.df.sub) <- c("Lon", "Lat", "GLDAS.sm.MAP", "GLDAS.sm.MAT", "GLDAS.SM.AET", "area")


for(i in seq(nrow(GLDAS.sm.df.sub))) {
  GLDAS.sm.df.sub$sm.MAP.D[i] <- max(abs(GLDAS.sm.df.sub[i, 3:5]))/abs(GLDAS.sm.df.sub[i,3])
  GLDAS.sm.df.sub$sm.MAT.D[i] <- max(abs(GLDAS.sm.df.sub[i, 3:5]))/abs(GLDAS.sm.df.sub[i,4])
  GLDAS.sm.df.sub$SM.AET.D[i] <- max(abs(GLDAS.sm.df.sub[i, 3:5]))/abs(GLDAS.sm.df.sub[i,5])
}

### for precipitation dominance area percent of RZSM
GLDAS.map.domi <- subset(GLDAS.sm.df.sub, GLDAS.sm.df.sub$sm.MAP.D == "1")
GLDAS.MAP.p <- sum(GLDAS.map.domi$area)/sum(GLDAS.sm.df.sub$area)  #### 0.4254841
GLDAS.MAP.p

### for temperature dominance area percent of sm
GLDAS.mat.domi <- subset(GLDAS.sm.df.sub, GLDAS.sm.df.sub$sm.MAT.D == "1")
GLDAS.MAT.p <- sum(GLDAS.mat.domi$area)/sum(GLDAS.sm.df.sub$area)  #### 0.2850306
GLDAS.MAT.p

### for aet dominance area percent of sm
GLDAS.aet.domi <- subset(GLDAS.sm.df.sub, GLDAS.sm.df.sub$SM.AET.D == "1")
GLDAS.AET.p <- sum(GLDAS.aet.domi$area)/sum(GLDAS.sm.df.sub$area)  #### 0.2894853
GLDAS.AET.p


GLDAS.par.per.df <- c(GLDAS.MAP.p*100, GLDAS.MAT.p*100, GLDAS.AET.p*100)  %>% as.data.frame() 
colnames(GLDAS.par.per.df) <- "Percentage" 

climates.type <- c("MAP", "MAT", "AET") %>% factor(levels = c("MAP", "MAT", "AET"))
GLDAS.par.per.df$climates.type <- climates.type
##############################################333333




#####
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation")

MERRA.MAP.coef <- raster("MERRA2.RZSM.MAP.coef.tif")
MERRA.MAT.coef <- raster("MERRA2.RZSM.MAT.coef.tif")
MERRA.AET.coef <- raster("MERRA2.RZSM.AET.coef.tif")

MERRA.sm.map.df <- as.data.frame(MERRA.MAP.coef, xy = T)
head(MERRA.sm.map.df)
nrow(MERRA.sm.map.df)

MERRA.sm.mat.df <- as.data.frame(MERRA.MAT.coef, xy = T)
head(MERRA.sm.mat.df)
nrow(MERRA.sm.mat.df)

MERRA.sm.aet.df <- as.data.frame(MERRA.AET.coef, xy = T)
head(MERRA.sm.aet.df)
nrow(MERRA.sm.aet.df)

##
area.df <- as.data.frame(area(MERRA.MAP.coef), xy = T)
head(area.df)
nrow(area.df)

MERRA.sm.df <- cbind(MERRA.sm.map.df, MERRA.sm.mat.df, MERRA.sm.aet.df, area.df) %>% na.omit
#nrow(MERRA.sm.df)
MERRA.sm.df.sub <- MERRA.sm.df[,c(1:3, 6, 9, 12)]
names(MERRA.sm.df.sub) <- c("Lon", "Lat", "MERRA.sm.MAP", "MERRA.sm.MAT", "MERRA.SM.AET", "area")


for(i in seq(nrow(MERRA.sm.df.sub))) {
  MERRA.sm.df.sub$sm.MAP.D[i] <- max(abs(MERRA.sm.df.sub[i, 3:5]))/abs(MERRA.sm.df.sub[i,3])
  MERRA.sm.df.sub$sm.MAT.D[i] <- max(abs(MERRA.sm.df.sub[i, 3:5]))/abs(MERRA.sm.df.sub[i,4])
  MERRA.sm.df.sub$SM.AET.D[i] <- max(abs(MERRA.sm.df.sub[i, 3:5]))/abs(MERRA.sm.df.sub[i,5])
}

### for precipitation dominance area percent of RZSM
MERRA.map.domi <- subset(MERRA.sm.df.sub, MERRA.sm.df.sub$sm.MAP.D == "1")
MERRA.MAP.p <- sum(MERRA.map.domi$area)/sum(MERRA.sm.df.sub$area)  #### 0.4513018
MERRA.MAP.p

### for temperature dominance area percent of sm
MERRA.mat.domi <- subset(MERRA.sm.df.sub, MERRA.sm.df.sub$sm.MAT.D == "1")
MERRA.MAT.p <- sum(MERRA.mat.domi$area)/sum(MERRA.sm.df.sub$area)  #### 0.1975119
MERRA.MAT.p

### for aet dominance area percent of sm
MERRA.aet.domi <- subset(MERRA.sm.df.sub, MERRA.sm.df.sub$SM.AET.D == "1")
MERRA.AET.p <- sum(MERRA.aet.domi$area)/sum(MERRA.sm.df.sub$area)  #### 0.3511864
MERRA.AET.p


MERRA.par.per.df <- c(MERRA.MAP.p*100, MERRA.MAT.p*100, MERRA.AET.p*100) %>% as.data.frame() 
colnames(MERRA.par.per.df) <- "Percentage" 

climates.type <- c("MAP", "MAT", "AET") %>% factor(levels = c("MAP", "MAT", "AET"))
MERRA.par.per.df$climates.type <- climates.type
########################3




####
setwd("F:/New PHD results/SM_variations_result_end/SM_Partial_correlation")

Model.mean.MAP.coef <- raster("Model.mean.RZSM.MAP.coef.tif")
Model.mean.MAT.coef <- raster("Model.mean.RZSM.MAT.coef.tif")
Model.mean.AET.coef <- raster("Model.mean.RZSM.AET.coef.tif")


Mean.sm.map.df <- as.data.frame(Model.mean.MAP.coef, xy = T)
head(Mean.sm.map.df)
nrow(Mean.sm.map.df)

Mean.sm.mat.df <- as.data.frame(Model.mean.MAT.coef, xy = T)
head(Mean.sm.mat.df)
nrow(Mean.sm.mat.df)

Mean.sm.aet.df <- as.data.frame(Model.mean.AET.coef, xy = T)
head(Mean.sm.aet.df)
nrow(Mean.sm.aet.df)

##
area.df <- as.data.frame(area(Mean.coef.tif[[1]]), xy = T)
head(area.df)
nrow(area.df)

Mean.sm.df <- cbind(Mean.sm.map.df, Mean.sm.mat.df, Mean.sm.aet.df, area.df) %>% na.omit
#nrow(Mean.sm.df)
Mean.sm.df.sub <- Mean.sm.df[,c(1:3, 6, 9, 12)]
names(Mean.sm.df.sub) <- c("Lon", "Lat", "sm.MAP", "sm.MAT", "SM.AET", "area")


for(i in seq(nrow(Mean.sm.df.sub))) {
  Mean.sm.df.sub$sm.MAP.D[i] <- max(abs(Mean.sm.df.sub[i, 3:5]))/abs(Mean.sm.df.sub[i,3])
  Mean.sm.df.sub$sm.MAT.D[i] <- max(abs(Mean.sm.df.sub[i, 3:5]))/abs(Mean.sm.df.sub[i,4])
  Mean.sm.df.sub$SM.AET.D[i] <- max(abs(Mean.sm.df.sub[i, 3:5]))/abs(Mean.sm.df.sub[i,5])
  
}

### for precipitation dominance area percent of RZSM
Mean.map.domi <- subset(Mean.sm.df.sub, Mean.sm.df.sub$sm.MAP.D == "1")
Mean.MAP.p <- sum(Mean.map.domi$area)/sum(Mean.sm.df.sub$area)  #### 0.4233799
Mean.MAP.p

### for temperature dominance area percent of sm
Mean.mat.domi <- subset(Mean.sm.df.sub, Mean.sm.df.sub$sm.MAT.D == "1")
Mean.MAT.p <- sum(Mean.mat.domi$area)/sum(Mean.sm.df.sub$area)  #### 0.19355
Mean.MAT.p

### for aet dominance area percent of sm
Mean.aet.domi <- subset(Mean.sm.df.sub, Mean.sm.df.sub$SM.AET.D == "1")
Mean.AET.p <- sum(Mean.aet.domi$area)/sum(Mean.sm.df.sub$area)  #### 0.38307
Mean.AET.p


Mean.par.per.df <- c(Mean.MAP.p*100, Mean.MAT.p*100, Mean.AET.p*100) %>% as.data.frame() 
colnames(Mean.par.per.df) <- "Percentage" 

climates.type <- c("MAP", "MAT", "AET") %>% factor(levels = c("MAP", "MAT", "AET"))
Mean.par.per.df$climates.type <- climates.type
###############################################





###########
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()



mytheme <- theme_bw() +
  theme(#element_line(color='black'), 
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_blank(),
    axis.line.x.top  = element_blank(),
    panel.border  = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_text(size= 20, family="Times New Roman"),
    axis.text.y = element_text(size= 24, family="Times New Roman"),
    axis.text.x = element_text(size= 22, hjust =0.5, vjust =1, angle = 0, family="Times New Roman"),
    plot.margin = unit(c(1,1,0,1),"lines"),
    plot.title = element_text(size = 22, hjust = 0.5, vjust = 1,family="Times New Roman"),
    legend.position = c(0.72,0.45), legend.justification=c("left","bottom"),
    legend.background = element_rect(fill='transparent'),
    #legend.text = element_text(size= 16, family="SimSun"),
    legend.title = element_blank())

my_scale_x_discrete <-  scale_x_discrete(breaks=climates.type, 
                                         labels=expression("MAP", "MAT", "AET"))
my_scale_y_discrete <-  scale_y_continuous(limit=c(0,60), n.breaks = 6, expand=c(0,0))

my_fill_munual <-  scale_fill_manual(values = c("grey", "grey", "grey"),
                                     labels=c('MAP', 'MAT', 'AET'))

my_geom_text <- geom_text(aes(label=sprintf("%0.0f", round(Percentage, digits = 0))), colour="black", size=8,  hjust =0.5, vjust =-0.5, family="Times New Roman") 


###
gg.ERA5.domi <-  ggplot(data=ERA5.par.per.df, aes(x=climates.type, y=Percentage, fill=climates.type)) +
  geom_bar(stat="identity",show.legend = F, width = 0.3, color="black") +
  labs(x="", y=expression(paste("Dominant percentage (%)")), title = "ERA5") 


gg.ERA5.domi.sum <- gg.ERA5.domi + mytheme + my_scale_x_discrete + my_scale_y_discrete +
  my_fill_munual + my_geom_text
gg.ERA5.domi.sum





###
gg.GLDAS.domi <-  ggplot(data=GLDAS.par.per.df, aes(x=climates.type, y=Percentage, fill=climates.type)) +
  geom_bar(stat="identity",show.legend = F, width = 0.3, color="black") +
  labs(x="", y=expression(paste("Dominant percentage (%)")), title = "GLDAS") 


gg.GLDAS.domi.sum <- gg.GLDAS.domi + mytheme + my_scale_x_discrete + my_scale_y_discrete +
  my_fill_munual + my_geom_text
gg.GLDAS.domi.sum




###
gg.MERRA2.domi <-  ggplot(data=MERRA.par.per.df, aes(x=climates.type, y=Percentage, fill=climates.type)) +
  geom_bar(stat="identity",show.legend = F, width = 0.3, color="black") +
  labs(x="", y=expression(paste("Dominant percentage (%)")), title = "MERRA-2") 


gg.MERRA2.domi.sum <- gg.MERRA2.domi + mytheme + my_scale_x_discrete + my_scale_y_discrete +
  my_fill_munual + my_geom_text
gg.MERRA2.domi.sum




###
gg.Model.mean.domi <-  ggplot(data=Mean.par.per.df, aes(x=climates.type, y=Percentage, fill=climates.type)) +
  geom_bar(stat="identity",show.legend = F, width = 0.3, color="black") +
  labs(x="", y=expression(paste("Dominant percentage (%)")), title = "Model mean") 


gg.Model.mean.domi.sum <- gg.Model.mean.domi + mytheme + my_scale_x_discrete + my_scale_y_discrete +
  my_fill_munual + my_geom_text
gg.Model.mean.domi.sum



##
gg.sum <- ggarrange(gg.ERA5.domi.sum, gg.GLDAS.domi.sum, gg.MERRA2.domi.sum, gg.Model.mean.domi.sum,
                    ncol=4, nrow = 1,
                    labels = c("(a)", "(b)", "(c)", "(d)"),
                    font.label = list(size = 22, color = "black", face = "plain", family = "Times New Roman"),
                    hjust = -3, vjust = 2)

gg.sum

ggsave('F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/RZSM_dominant_driver_percentage.pdf',
       width = 16, height = 4,
       dpi = 10000)


