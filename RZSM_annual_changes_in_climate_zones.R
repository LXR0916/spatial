library(ggpubr)
library(reshape2)
library(sysfonts)
library(showtext)
library(grid)
library(dplyr)
library(ggpubr)
library(extrafont)
library(ggplot2)
library(raster)




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

### reclassify landcover----------------------
lc.re <- reclassify(lc.tif, c(1,5,1, 5,7,2, 7,9,3, 9,10,4, 10,11,5, 11,13,6, 13,14,7, 14,16,8)) 
# 1 Forest  ## cbind(1,2,3,4,5)
# 2 Shrublands ## cbind(6,7)
# 3 Savannas  ## cbind(8,9)
# 4 Grasslands ## cbind(10)
# 5 Wetlands ## cbind(11)
# 6 Croplands ## cbind(12,13)
# 7 urban ## cbind(14)
# 8 bare land  ## cbind(16)



######
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")

ERA5 <- brick("ERA5_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
GLDAS <- brick("GLDAS_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3
MERRA2 <- brick("MERRA2_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3

Model.mean.y <- MERRA2
Model.mean.y[] <- NA
for (i in 1:nlayers(Model.mean.y)) {
  Model.mean.y[[i]] <- calc(stack(ERA5[[i]], GLDAS[[i]], MERRA2[[i]]), function(x)mean(x))
}

spplot(Model.mean.y[[1]])



#### Climate zones-------------------------------------------
cz <- raster("E:/New PHD/Study data/climate zone classification/Beck_KG_V1/Reclassify_climate_zone/Reclass_CZ_5.tif")
#spplot(cz.reclass)
# 1 Tropical
# 2 Arid
# 3 Temperate
# 4 Cold
# 5 Polar
cz.m <- mask(cz, lc.tif[[30]], maskvalue = NA)
cz.m


#### CZ1: Tropical----------------------------------------
Tropical.zone <- cz.m
Tropical.zone[Tropical.zone[] == 2 | Tropical.zone[] == 3 | Tropical.zone[] == 4 | Tropical.zone[] == 5] <- NA

Tropical.w <- area(Tropical.zone, na.rm=T) / area(Tropical.zone, na.rm=T) %>% cellStats("sum") 

ERA5.Tropical <- mask(ERA5*Tropical.w, Tropical.zone, maskvalue=NA) %>% cellStats("sum") 
GLDAS.Tropical <- mask(GLDAS*Tropical.w, Tropical.zone, maskvalue=NA) %>% cellStats("sum") 
MERRA2.Tropical <- mask(MERRA2*Tropical.w, Tropical.zone, maskvalue=NA) %>% cellStats("sum") 
Model.mean.Tropical <- mask(Model.mean.y*Tropical.w, Tropical.zone, maskvalue=NA) %>% cellStats("sum")

Tropical.df <- cbind(ERA5.Tropical, GLDAS.Tropical, MERRA2.Tropical, Model.mean.Tropical) %>% as.data.frame() 
names(Tropical.df) <- c("ERA5.Tropical","GLDAS.Tropical","MERRA2.Tropical","Model.mean.Tropical")
Tropical.df$Year <- c(1981:2017)



#### CZ2: Arid----------------------------------------
Arid.zone <- cz.m
Arid.zone[Arid.zone[] == 1 | Arid.zone[] == 3 | Arid.zone[] == 4 | Arid.zone[] == 5] <- NA


Arid.w <- area(Arid.zone, na.rm=T) / area(Arid.zone, na.rm=T) %>% cellStats("sum") 

ERA5.Arid <- mask(ERA5*Arid.w, Arid.zone, maskvalue=NA) %>% cellStats("sum") 
GLDAS.Arid <- mask(GLDAS*Arid.w, Arid.zone, maskvalue=NA) %>% cellStats("sum") 
MERRA2.Arid <- mask(MERRA2*Arid.w, Arid.zone, maskvalue=NA) %>% cellStats("sum") 
Model.mean.Arid <- mask(Model.mean.y*Arid.w, Arid.zone, maskvalue=NA) %>% cellStats("sum")

Arid.df <- cbind(ERA5.Arid, GLDAS.Arid, MERRA2.Arid, Model.mean.Arid) %>% as.data.frame() 
names(Arid.df) <- c("ERA5.Arid","GLDAS.Arid","MERRA2.Arid","Model.mean.Arid")
Arid.df$Year <- c(1981:2017)


#### CZ3: Temperate----------------------------------------
Temperate.zone <- cz.m
Temperate.zone[Temperate.zone[] == 1 | Temperate.zone[] == 2 | Temperate.zone[] == 4 | Temperate.zone[] == 5] <- NA

Temperate.w <- area(Temperate.zone, na.rm=T) / area(Temperate.zone, na.rm=T) %>% cellStats("sum") 

ERA5.Temperate <- mask(ERA5*Temperate.w, Temperate.zone, maskvalue=NA) %>% cellStats("sum") 
GLDAS.Temperate <- mask(GLDAS*Temperate.w, Temperate.zone, maskvalue=NA) %>% cellStats("sum") 
MERRA2.Temperate <- mask(MERRA2*Temperate.w, Temperate.zone, maskvalue=NA) %>% cellStats("sum") 
Model.mean.Temperate <- mask(Model.mean.y*Temperate.w, Temperate.zone, maskvalue=NA) %>% cellStats("sum")

Temperate.df <- cbind(ERA5.Temperate, GLDAS.Temperate, MERRA2.Temperate, Model.mean.Temperate) %>% as.data.frame() 
names(Temperate.df) <- c("ERA5.Temperate","GLDAS.Temperate","MERRA2.Temperate","Model.mean.Temperate")
Temperate.df$Year <- c(1981:2017)




#### CZ4: Cold----------------------------------------
Cold.zone <- cz.m
Cold.zone[Cold.zone[] == 1 | Cold.zone[] == 2 | Cold.zone[] == 3 | Cold.zone[] == 5] <- NA

Cold.w <- area(Cold.zone, na.rm=T) / area(Cold.zone, na.rm=T) %>% cellStats("sum") 

ERA5.Cold <- mask(ERA5*Cold.w, Cold.zone, maskvalue=NA) %>% cellStats("sum") 
GLDAS.Cold <- mask(GLDAS*Cold.w, Cold.zone, maskvalue=NA) %>% cellStats("sum") 
MERRA2.Cold <- mask(MERRA2*Cold.w, Cold.zone, maskvalue=NA) %>% cellStats("sum") 
Model.mean.Cold <- mask(Model.mean.y*Cold.w, Cold.zone, maskvalue=NA) %>% cellStats("sum")

Cold.df <- cbind(ERA5.Cold, GLDAS.Cold, MERRA2.Cold, Model.mean.Cold) %>% as.data.frame() 
names(Cold.df) <- c("ERA5.Cold","GLDAS.Cold","MERRA2.Cold","Model.mean.Cold")
Cold.df$Year <- c(1981:2017)




#### CZ5: Polar----------------------------------------
Polar.zone <- cz.m
Polar.zone[Polar.zone[] == 1 | Polar.zone[] == 2 | Polar.zone[] == 3 | Polar.zone[] == 4] <- NA

Polar.w <- area(Polar.zone, na.rm=T) / area(Polar.zone, na.rm=T) %>% cellStats("sum") 

ERA5.Polar <- mask(ERA5*Polar.w, Polar.zone, maskvalue=NA) %>% cellStats("sum") 
GLDAS.Polar <- mask(GLDAS*Polar.w, Polar.zone, maskvalue=NA) %>% cellStats("sum") 
MERRA2.Polar <- mask(MERRA2*Polar.w, Polar.zone, maskvalue=NA) %>% cellStats("sum") 
Model.mean.Polar <- mask(Model.mean.y*Polar.w, Polar.zone, maskvalue=NA) %>% cellStats("sum")

Polar.df <- cbind(ERA5.Polar, GLDAS.Polar, MERRA2.Polar, Model.mean.Polar) %>% as.data.frame() 
names(Polar.df) <- c("ERA5.Polar","GLDAS.Polar","MERRA2.Polar","Model.mean.Polar")
Polar.df$Year <- c(1981:2017)



#### ggplot-----------------------------------------------------------------

#windowsFonts()
font_import("timesi.ttf")
font_paths() 
font.file <- font_files()
font_add("Times New Roman", "times.ttf")
font_add("Italic Times New Roman", "timesi.ttf")
font_add("SimSun", "simsun.ttc")

font_families()
showtext_auto()



mytheme <- theme_bw() +
  theme(axis.line = element_line(color='black'), # 去掉背景grid lines
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = NULL,
        legend.position = c(0.75, 0.2), # 从原点起 
        legend.direction = "vertical", # "vertical" "horizontal"，图例水平组合放置，或垂直组合放置
        legend.key.size = unit(1, "lines"), # 垂直组合放置时，改变上下间隔，同时线的长短；水平组合放置时，图例中线的长短
        legend.key.height = NULL, # 图例整体高度， unit(0.5, "cm"),                
        legend.key.width = NULL, # 图例整体宽度
        legend.background = element_rect(fill='transparent'), # 图例背景是透明
        legend.text.align = 0,
        legend.text = element_text(size=19, family = "Times New Roman"),
        axis.text.x=element_text(size=18, family = "Times New Roman"),
        axis.text.y=element_text(size=18, family = "Times New Roman"),
        axis.title.y=element_text(size=18, family = "Times New Roman"),
        plot.margin = unit(c(0.5,1,0.5,1),"lines") #( top, right, bottom, left )
        #plot.title = element_text(hjust = 0.5, vjust = 0, size = 21)
  )

lwr_line <- geom_line(aes(y = lwr), color = "red", linetype = "dashed")
upr_line <-  geom_line(aes(y = upr), color = "red", linetype = "dashed") 

my_geom_smooth <- geom_smooth(method = "lm", se=T, color="black") 

my_geom_point <-  geom_point(pch = 17, size=3) 

my_scale_x_continuous <- scale_x_continuous(limits=c(1980, 2017), breaks=seq(1980, 2017, 7), expand=c(0,0)) 


######## ggplot Tropical----------------------------------------------------------
ERA5.Tropical.list <- list(ERA5.Tropical.slope = sprintf('%0.5f', round(summary(lm(ERA5.Tropical ~ Year, data=Tropical.df[c("ERA5.Tropical", "Year")]))$coefficients[2,1],5), digits = 3),
                           ERA5.Tropical.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Tropical ~ Year, data=Tropical.df[c("ERA5.Tropical", "Year")]))$r.squared,3), digits = 3),
                           ERA5.Tropical.p = sprintf('%0.3f', round(summary(lm(ERA5.Tropical ~ Year, data=Tropical.df[c("ERA5.Tropical", "Year")]))$coefficients[2,4],3), digits = 3),
                           ERA5.Tropical.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Tropical ~ Year, data=Tropical.df[c("ERA5.Tropical", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Tropical.list

ERA5.Tropical.list.s <- substitute(atop(italic(y) == ERA5.Tropical.slope * x~ + ERA5.Tropical.intercept~"",
                                        italic(R) ^ 2~ '=' ~ERA5.Tropical.r2~""~italic(p)~"="~ERA5.Tropical.p), ERA5.Tropical.list)

ERA5.Tropical.list.s

ERA5.Tropical.pred.int <- predict(lm(ERA5.Tropical ~ Year, data = Tropical.df), interval = "confidence", level=0.95)
new.ERA5.Tropical.df <- cbind(Tropical.df["ERA5.Tropical"], ERA5.Tropical.pred.int)
new.ERA5.Tropical.df$Year <- c(1981:2017)

##
gg.ERA5.Tropical <- ggplot(new.ERA5.Tropical.df, aes(y=ERA5.Tropical, x=Year)) +
  geom_line(aes(y=ERA5.Tropical)) +
  scale_y_continuous(limits=c(0.33,0.365), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Tropical", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Tropical.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Tropical.sum <- gg.ERA5.Tropical + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Tropical.sum



#######
GLDAS.Tropical.list <- list(GLDAS.Tropical.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Tropical ~ Year, data=Tropical.df[c("GLDAS.Tropical", "Year")]))$coefficients[2,1],5), digits = 3),
                            GLDAS.Tropical.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Tropical ~ Year, data=Tropical.df[c("GLDAS.Tropical", "Year")]))$r.squared,3), digits = 3),
                            GLDAS.Tropical.p = sprintf('%0.3f', round(summary(lm(GLDAS.Tropical ~ Year, data=Tropical.df[c("GLDAS.Tropical", "Year")]))$coefficients[2,4],3), digits = 3),
                            GLDAS.Tropical.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Tropical ~ Year, data=Tropical.df[c("GLDAS.Tropical", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Tropical.list

GLDAS.Tropical.list.s <- substitute(atop(italic(y) == GLDAS.Tropical.slope * x~ + GLDAS.Tropical.intercept~"",
                                         italic(R) ^ 2~ '=' ~GLDAS.Tropical.r2~""~italic(p)~"="~GLDAS.Tropical.p), GLDAS.Tropical.list)

GLDAS.Tropical.list.s

GLDAS.Tropical.pred.int <- predict(lm(GLDAS.Tropical ~ Year, data = Tropical.df), interval = "confidence", level=0.95)
new.GLDAS.Tropical.df <- cbind(Tropical.df["GLDAS.Tropical"], GLDAS.Tropical.pred.int)
new.GLDAS.Tropical.df$Year <- c(1981:2017)

##
gg.GLDAS.Tropical <- ggplot(new.GLDAS.Tropical.df, aes(y=GLDAS.Tropical, x=Year)) +
  geom_line(aes(y=GLDAS.Tropical)) +
  scale_y_continuous(limits=c(0.12,0.145), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Tropical", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Tropical.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Tropical.sum <- gg.GLDAS.Tropical + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Tropical.sum




####
MERRA2.Tropical.list <- list(MERRA2.Tropical.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Tropical ~ Year, data=Tropical.df[c("MERRA2.Tropical", "Year")]))$coefficients[2,1],5), digits = 3),
                             MERRA2.Tropical.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Tropical ~ Year, data=Tropical.df[c("MERRA2.Tropical", "Year")]))$r.squared,3), digits = 3),
                             MERRA2.Tropical.p = sprintf('%0.3f', round(summary(lm(MERRA2.Tropical ~ Year, data=Tropical.df[c("MERRA2.Tropical", "Year")]))$coefficients[2,4],3), digits = 3),
                             MERRA2.Tropical.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Tropical ~ Year, data=Tropical.df[c("MERRA2.Tropical", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Tropical.list

MERRA2.Tropical.list.s <- substitute(atop(italic(y) == MERRA2.Tropical.slope * x~ + MERRA2.Tropical.intercept~"",
                                          italic(R) ^ 2~ '=' ~MERRA2.Tropical.r2~""~italic(p)~"="~MERRA2.Tropical.p), MERRA2.Tropical.list)

MERRA2.Tropical.list.s

MERRA2.Tropical.pred.int <- predict(lm(MERRA2.Tropical ~ Year, data = Tropical.df), interval = "confidence", level=0.95)
new.MERRA2.Tropical.df <- cbind(Tropical.df["MERRA2.Tropical"], MERRA2.Tropical.pred.int)
new.MERRA2.Tropical.df$Year <- c(1981:2017)

##
gg.MERRA2.Tropical <- ggplot(new.MERRA2.Tropical.df, aes(y=MERRA2.Tropical, x=Year)) +
  geom_line(aes(y=MERRA2.Tropical)) +
  scale_y_continuous(limits=c(0.26,0.31), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Tropical", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Tropical.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Tropical.sum <- gg.MERRA2.Tropical + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Tropical.sum



####
Model.mean.Tropical.list <- list(Model.mean.Tropical.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Tropical ~ Year, data=Tropical.df[c("Model.mean.Tropical", "Year")]))$coefficients[2,1],5), digits = 3),
                                 Model.mean.Tropical.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Tropical ~ Year, data=Tropical.df[c("Model.mean.Tropical", "Year")]))$r.squared,3), digits = 3),
                                 Model.mean.Tropical.p = sprintf('%0.3f', round(summary(lm(Model.mean.Tropical ~ Year, data=Tropical.df[c("Model.mean.Tropical", "Year")]))$coefficients[2,4],3), digits = 3),
                                 Model.mean.Tropical.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Tropical ~ Year, data=Tropical.df[c("Model.mean.Tropical", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Tropical.list

Model.mean.Tropical.list.s <- substitute(atop(italic(y) == Model.mean.Tropical.slope * x~ + Model.mean.Tropical.intercept~"",
                                              italic(R) ^ 2~ '=' ~Model.mean.Tropical.r2~""~italic(p)~"="~Model.mean.Tropical.p), Model.mean.Tropical.list)

Model.mean.Tropical.list.s

Model.mean.Tropical.pred.int <- predict(lm(Model.mean.Tropical ~ Year, data = Tropical.df), interval = "confidence", level=0.95)
new.Model.mean.Tropical.df <- cbind(Tropical.df["Model.mean.Tropical"], Model.mean.Tropical.pred.int)
new.Model.mean.Tropical.df$Year <- c(1981:2017)

##
gg.Model.mean.Tropical <- ggplot(new.Model.mean.Tropical.df, aes(y=Model.mean.Tropical, x=Year)) +
  geom_line(aes(y=Model.mean.Tropical)) +
  scale_y_continuous(limits=c(0.24,0.27), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Tropical", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Tropical.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Tropical.sum <- gg.Model.mean.Tropical + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Tropical.sum


####
gg.Tropical.sum <- ggarrange(gg.ERA5.Tropical.sum, gg.GLDAS.Tropical.sum,
                             gg.MERRA2.Tropical.sum, gg.Model.mean.Tropical.sum,
                             ncol = 4, nrow = 1)
gg.Tropical.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Tropical.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################




######## ggplot Arid----------------------------------------------------------
ERA5.Arid.list <- list(ERA5.Arid.slope = sprintf('%0.5f', round(summary(lm(ERA5.Arid ~ Year, data=Arid.df[c("ERA5.Arid", "Year")]))$coefficients[2,1],5), digits = 3),
                       ERA5.Arid.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Arid ~ Year, data=Arid.df[c("ERA5.Arid", "Year")]))$r.squared,3), digits = 3),
                       ERA5.Arid.p = sprintf('%0.3f', round(summary(lm(ERA5.Arid ~ Year, data=Arid.df[c("ERA5.Arid", "Year")]))$coefficients[2,4],3), digits = 3),
                       ERA5.Arid.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Arid ~ Year, data=Arid.df[c("ERA5.Arid", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Arid.list

ERA5.Arid.list.s <- substitute(atop(italic(y) == ERA5.Arid.slope * x~ + ERA5.Arid.intercept~"",
                                    italic(R) ^ 2~ '=' ~ERA5.Arid.r2~""~italic(p)~"="~ERA5.Arid.p), ERA5.Arid.list)

ERA5.Arid.list.s

ERA5.Arid.pred.int <- predict(lm(ERA5.Arid ~ Year, data = Arid.df), interval = "confidence", level=0.95)
new.ERA5.Arid.df <- cbind(Arid.df["ERA5.Arid"], ERA5.Arid.pred.int)
new.ERA5.Arid.df$Year <- c(1981:2017)

##
gg.ERA5.Arid <- ggplot(new.ERA5.Arid.df, aes(y=ERA5.Arid, x=Year)) +
  geom_line(aes(y=ERA5.Arid)) +
  scale_y_continuous(limits=c(0.13,0.16), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Arid", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Arid.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Arid.sum <- gg.ERA5.Arid + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Arid.sum



#######
GLDAS.Arid.list <- list(GLDAS.Arid.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Arid ~ Year, data=Arid.df[c("GLDAS.Arid", "Year")]))$coefficients[2,1],5), digits = 3),
                        GLDAS.Arid.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Arid ~ Year, data=Arid.df[c("GLDAS.Arid", "Year")]))$r.squared,3), digits = 3),
                        GLDAS.Arid.p = sprintf('%0.3f', round(summary(lm(GLDAS.Arid ~ Year, data=Arid.df[c("GLDAS.Arid", "Year")]))$coefficients[2,4],3), digits = 3),
                        GLDAS.Arid.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Arid ~ Year, data=Arid.df[c("GLDAS.Arid", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Arid.list

GLDAS.Arid.list.s <- substitute(atop(italic(y) == GLDAS.Arid.slope * x~  GLDAS.Arid.intercept~"",
                                     italic(R) ^ 2~ '=' ~GLDAS.Arid.r2~""~italic(p)~"="~GLDAS.Arid.p), GLDAS.Arid.list)

GLDAS.Arid.list.s

GLDAS.Arid.pred.int <- predict(lm(GLDAS.Arid ~ Year, data = Arid.df), interval = "confidence", level=0.95)
new.GLDAS.Arid.df <- cbind(Arid.df["GLDAS.Arid"], GLDAS.Arid.pred.int)
new.GLDAS.Arid.df$Year <- c(1981:2017)

##
gg.GLDAS.Arid <- ggplot(new.GLDAS.Arid.df, aes(y=GLDAS.Arid, x=Year)) +
  geom_line(aes(y=GLDAS.Arid)) +
  scale_y_continuous(limits=c(0.07,0.09), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Arid", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Arid.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Arid.sum <- gg.GLDAS.Arid + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Arid.sum




####
MERRA2.Arid.list <- list(MERRA2.Arid.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Arid ~ Year, data=Arid.df[c("MERRA2.Arid", "Year")]))$coefficients[2,1],5), digits = 3),
                         MERRA2.Arid.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Arid ~ Year, data=Arid.df[c("MERRA2.Arid", "Year")]))$r.squared,3), digits = 3),
                         MERRA2.Arid.p = sprintf('%0.3f', round(summary(lm(MERRA2.Arid ~ Year, data=Arid.df[c("MERRA2.Arid", "Year")]))$coefficients[2,4],3), digits = 3),
                         MERRA2.Arid.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Arid ~ Year, data=Arid.df[c("MERRA2.Arid", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Arid.list

MERRA2.Arid.list.s <- substitute(atop(italic(y) == MERRA2.Arid.slope * x~ + MERRA2.Arid.intercept~"",
                                      italic(R) ^ 2~ '=' ~MERRA2.Arid.r2~""~italic(p)~"="~MERRA2.Arid.p), MERRA2.Arid.list)

MERRA2.Arid.list.s

MERRA2.Arid.pred.int <- predict(lm(MERRA2.Arid ~ Year, data = Arid.df), interval = "confidence", level=0.95)
new.MERRA2.Arid.df <- cbind(Arid.df["MERRA2.Arid"], MERRA2.Arid.pred.int)
new.MERRA2.Arid.df$Year <- c(1981:2017)

##
gg.MERRA2.Arid <- ggplot(new.MERRA2.Arid.df, aes(y=MERRA2.Arid, x=Year)) +
  geom_line(aes(y=MERRA2.Arid)) +
  scale_y_continuous(limits=c(0.15,0.17), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Arid", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Arid.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Arid.sum <- gg.MERRA2.Arid + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Arid.sum



####
Model.mean.Arid.list <- list(Model.mean.Arid.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Arid ~ Year, data=Arid.df[c("Model.mean.Arid", "Year")]))$coefficients[2,1],5), digits = 3),
                             Model.mean.Arid.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Arid ~ Year, data=Arid.df[c("Model.mean.Arid", "Year")]))$r.squared,3), digits = 3),
                             Model.mean.Arid.p = sprintf('%0.3f', round(summary(lm(Model.mean.Arid ~ Year, data=Arid.df[c("Model.mean.Arid", "Year")]))$coefficients[2,4],3), digits = 3),
                             Model.mean.Arid.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Arid ~ Year, data=Arid.df[c("Model.mean.Arid", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Arid.list

Model.mean.Arid.list.s <- substitute(atop(italic(y) == Model.mean.Arid.slope * x~ + Model.mean.Arid.intercept~"",
                                          italic(R) ^ 2~ '=' ~Model.mean.Arid.r2~""~italic(p)~"="~Model.mean.Arid.p), Model.mean.Arid.list)

Model.mean.Arid.list.s

Model.mean.Arid.pred.int <- predict(lm(Model.mean.Arid ~ Year, data = Arid.df), interval = "confidence", level=0.95)
new.Model.mean.Arid.df <- cbind(Arid.df["Model.mean.Arid"], Model.mean.Arid.pred.int)
new.Model.mean.Arid.df$Year <- c(1981:2017)

##
gg.Model.mean.Arid <- ggplot(new.Model.mean.Arid.df, aes(y=Model.mean.Arid, x=Year)) +
  geom_line(aes(y=Model.mean.Arid)) +
  scale_y_continuous(limits=c(0.12,0.14), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Arid", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Arid.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Arid.sum <- gg.Model.mean.Arid + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Arid.sum


####
gg.Arid.sum <- ggarrange(gg.ERA5.Arid.sum, gg.GLDAS.Arid.sum,
                         gg.MERRA2.Arid.sum, gg.Model.mean.Arid.sum,
                         ncol = 4, nrow = 1)
gg.Arid.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Arid.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################




######## ggplot Temperate----------------------------------------------------------
ERA5.Temperate.list <- list(ERA5.Temperate.slope = sprintf('%0.5f', round(summary(lm(ERA5.Temperate ~ Year, data=Temperate.df[c("ERA5.Temperate", "Year")]))$coefficients[2,1],5), digits = 3),
                            ERA5.Temperate.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Temperate ~ Year, data=Temperate.df[c("ERA5.Temperate", "Year")]))$r.squared,3), digits = 3),
                            ERA5.Temperate.p = sprintf('%0.3f', round(summary(lm(ERA5.Temperate ~ Year, data=Temperate.df[c("ERA5.Temperate", "Year")]))$coefficients[2,4],3), digits = 3),
                            ERA5.Temperate.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Temperate ~ Year, data=Temperate.df[c("ERA5.Temperate", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Temperate.list

ERA5.Temperate.list.s <- substitute(atop(italic(y) == ERA5.Temperate.slope * x~ + ERA5.Temperate.intercept~"",
                                         italic(R) ^ 2~ '=' ~ERA5.Temperate.r2~""~italic(p)~"="~ERA5.Temperate.p), ERA5.Temperate.list)

ERA5.Temperate.list.s

ERA5.Temperate.pred.int <- predict(lm(ERA5.Temperate ~ Year, data = Temperate.df), interval = "confidence", level=0.95)
new.ERA5.Temperate.df <- cbind(Temperate.df["ERA5.Temperate"], ERA5.Temperate.pred.int)
new.ERA5.Temperate.df$Year <- c(1981:2017)

##
gg.ERA5.Temperate <- ggplot(new.ERA5.Temperate.df, aes(y=ERA5.Temperate, x=Year)) +
  geom_line(aes(y=ERA5.Temperate)) +
  scale_y_continuous(limits=c(0.31,0.35), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Temperate", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Temperate.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Temperate.sum <- gg.ERA5.Temperate + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Temperate.sum



#######
GLDAS.Temperate.list <- list(GLDAS.Temperate.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Temperate ~ Year, data=Temperate.df[c("GLDAS.Temperate", "Year")]))$coefficients[2,1],5), digits = 3),
                             GLDAS.Temperate.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Temperate ~ Year, data=Temperate.df[c("GLDAS.Temperate", "Year")]))$r.squared,3), digits = 3),
                             GLDAS.Temperate.p = sprintf('%0.3f', round(summary(lm(GLDAS.Temperate ~ Year, data=Temperate.df[c("GLDAS.Temperate", "Year")]))$coefficients[2,4],3), digits = 3),
                             GLDAS.Temperate.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Temperate ~ Year, data=Temperate.df[c("GLDAS.Temperate", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Temperate.list

GLDAS.Temperate.list.s <- substitute(atop(italic(y) == GLDAS.Temperate.slope * x~ + GLDAS.Temperate.intercept~"",
                                          italic(R) ^ 2~ '=' ~GLDAS.Temperate.r2~""~italic(p)~"="~GLDAS.Temperate.p), GLDAS.Temperate.list)

GLDAS.Temperate.list.s

GLDAS.Temperate.pred.int <- predict(lm(GLDAS.Temperate ~ Year, data = Temperate.df), interval = "confidence", level=0.95)
new.GLDAS.Temperate.df <- cbind(Temperate.df["GLDAS.Temperate"], GLDAS.Temperate.pred.int)
new.GLDAS.Temperate.df$Year <- c(1981:2017)

##
gg.GLDAS.Temperate <- ggplot(new.GLDAS.Temperate.df, aes(y=GLDAS.Temperate, x=Year)) +
  geom_line(aes(y=GLDAS.Temperate)) +
  scale_y_continuous(limits=c(0.11,0.13), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Temperate", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Temperate.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Temperate.sum <- gg.GLDAS.Temperate + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Temperate.sum




####
MERRA2.Temperate.list <- list(MERRA2.Temperate.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Temperate ~ Year, data=Temperate.df[c("MERRA2.Temperate", "Year")]))$coefficients[2,1],5), digits = 3),
                              MERRA2.Temperate.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Temperate ~ Year, data=Temperate.df[c("MERRA2.Temperate", "Year")]))$r.squared,3), digits = 3),
                              MERRA2.Temperate.p = sprintf('%0.3f', round(summary(lm(MERRA2.Temperate ~ Year, data=Temperate.df[c("MERRA2.Temperate", "Year")]))$coefficients[2,4],3), digits = 3),
                              MERRA2.Temperate.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Temperate ~ Year, data=Temperate.df[c("MERRA2.Temperate", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Temperate.list

MERRA2.Temperate.list.s <- substitute(atop(italic(y) == MERRA2.Temperate.slope * x~ + MERRA2.Temperate.intercept~"",
                                           italic(R) ^ 2~ '=' ~MERRA2.Temperate.r2~""~italic(p)~"="~MERRA2.Temperate.p), MERRA2.Temperate.list)

MERRA2.Temperate.list.s

MERRA2.Temperate.pred.int <- predict(lm(MERRA2.Temperate ~ Year, data = Temperate.df), interval = "confidence", level=0.95)
new.MERRA2.Temperate.df <- cbind(Temperate.df["MERRA2.Temperate"], MERRA2.Temperate.pred.int)
new.MERRA2.Temperate.df$Year <- c(1981:2017)

##
gg.MERRA2.Temperate <- ggplot(new.MERRA2.Temperate.df, aes(y=MERRA2.Temperate, x=Year)) +
  geom_line(aes(y=MERRA2.Temperate)) +
  scale_y_continuous(limits=c(0.255,0.29), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Temperate", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Temperate.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Temperate.sum <- gg.MERRA2.Temperate + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Temperate.sum



####
Model.mean.Temperate.list <- list(Model.mean.Temperate.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Temperate ~ Year, data=Temperate.df[c("Model.mean.Temperate", "Year")]))$coefficients[2,1],5), digits = 3),
                                  Model.mean.Temperate.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Temperate ~ Year, data=Temperate.df[c("Model.mean.Temperate", "Year")]))$r.squared,3), digits = 3),
                                  Model.mean.Temperate.p = sprintf('%0.3f', round(summary(lm(Model.mean.Temperate ~ Year, data=Temperate.df[c("Model.mean.Temperate", "Year")]))$coefficients[2,4],3), digits = 3),
                                  Model.mean.Temperate.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Temperate ~ Year, data=Temperate.df[c("Model.mean.Temperate", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Temperate.list

Model.mean.Temperate.list.s <- substitute(atop(italic(y) == Model.mean.Temperate.slope * x~ + Model.mean.Temperate.intercept~"",
                                               italic(R) ^ 2~ '=' ~Model.mean.Temperate.r2~""~italic(p)~"="~Model.mean.Temperate.p), Model.mean.Temperate.list)

Model.mean.Temperate.list.s

Model.mean.Temperate.pred.int <- predict(lm(Model.mean.Temperate ~ Year, data = Temperate.df), interval = "confidence", level=0.95)
new.Model.mean.Temperate.df <- cbind(Temperate.df["Model.mean.Temperate"], Model.mean.Temperate.pred.int)
new.Model.mean.Temperate.df$Year <- c(1981:2017)

##
gg.Model.mean.Temperate <- ggplot(new.Model.mean.Temperate.df, aes(y=Model.mean.Temperate, x=Year)) +
  geom_line(aes(y=Model.mean.Temperate)) +
  scale_y_continuous(limits=c(0.23,0.25), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Temperate", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Temperate.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Temperate.sum <- gg.Model.mean.Temperate + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Temperate.sum


####
gg.Temperate.sum <- ggarrange(gg.ERA5.Temperate.sum, gg.GLDAS.Temperate.sum,
                              gg.MERRA2.Temperate.sum, gg.Model.mean.Temperate.sum,
                              ncol = 4, nrow = 1)
gg.Temperate.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Temperate.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################




######## ggplot Cold----------------------------------------------------------
ERA5.Cold.list <- list(ERA5.Cold.slope = sprintf('%0.5f', round(summary(lm(ERA5.Cold ~ Year, data=Cold.df[c("ERA5.Cold", "Year")]))$coefficients[2,1],5), digits = 3),
                       ERA5.Cold.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Cold ~ Year, data=Cold.df[c("ERA5.Cold", "Year")]))$r.squared,3), digits = 3),
                       ERA5.Cold.p = sprintf('%0.3f', round(summary(lm(ERA5.Cold ~ Year, data=Cold.df[c("ERA5.Cold", "Year")]))$coefficients[2,4],3), digits = 3),
                       ERA5.Cold.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Cold ~ Year, data=Cold.df[c("ERA5.Cold", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Cold.list

ERA5.Cold.list.s <- substitute(atop(italic(y) == ERA5.Cold.slope * x~ + ERA5.Cold.intercept~"",
                                    italic(R) ^ 2~ '=' ~ERA5.Cold.r2~""~italic(p)~"="~ERA5.Cold.p), ERA5.Cold.list)

ERA5.Cold.list.s

ERA5.Cold.pred.int <- predict(lm(ERA5.Cold ~ Year, data = Cold.df), interval = "confidence", level=0.95)
new.ERA5.Cold.df <- cbind(Cold.df["ERA5.Cold"], ERA5.Cold.pred.int)
new.ERA5.Cold.df$Year <- c(1981:2017)

##
gg.ERA5.Cold <- ggplot(new.ERA5.Cold.df, aes(y=ERA5.Cold, x=Year)) +
  geom_line(aes(y=ERA5.Cold)) +
  scale_y_continuous(limits=c(0.32,0.345), n.breaks=3) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Cold", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Cold.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Cold.sum <- gg.ERA5.Cold + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Cold.sum



#######
GLDAS.Cold.list <- list(GLDAS.Cold.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Cold ~ Year, data=Cold.df[c("GLDAS.Cold", "Year")]))$coefficients[2,1],5), digits = 3),
                        GLDAS.Cold.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Cold ~ Year, data=Cold.df[c("GLDAS.Cold", "Year")]))$r.squared,3), digits = 3),
                        GLDAS.Cold.p = sprintf('%0.3f', round(summary(lm(GLDAS.Cold ~ Year, data=Cold.df[c("GLDAS.Cold", "Year")]))$coefficients[2,4],3), digits = 3),
                        GLDAS.Cold.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Cold ~ Year, data=Cold.df[c("GLDAS.Cold", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Cold.list

GLDAS.Cold.list.s <- substitute(atop(italic(y) == GLDAS.Cold.slope * x~ + GLDAS.Cold.intercept~"",
                                     italic(R) ^ 2~ '=' ~GLDAS.Cold.r2~""~italic(p)~"="~GLDAS.Cold.p), GLDAS.Cold.list)

GLDAS.Cold.list.s

GLDAS.Cold.pred.int <- predict(lm(GLDAS.Cold ~ Year, data = Cold.df), interval = "confidence", level=0.95)
new.GLDAS.Cold.df <- cbind(Cold.df["GLDAS.Cold"], GLDAS.Cold.pred.int)
new.GLDAS.Cold.df$Year <- c(1981:2017)

##
gg.GLDAS.Cold <- ggplot(new.GLDAS.Cold.df, aes(y=GLDAS.Cold, x=Year)) +
  geom_line(aes(y=GLDAS.Cold)) +
  scale_y_continuous(limits=c(0.10,0.12), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Cold", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Cold.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Cold.sum <- gg.GLDAS.Cold + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Cold.sum




####
MERRA2.Cold.list <- list(MERRA2.Cold.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Cold ~ Year, data=Cold.df[c("MERRA2.Cold", "Year")]))$coefficients[2,1],5), digits = 3),
                         MERRA2.Cold.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Cold ~ Year, data=Cold.df[c("MERRA2.Cold", "Year")]))$r.squared,3), digits = 3),
                         MERRA2.Cold.p = sprintf('%0.3f', round(summary(lm(MERRA2.Cold ~ Year, data=Cold.df[c("MERRA2.Cold", "Year")]))$coefficients[2,4],3), digits = 3),
                         MERRA2.Cold.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Cold ~ Year, data=Cold.df[c("MERRA2.Cold", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Cold.list

MERRA2.Cold.list.s <- substitute(atop(italic(y) == MERRA2.Cold.slope * x~ + MERRA2.Cold.intercept~"",
                                      italic(R) ^ 2~ '=' ~MERRA2.Cold.r2~""~italic(p)~"="~MERRA2.Cold.p), MERRA2.Cold.list)

MERRA2.Cold.list.s

MERRA2.Cold.pred.int <- predict(lm(MERRA2.Cold ~ Year, data = Cold.df), interval = "confidence", level=0.95)
new.MERRA2.Cold.df <- cbind(Cold.df["MERRA2.Cold"], MERRA2.Cold.pred.int)
new.MERRA2.Cold.df$Year <- c(1981:2017)

##
gg.MERRA2.Cold <- ggplot(new.MERRA2.Cold.df, aes(y=MERRA2.Cold, x=Year)) +
  geom_line(aes(y=MERRA2.Cold)) +
  scale_y_continuous(limits=c(0.26,0.305), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Cold", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Cold.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Cold.sum <- gg.MERRA2.Cold + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Cold.sum



####
Model.mean.Cold.list <- list(Model.mean.Cold.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Cold ~ Year, data=Cold.df[c("Model.mean.Cold", "Year")]))$coefficients[2,1],5), digits = 3),
                             Model.mean.Cold.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Cold ~ Year, data=Cold.df[c("Model.mean.Cold", "Year")]))$r.squared,3), digits = 3),
                             Model.mean.Cold.p = sprintf('%0.3f', round(summary(lm(Model.mean.Cold ~ Year, data=Cold.df[c("Model.mean.Cold", "Year")]))$coefficients[2,4],3), digits = 3),
                             Model.mean.Cold.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Cold ~ Year, data=Cold.df[c("Model.mean.Cold", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Cold.list

Model.mean.Cold.list.s <- substitute(atop(italic(y) == Model.mean.Cold.slope * x~ + Model.mean.Cold.intercept~"",
                                          italic(R) ^ 2~ '=' ~Model.mean.Cold.r2~""~italic(p)~"="~Model.mean.Cold.p), Model.mean.Cold.list)

Model.mean.Cold.list.s

Model.mean.Cold.pred.int <- predict(lm(Model.mean.Cold ~ Year, data = Cold.df), interval = "confidence", level=0.95)
new.Model.mean.Cold.df <- cbind(Cold.df["Model.mean.Cold"], Model.mean.Cold.pred.int)
new.Model.mean.Cold.df$Year <- c(1981:2017)

##
gg.Model.mean.Cold <- ggplot(new.Model.mean.Cold.df, aes(y=Model.mean.Cold, x=Year)) +
  geom_line(aes(y=Model.mean.Cold)) +
  scale_y_continuous(limits=c(0.23,0.255), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Cold", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Cold.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Cold.sum <- gg.Model.mean.Cold + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Cold.sum


####
gg.Cold.sum <- ggarrange(gg.ERA5.Cold.sum, gg.GLDAS.Cold.sum,
                         gg.MERRA2.Cold.sum, gg.Model.mean.Cold.sum,
                         ncol = 4, nrow = 1)
gg.Cold.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Cold.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################



######## ggplot Polar----------------------------------------------------------
ERA5.Polar.list <- list(ERA5.Polar.slope = sprintf('%0.5f', round(summary(lm(ERA5.Polar ~ Year, data=Polar.df[c("ERA5.Polar", "Year")]))$coefficients[2,1],5), digits = 3),
                        ERA5.Polar.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Polar ~ Year, data=Polar.df[c("ERA5.Polar", "Year")]))$r.squared,3), digits = 3),
                        ERA5.Polar.p = sprintf('%0.3f', round(summary(lm(ERA5.Polar ~ Year, data=Polar.df[c("ERA5.Polar", "Year")]))$coefficients[2,4],3), digits = 3),
                        ERA5.Polar.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Polar ~ Year, data=Polar.df[c("ERA5.Polar", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Polar.list

ERA5.Polar.list.s <- substitute(atop(italic(y) == ERA5.Polar.slope * x~  ERA5.Polar.intercept~"",
                                     italic(R) ^ 2~ '=' ~ERA5.Polar.r2~""~italic(p)~"="~ERA5.Polar.p), ERA5.Polar.list)

ERA5.Polar.list.s

ERA5.Polar.pred.int <- predict(lm(ERA5.Polar ~ Year, data = Polar.df), interval = "confidence", level=0.95)
new.ERA5.Polar.df <- cbind(Polar.df["ERA5.Polar"], ERA5.Polar.pred.int)
new.ERA5.Polar.df$Year <- c(1981:2017)

##
gg.ERA5.Polar <- ggplot(new.ERA5.Polar.df, aes(y=ERA5.Polar, x=Year)) +
  geom_line(aes(y=ERA5.Polar)) +
  scale_y_continuous(limits=c(0.29,0.31), n.breaks=4) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Polar", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Polar.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Polar.sum <- gg.ERA5.Polar + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Polar.sum



#######
GLDAS.Polar.list <- list(GLDAS.Polar.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Polar ~ Year, data=Polar.df[c("GLDAS.Polar", "Year")]))$coefficients[2,1],5), digits = 3),
                         GLDAS.Polar.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Polar ~ Year, data=Polar.df[c("GLDAS.Polar", "Year")]))$r.squared,3), digits = 3),
                         GLDAS.Polar.p = sprintf('%0.3f', round(summary(lm(GLDAS.Polar ~ Year, data=Polar.df[c("GLDAS.Polar", "Year")]))$coefficients[2,4],3), digits = 3),
                         GLDAS.Polar.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Polar ~ Year, data=Polar.df[c("GLDAS.Polar", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Polar.list

GLDAS.Polar.list.s <- substitute(atop(italic(y) == GLDAS.Polar.slope * x~  GLDAS.Polar.intercept~"",
                                      italic(R) ^ 2~ '=' ~GLDAS.Polar.r2~""~italic(p)~"="~GLDAS.Polar.p), GLDAS.Polar.list)

GLDAS.Polar.list.s

GLDAS.Polar.pred.int <- predict(lm(GLDAS.Polar ~ Year, data = Polar.df), interval = "confidence", level=0.95)
new.GLDAS.Polar.df <- cbind(Polar.df["GLDAS.Polar"], GLDAS.Polar.pred.int)
new.GLDAS.Polar.df$Year <- c(1981:2017)

##
gg.GLDAS.Polar <- ggplot(new.GLDAS.Polar.df, aes(y=GLDAS.Polar, x=Year)) +
  geom_line(aes(y=GLDAS.Polar)) +
  scale_y_continuous(limits=c(0.11,0.13), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Polar", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Polar.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Polar.sum <- gg.GLDAS.Polar + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Polar.sum




####
MERRA2.Polar.list <- list(MERRA2.Polar.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Polar ~ Year, data=Polar.df[c("MERRA2.Polar", "Year")]))$coefficients[2,1],5), digits = 3),
                          MERRA2.Polar.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Polar ~ Year, data=Polar.df[c("MERRA2.Polar", "Year")]))$r.squared,3), digits = 3),
                          MERRA2.Polar.p = sprintf('%0.3f', round(summary(lm(MERRA2.Polar ~ Year, data=Polar.df[c("MERRA2.Polar", "Year")]))$coefficients[2,4],3), digits = 3),
                          MERRA2.Polar.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Polar ~ Year, data=Polar.df[c("MERRA2.Polar", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Polar.list

MERRA2.Polar.list.s <- substitute(atop(italic(y) == MERRA2.Polar.slope * x~ + MERRA2.Polar.intercept~"",
                                       italic(R) ^ 2~ '=' ~MERRA2.Polar.r2~""~italic(p)~"="~MERRA2.Polar.p), MERRA2.Polar.list)

MERRA2.Polar.list.s

MERRA2.Polar.pred.int <- predict(lm(MERRA2.Polar ~ Year, data = Polar.df), interval = "confidence", level=0.95)
new.MERRA2.Polar.df <- cbind(Polar.df["MERRA2.Polar"], MERRA2.Polar.pred.int)
new.MERRA2.Polar.df$Year <- c(1981:2017)

##
gg.MERRA2.Polar <- ggplot(new.MERRA2.Polar.df, aes(y=MERRA2.Polar, x=Year)) +
  geom_line(aes(y=MERRA2.Polar)) +
  scale_y_continuous(limits=c(0.275,0.33), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Polar", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Polar.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Polar.sum <- gg.MERRA2.Polar + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Polar.sum



####
Model.mean.Polar.list <- list(Model.mean.Polar.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Polar ~ Year, data=Polar.df[c("Model.mean.Polar", "Year")]))$coefficients[2,1],5), digits = 3),
                              Model.mean.Polar.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Polar ~ Year, data=Polar.df[c("Model.mean.Polar", "Year")]))$r.squared,3), digits = 3),
                              Model.mean.Polar.p = sprintf('%0.3f', round(summary(lm(Model.mean.Polar ~ Year, data=Polar.df[c("Model.mean.Polar", "Year")]))$coefficients[2,4],3), digits = 3),
                              Model.mean.Polar.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Polar ~ Year, data=Polar.df[c("Model.mean.Polar", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Polar.list

Model.mean.Polar.list.s <- substitute(atop(italic(y) == Model.mean.Polar.slope * x~  Model.mean.Polar.intercept~"",
                                           italic(R) ^ 2~ '=' ~Model.mean.Polar.r2~""~italic(p)~"="~Model.mean.Polar.p), Model.mean.Polar.list)

Model.mean.Polar.list.s

Model.mean.Polar.pred.int <- predict(lm(Model.mean.Polar ~ Year, data = Polar.df), interval = "confidence", level=0.95)
new.Model.mean.Polar.df <- cbind(Polar.df["Model.mean.Polar"], Model.mean.Polar.pred.int)
new.Model.mean.Polar.df$Year <- c(1981:2017)

##
gg.Model.mean.Polar <- ggplot(new.Model.mean.Polar.df, aes(y=Model.mean.Polar, x=Year)) +
  geom_line(aes(y=Model.mean.Polar)) +
  scale_y_continuous(limits=c(0.225,0.25), n.breaks=3) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Polar", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Polar.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Polar.sum <- gg.Model.mean.Polar + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Polar.sum


####
gg.Polar.sum <- ggarrange(gg.ERA5.Polar.sum, gg.GLDAS.Polar.sum,
                          gg.MERRA2.Polar.sum, gg.Model.mean.Polar.sum,
                          ncol = 4, nrow = 1)
gg.Polar.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Polar.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################
