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



########
## Forest---------------------------------------------
Forest <- lc.re
Forest[Forest[]==2 | Forest[]==3 | Forest[]==4 | Forest[]==5 | Forest[]==6 | Forest[]==7 | Forest[]==8] <- NA
spplot(Forest[[1]])

w.Forest <- area(Forest[[1]], na.rm=T) / area(Forest[[1]], na.rm=T) %>% cellStats("sum")

ERA5.Forest.df <- mask(ERA5*w.Forest, Forest, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.Forest.df <- mask(GLDAS*w.Forest, Forest, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.Forest.df <- mask(MERRA2*w.Forest, Forest, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.Forest.df <- mask(Model.mean.y*w.Forest, Forest, maskvalue=NA) %>% cellStats("sum") # 



## Shrubland
SL <- lc.re
SL[SL[]==1 | SL[]==3 | SL[]==4 | SL[]==5 | SL[]==6 | SL[]==7 | SL[]==8] <- NA
spplot(SL[[1]])

w.SL <- area(SL[[1]], na.rm=T) / area(SL[[1]], na.rm=T) %>% cellStats("sum")

ERA5.SL.df <- mask(ERA5*w.SL, SL, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.SL.df <- mask(GLDAS*w.SL, SL, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.SL.df <- mask(MERRA2*w.SL, SL, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.SL.df <- mask(Model.mean.y*w.SL, SL, maskvalue=NA) %>% cellStats("sum") # 



### Savanna
Sa <- lc.re
Sa[Sa[]==1 | Sa[]==2 | Sa[]==4 | Sa[]==5 | Sa[]==6 | Sa[]==7 | Sa[]==8] <- NA

w.Sa <- area(Sa[[1]], na.rm=T) / area(Sa[[1]], na.rm=T) %>% cellStats("sum")

ERA5.Sa.df <- mask(ERA5*w.Sa, Sa, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.Sa.df <- mask(GLDAS*w.Sa, Sa, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.Sa.df <- mask(MERRA2*w.Sa, Sa, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.Sa.df <- mask(Model.mean.y*w.Sa, Sa, maskvalue=NA) %>% cellStats("sum") # 



## Grassland
GL <- lc.re
GL[GL[]==1 | GL[]==2 | GL[]==3 | GL[]==5 | GL[]==6 | GL[]==7 | GL[]==8] <- NA
spplot(GL[[1]])

w.GL <- area(GL[[1]], na.rm=T) / area(GL[[1]], na.rm=T) %>% cellStats("sum")

ERA5.GL.df <- mask(ERA5*w.GL, GL, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.GL.df <- mask(GLDAS*w.GL, GL, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.GL.df <- mask(MERRA2*w.GL, GL, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.GL.df <- mask(Model.mean.y*w.GL, GL, maskvalue=NA) %>% cellStats("sum") # 


## Wetland
WL <- lc.re
WL[WL[]==1 | WL[]==2 | WL[]==3 | WL[]==4 | WL[]==6 | WL[]==7 | WL[]==8] <- NA
spplot(WL[[1]])

w.WL <- area(WL[[1]], na.rm=T) / area(WL[[1]], na.rm=T) %>% cellStats("sum")

ERA5.WL.df <- mask(ERA5*w.WL, WL, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.WL.df <- mask(GLDAS*w.WL, WL, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.WL.df <- mask(MERRA2*w.WL, WL, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.WL.df <- mask(Model.mean.y*w.WL, WL, maskvalue=NA) %>% cellStats("sum") # 


## Cropland
CL <- lc.re
CL[CL[]==1 | CL[]==2 | CL[]==3 | CL[]==4 | CL[]==5 | CL[]==7 | CL[]==8] <- NA
spplot(CL[[1]])


w.CL <- area(CL[[1]], na.rm=T) / area(CL[[1]], na.rm=T) %>% cellStats("sum")

ERA5.CL.df <- mask(ERA5*w.CL, CL, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.CL.df <- mask(GLDAS*w.CL, CL, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.CL.df <- mask(MERRA2*w.CL, CL, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.CL.df <- mask(Model.mean.y*w.CL, CL, maskvalue=NA) %>% cellStats("sum") # 





####### Bareland ----------------------------------------
BL <- lc.re
BL[BL[] == 1 | BL[] == 2 | BL[] == 3 | BL[] == 4 | BL[] == 5 | BL[] == 6 | BL[] == 7] <- NA


w.BL <- area(BL[[1]], na.rm=T) / area(BL[[1]], na.rm=T) %>% cellStats("sum")

ERA5.BL.df <- mask(ERA5*w.BL, BL, maskvalue=NA) %>% cellStats("sum") # 
GLDAS.BL.df <- mask(GLDAS*w.BL, BL, maskvalue=NA) %>% cellStats("sum") # 
MERRA2.BL.df <- mask(MERRA2*w.BL, BL, maskvalue=NA) %>% cellStats("sum") # 
Model.mean.BL.df <- mask(Model.mean.y*w.BL, BL, maskvalue=NA) %>% cellStats("sum") # 







SL.df <- cbind(ERA5.SL.df,GLDAS.SL.df,MERRA2.SL.df, Model.mean.SL.df) %>% as.data.frame()
names(SL.df) <- c("ERA5.SL","GLDAS.SL","MERRA2.SL","Model.mean.SL")
SL.df$Year <- c(1981:2017)
head(SL.df)


Sa.df <- cbind(ERA5.Sa.df,GLDAS.Sa.df,MERRA2.Sa.df, Model.mean.Sa.df) %>% as.data.frame()
names(Sa.df) <- c("ERA5.Sa","GLDAS.Sa","MERRA2.Sa","Model.mean.Sa")
Sa.df$Year <- c(1981:2017)
head(Sa.df)


GL.df <- cbind(ERA5.GL.df,GLDAS.GL.df,MERRA2.GL.df, Model.mean.GL.df) %>% as.data.frame()
names(GL.df) <- c("ERA5.GL","GLDAS.GL","MERRA2.GL","Model.mean.GL")
GL.df$Year <- c(1981:2017)
head(GL.df)


CL.df <- cbind(ERA5.CL.df,GLDAS.CL.df,MERRA2.CL.df, Model.mean.CL.df) %>% as.data.frame()
names(CL.df) <- c("ERA5.CL","GLDAS.CL","MERRA2.CL","Model.mean.CL")
CL.df$Year <- c(1981:2017)
head(CL.df)

WL.df <- cbind(ERA5.WL.df,GLDAS.WL.df,MERRA2.WL.df, Model.mean.WL.df) %>% as.data.frame()
names(WL.df) <- c("ERA5.WL","GLDAS.WL","MERRA2.WL","Model.mean.WL")
WL.df$Year <- c(1981:2017)
head(WL.df)



BL.df <- cbind(ERA5.BL.df,GLDAS.BL.df,MERRA2.BL.df, Model.mean.BL.df) %>% as.data.frame()
names(BL.df) <- c("ERA5.BL","GLDAS.BL","MERRA2.BL","Model.mean.BL")
BL.df$Year <- c(1981:2017)
head(BL.df)

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



#####
### cbind df
Forest.df <- cbind(ERA5.Forest.df,GLDAS.Forest.df,MERRA2.Forest.df, Model.mean.Forest.df) %>% as.data.frame()
names(Forest.df) <- c("ERA5.Forest","GLDAS.Forest","MERRA2.Forest","Model.mean.Forest")
Forest.df$Year <- c(1981:2017)
head(Forest.df)



######## ggplot Forest----------------------------------------------------------
ERA5.Forest.list <- list(ERA5.Forest.slope = sprintf('%0.5f', round(summary(lm(ERA5.Forest ~ Year, data=Forest.df[c("ERA5.Forest", "Year")]))$coefficients[2,1],5), digits = 3),
                         ERA5.Forest.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Forest ~ Year, data=Forest.df[c("ERA5.Forest", "Year")]))$r.squared,3), digits = 3),
                         ERA5.Forest.p = sprintf('%0.3f', round(summary(lm(ERA5.Forest ~ Year, data=Forest.df[c("ERA5.Forest", "Year")]))$coefficients[2,4],3), digits = 3),
                         ERA5.Forest.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Forest ~ Year, data=Forest.df[c("ERA5.Forest", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Forest.list

ERA5.Forest.list.s <- substitute(atop(italic(y) == ERA5.Forest.slope * x~ + ERA5.Forest.intercept~"",
                                      italic(R) ^ 2~ '=' ~ERA5.Forest.r2~""~italic(p)~"="~ERA5.Forest.p), ERA5.Forest.list)

ERA5.Forest.list.s

ERA5.Forest.pred.int <- predict(lm(ERA5.Forest ~ Year, data = Forest.df), interval = "confidence", level=0.95)
new.ERA5.Forest.df <- cbind(Forest.df["ERA5.Forest"], ERA5.Forest.pred.int)
new.ERA5.Forest.df$Year <- c(1981:2017)

##
gg.ERA5.Forest <- ggplot(new.ERA5.Forest.df, aes(y=ERA5.Forest, x=Year)) +
  geom_line(aes(y=ERA5.Forest)) +
  scale_y_continuous(limits=c(0.32,0.40), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Forest", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Forest.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Forest.sum <- gg.ERA5.Forest + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Forest.sum



#######
GLDAS.Forest.list <- list(GLDAS.Forest.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Forest ~ Year, data=Forest.df[c("GLDAS.Forest", "Year")]))$coefficients[2,1],5), digits = 3),
                          GLDAS.Forest.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Forest ~ Year, data=Forest.df[c("GLDAS.Forest", "Year")]))$r.squared,3), digits = 3),
                          GLDAS.Forest.p = sprintf('%0.3f', round(summary(lm(GLDAS.Forest ~ Year, data=Forest.df[c("GLDAS.Forest", "Year")]))$coefficients[2,4],3), digits = 3),
                          GLDAS.Forest.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Forest ~ Year, data=Forest.df[c("GLDAS.Forest", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Forest.list

GLDAS.Forest.list.s <- substitute(atop(italic(y) == GLDAS.Forest.slope * x~ + GLDAS.Forest.intercept~"",
                                       italic(R) ^ 2~ '=' ~GLDAS.Forest.r2~""~italic(p)~"="~GLDAS.Forest.p), GLDAS.Forest.list)

GLDAS.Forest.list.s

GLDAS.Forest.pred.int <- predict(lm(GLDAS.Forest ~ Year, data = Forest.df), interval = "confidence", level=0.95)
new.GLDAS.Forest.df <- cbind(Forest.df["GLDAS.Forest"], GLDAS.Forest.pred.int)
new.GLDAS.Forest.df$Year <- c(1981:2017)

##
gg.GLDAS.Forest <- ggplot(new.GLDAS.Forest.df, aes(y=GLDAS.Forest, x=Year)) +
  geom_line(aes(y=GLDAS.Forest)) +
  scale_y_continuous(limits=c(0.11,0.15), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Forest", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Forest.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Forest.sum <- gg.GLDAS.Forest + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Forest.sum




####
MERRA2.Forest.list <- list(MERRA2.Forest.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Forest ~ Year, data=Forest.df[c("MERRA2.Forest", "Year")]))$coefficients[2,1],5), digits = 3),
                           MERRA2.Forest.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Forest ~ Year, data=Forest.df[c("MERRA2.Forest", "Year")]))$r.squared,3), digits = 3),
                           MERRA2.Forest.p = sprintf('%0.3f', round(summary(lm(MERRA2.Forest ~ Year, data=Forest.df[c("MERRA2.Forest", "Year")]))$coefficients[2,4],3), digits = 3),
                           MERRA2.Forest.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Forest ~ Year, data=Forest.df[c("MERRA2.Forest", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Forest.list

MERRA2.Forest.list.s <- substitute(atop(italic(y) == MERRA2.Forest.slope * x~ + MERRA2.Forest.intercept~"",
                                        italic(R) ^ 2~ '=' ~MERRA2.Forest.r2~""~italic(p)~"="~MERRA2.Forest.p), MERRA2.Forest.list)

MERRA2.Forest.list.s

MERRA2.Forest.pred.int <- predict(lm(MERRA2.Forest ~ Year, data = Forest.df), interval = "confidence", level=0.95)
new.MERRA2.Forest.df <- cbind(Forest.df["MERRA2.Forest"], MERRA2.Forest.pred.int)
new.MERRA2.Forest.df$Year <- c(1981:2017)

##
gg.MERRA2.Forest <- ggplot(new.MERRA2.Forest.df, aes(y=MERRA2.Forest, x=Year)) +
  geom_line(aes(y=MERRA2.Forest)) +
  scale_y_continuous(limits=c(0.26,0.33), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Forest", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Forest.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Forest.sum <- gg.MERRA2.Forest + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Forest.sum



####
Model.mean.Forest.list <- list(Model.mean.Forest.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Forest ~ Year, data=Forest.df[c("Model.mean.Forest", "Year")]))$coefficients[2,1],5), digits = 3),
                               Model.mean.Forest.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Forest ~ Year, data=Forest.df[c("Model.mean.Forest", "Year")]))$r.squared,3), digits = 3),
                               Model.mean.Forest.p = sprintf('%0.3f', round(summary(lm(Model.mean.Forest ~ Year, data=Forest.df[c("Model.mean.Forest", "Year")]))$coefficients[2,4],3), digits = 3),
                               Model.mean.Forest.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Forest ~ Year, data=Forest.df[c("Model.mean.Forest", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Forest.list

Model.mean.Forest.list.s <- substitute(atop(italic(y) == Model.mean.Forest.slope * x~ + Model.mean.Forest.intercept~"",
                                            italic(R) ^ 2~ '=' ~Model.mean.Forest.r2~""~italic(p)~"="~Model.mean.Forest.p), Model.mean.Forest.list)

Model.mean.Forest.list.s

Model.mean.Forest.pred.int <- predict(lm(Model.mean.Forest ~ Year, data = Forest.df), interval = "confidence", level=0.95)
new.Model.mean.Forest.df <- cbind(Forest.df["Model.mean.Forest"], Model.mean.Forest.pred.int)
new.Model.mean.Forest.df$Year <- c(1981:2017)

##
gg.Model.mean.Forest <- ggplot(new.Model.mean.Forest.df, aes(y=Model.mean.Forest, x=Year)) +
  geom_line(aes(y=Model.mean.Forest)) +
  scale_y_continuous(limits=c(0.23,0.29), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Forest", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Forest.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Forest.sum <- gg.Model.mean.Forest + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Forest.sum


####
gg.Forest.sum <- ggarrange(gg.ERA5.Forest.sum, gg.GLDAS.Forest.sum,
                           gg.MERRA2.Forest.sum, gg.Model.mean.Forest.sum,
                           ncol = 4, nrow = 1)
gg.Forest.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Forest.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################





######## ggplot SL----------------------------------------------------------
ERA5.SL.list <- list(ERA5.SL.slope = sprintf('%0.5f', round(summary(lm(ERA5.SL ~ Year, data=SL.df[c("ERA5.SL", "Year")]))$coefficients[2,1],5), digits = 3),
                     ERA5.SL.r2 = sprintf('%0.3f', round(summary(lm(ERA5.SL ~ Year, data=SL.df[c("ERA5.SL", "Year")]))$r.squared,3), digits = 3),
                     ERA5.SL.p = sprintf('%0.3f', round(summary(lm(ERA5.SL ~ Year, data=SL.df[c("ERA5.SL", "Year")]))$coefficients[2,4],3), digits = 3),
                     ERA5.SL.intercept = sprintf('%0.5f', round(summary(lm(ERA5.SL ~ Year, data=SL.df[c("ERA5.SL", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.SL.list

ERA5.SL.list.s <- substitute(atop(italic(y) == ERA5.SL.slope * x~ + ERA5.SL.intercept~"",
                                  italic(R) ^ 2~ '=' ~ERA5.SL.r2~""~italic(p)~"="~ERA5.SL.p), ERA5.SL.list)

ERA5.SL.list.s

ERA5.SL.pred.int <- predict(lm(ERA5.SL ~ Year, data = SL.df), interval = "confidence", level=0.95)
new.ERA5.SL.df <- cbind(SL.df["ERA5.SL"], ERA5.SL.pred.int)
new.ERA5.SL.df$Year <- c(1981:2017)

##
gg.ERA5.SL <- ggplot(new.ERA5.SL.df, aes(y=ERA5.SL, x=Year)) +
  geom_line(aes(y=ERA5.SL)) +
  scale_y_continuous(limits=c(0.19,0.235), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Shrublands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.SL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.SL.sum <- gg.ERA5.SL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.SL.sum



#######
GLDAS.SL.list <- list(GLDAS.SL.slope = sprintf('%0.5f', round(summary(lm(GLDAS.SL ~ Year, data=SL.df[c("GLDAS.SL", "Year")]))$coefficients[2,1],5), digits = 3),
                      GLDAS.SL.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.SL ~ Year, data=SL.df[c("GLDAS.SL", "Year")]))$r.squared,3), digits = 3),
                      GLDAS.SL.p = sprintf('%0.3f', round(summary(lm(GLDAS.SL ~ Year, data=SL.df[c("GLDAS.SL", "Year")]))$coefficients[2,4],3), digits = 3),
                      GLDAS.SL.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.SL ~ Year, data=SL.df[c("GLDAS.SL", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.SL.list

GLDAS.SL.list.s <- substitute(atop(italic(y) == GLDAS.SL.slope * x~  GLDAS.SL.intercept~"",
                                   italic(R) ^ 2~ '=' ~GLDAS.SL.r2~""~italic(p)~"="~GLDAS.SL.p), GLDAS.SL.list)

GLDAS.SL.list.s

GLDAS.SL.pred.int <- predict(lm(GLDAS.SL ~ Year, data = SL.df), interval = "confidence", level=0.95)
new.GLDAS.SL.df <- cbind(SL.df["GLDAS.SL"], GLDAS.SL.pred.int)
new.GLDAS.SL.df$Year <- c(1981:2017)

##
gg.GLDAS.SL <- ggplot(new.GLDAS.SL.df, aes(y=GLDAS.SL, x=Year)) +
  geom_line(aes(y=GLDAS.SL)) +
  scale_y_continuous(limits=c(0.07,0.105), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Shrublands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.SL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.SL.sum <- gg.GLDAS.SL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.SL.sum




####
MERRA2.SL.list <- list(MERRA2.SL.slope = sprintf('%0.5f', round(summary(lm(MERRA2.SL ~ Year, data=SL.df[c("MERRA2.SL", "Year")]))$coefficients[2,1],5), digits = 3),
                       MERRA2.SL.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.SL ~ Year, data=SL.df[c("MERRA2.SL", "Year")]))$r.squared,3), digits = 3),
                       MERRA2.SL.p = sprintf('%0.3f', round(summary(lm(MERRA2.SL ~ Year, data=SL.df[c("MERRA2.SL", "Year")]))$coefficients[2,4],3), digits = 3),
                       MERRA2.SL.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.SL ~ Year, data=SL.df[c("MERRA2.SL", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.SL.list

MERRA2.SL.list.s <- substitute(atop(italic(y) == MERRA2.SL.slope * x~ + MERRA2.SL.intercept~"",
                                    italic(R) ^ 2~ '=' ~MERRA2.SL.r2~""~italic(p)~"="~MERRA2.SL.p), MERRA2.SL.list)

MERRA2.SL.list.s

MERRA2.SL.pred.int <- predict(lm(MERRA2.SL ~ Year, data = SL.df), interval = "confidence", level=0.95)
new.MERRA2.SL.df <- cbind(SL.df["MERRA2.SL"], MERRA2.SL.pred.int)
new.MERRA2.SL.df$Year <- c(1981:2017)

##
gg.MERRA2.SL <- ggplot(new.MERRA2.SL.df, aes(y=MERRA2.SL, x=Year)) +
  geom_line(aes(y=MERRA2.SL)) +
  scale_y_continuous(limits=c(0.20,0.245), breaks=c(0.2,0.21,0.22,0.23,0.24)) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Shrublands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.SL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.SL.sum <- gg.MERRA2.SL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.SL.sum



####
Model.mean.SL.list <- list(Model.mean.SL.slope = sprintf('%0.5f', round(summary(lm(Model.mean.SL ~ Year, data=SL.df[c("Model.mean.SL", "Year")]))$coefficients[2,1],5), digits = 3),
                           Model.mean.SL.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.SL ~ Year, data=SL.df[c("Model.mean.SL", "Year")]))$r.squared,3), digits = 3),
                           Model.mean.SL.p = sprintf('%0.3f', round(summary(lm(Model.mean.SL ~ Year, data=SL.df[c("Model.mean.SL", "Year")]))$coefficients[2,4],3), digits = 3),
                           Model.mean.SL.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.SL ~ Year, data=SL.df[c("Model.mean.SL", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.SL.list

Model.mean.SL.list.s <- substitute(atop(italic(y) == Model.mean.SL.slope * x~ + Model.mean.SL.intercept~"",
                                        italic(R) ^ 2~ '=' ~Model.mean.SL.r2~""~italic(p)~"="~Model.mean.SL.p), Model.mean.SL.list)

Model.mean.SL.list.s

Model.mean.SL.pred.int <- predict(lm(Model.mean.SL ~ Year, data = SL.df), interval = "confidence", level=0.95)
new.Model.mean.SL.df <- cbind(SL.df["Model.mean.SL"], Model.mean.SL.pred.int)
new.Model.mean.SL.df$Year <- c(1981:2017)

##
gg.Model.mean.SL <- ggplot(new.Model.mean.SL.df, aes(y=Model.mean.SL, x=Year)) +
  geom_line(aes(y=Model.mean.SL)) +
  scale_y_continuous(limits=c(0.16,0.20), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Shrublands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.SL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.SL.sum <- gg.Model.mean.SL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.SL.sum


####
gg.SL.sum <- ggarrange(gg.ERA5.SL.sum, gg.GLDAS.SL.sum,
                       gg.MERRA2.SL.sum, gg.Model.mean.SL.sum,
                       ncol = 4, nrow = 1)
gg.SL.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Shrublands.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################





######## ggplot Sa----------------------------------------------------------
ERA5.Sa.list <- list(ERA5.Sa.slope = sprintf('%0.5f', round(summary(lm(ERA5.Sa ~ Year, data=Sa.df[c("ERA5.Sa", "Year")]))$coefficients[2,1],5), digits = 3),
                     ERA5.Sa.r2 = sprintf('%0.3f', round(summary(lm(ERA5.Sa ~ Year, data=Sa.df[c("ERA5.Sa", "Year")]))$r.squared,3), digits = 3),
                     ERA5.Sa.p = sprintf('%0.3f', round(summary(lm(ERA5.Sa ~ Year, data=Sa.df[c("ERA5.Sa", "Year")]))$coefficients[2,4],3), digits = 3),
                     ERA5.Sa.intercept = sprintf('%0.5f', round(summary(lm(ERA5.Sa ~ Year, data=Sa.df[c("ERA5.Sa", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.Sa.list

ERA5.Sa.list.s <- substitute(atop(italic(y) == ERA5.Sa.slope * x~ + ERA5.Sa.intercept~"",
                                  italic(R) ^ 2~ '=' ~ERA5.Sa.r2~""~italic(p)~"="~ERA5.Sa.p), ERA5.Sa.list)

ERA5.Sa.list.s

ERA5.Sa.pred.int <- predict(lm(ERA5.Sa ~ Year, data = Sa.df), interval = "confidence", level=0.95)
new.ERA5.Sa.df <- cbind(Sa.df["ERA5.Sa"], ERA5.Sa.pred.int)
new.ERA5.Sa.df$Year <- c(1981:2017)

##
gg.ERA5.Sa <- ggplot(new.ERA5.Sa.df, aes(y=ERA5.Sa, x=Year)) +
  geom_line(aes(y=ERA5.Sa)) +
  scale_y_continuous(limits=c(0.29,0.365), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Savannas", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.Sa.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.Sa.sum <- gg.ERA5.Sa + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.Sa.sum



#######
GLDAS.Sa.list <- list(GLDAS.Sa.slope = sprintf('%0.5f', round(summary(lm(GLDAS.Sa ~ Year, data=Sa.df[c("GLDAS.Sa", "Year")]))$coefficients[2,1],5), digits = 3),
                      GLDAS.Sa.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.Sa ~ Year, data=Sa.df[c("GLDAS.Sa", "Year")]))$r.squared,3), digits = 3),
                      GLDAS.Sa.p = sprintf('%0.3f', round(summary(lm(GLDAS.Sa ~ Year, data=Sa.df[c("GLDAS.Sa", "Year")]))$coefficients[2,4],3), digits = 3),
                      GLDAS.Sa.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.Sa ~ Year, data=Sa.df[c("GLDAS.Sa", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.Sa.list

GLDAS.Sa.list.s <- substitute(atop(italic(y) == GLDAS.Sa.slope * x~ + GLDAS.Sa.intercept~"",
                                   italic(R) ^ 2~ '=' ~GLDAS.Sa.r2~""~italic(p)~"="~GLDAS.Sa.p), GLDAS.Sa.list)

GLDAS.Sa.list.s

GLDAS.Sa.pred.int <- predict(lm(GLDAS.Sa ~ Year, data = Sa.df), interval = "confidence", level=0.95)
new.GLDAS.Sa.df <- cbind(Sa.df["GLDAS.Sa"], GLDAS.Sa.pred.int)
new.GLDAS.Sa.df$Year <- c(1981:2017)

##
gg.GLDAS.Sa <- ggplot(new.GLDAS.Sa.df, aes(y=GLDAS.Sa, x=Year)) +
  geom_line(aes(y=GLDAS.Sa)) +
  scale_y_continuous(limits=c(0.10,0.13), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Savanns", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.Sa.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.Sa.sum <- gg.GLDAS.Sa + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.Sa.sum




####
MERRA2.Sa.list <- list(MERRA2.Sa.slope = sprintf('%0.5f', round(summary(lm(MERRA2.Sa ~ Year, data=Sa.df[c("MERRA2.Sa", "Year")]))$coefficients[2,1],5), digits = 3),
                       MERRA2.Sa.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.Sa ~ Year, data=Sa.df[c("MERRA2.Sa", "Year")]))$r.squared,3), digits = 3),
                       MERRA2.Sa.p = sprintf('%0.3f', round(summary(lm(MERRA2.Sa ~ Year, data=Sa.df[c("MERRA2.Sa", "Year")]))$coefficients[2,4],3), digits = 3),
                       MERRA2.Sa.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.Sa ~ Year, data=Sa.df[c("MERRA2.Sa", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.Sa.list

MERRA2.Sa.list.s <- substitute(atop(italic(y) == MERRA2.Sa.slope * x~ + MERRA2.Sa.intercept~"",
                                    italic(R) ^ 2~ '=' ~MERRA2.Sa.r2~""~italic(p)~"="~MERRA2.Sa.p), MERRA2.Sa.list)

MERRA2.Sa.list.s

MERRA2.Sa.pred.int <- predict(lm(MERRA2.Sa ~ Year, data = Sa.df), interval = "confidence", level=0.95)
new.MERRA2.Sa.df <- cbind(Sa.df["MERRA2.Sa"], MERRA2.Sa.pred.int)
new.MERRA2.Sa.df$Year <- c(1981:2017)

##
gg.MERRA2.Sa <- ggplot(new.MERRA2.Sa.df, aes(y=MERRA2.Sa, x=Year)) +
  geom_line(aes(y=MERRA2.Sa)) +
  scale_y_continuous(limits=c(0.24,0.30), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Savannas", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.Sa.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.Sa.sum <- gg.MERRA2.Sa + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.Sa.sum



####
Model.mean.Sa.list <- list(Model.mean.Sa.slope = sprintf('%0.5f', round(summary(lm(Model.mean.Sa ~ Year, data=Sa.df[c("Model.mean.Sa", "Year")]))$coefficients[2,1],5), digits = 3),
                           Model.mean.Sa.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.Sa ~ Year, data=Sa.df[c("Model.mean.Sa", "Year")]))$r.squared,3), digits = 3),
                           Model.mean.Sa.p = sprintf('%0.3f', round(summary(lm(Model.mean.Sa ~ Year, data=Sa.df[c("Model.mean.Sa", "Year")]))$coefficients[2,4],3), digits = 3),
                           Model.mean.Sa.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.Sa ~ Year, data=Sa.df[c("Model.mean.Sa", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.Sa.list

Model.mean.Sa.list.s <- substitute(atop(italic(y) == Model.mean.Sa.slope * x~ + Model.mean.Sa.intercept~"",
                                        italic(R) ^ 2~ '=' ~Model.mean.Sa.r2~""~italic(p)~"="~Model.mean.Sa.p), Model.mean.Sa.list)

Model.mean.Sa.list.s

Model.mean.Sa.pred.int <- predict(lm(Model.mean.Sa ~ Year, data = Sa.df), interval = "confidence", level=0.95)
new.Model.mean.Sa.df <- cbind(Sa.df["Model.mean.Sa"], Model.mean.Sa.pred.int)
new.Model.mean.Sa.df$Year <- c(1981:2017)

##
gg.Model.mean.Sa <- ggplot(new.Model.mean.Sa.df, aes(y=Model.mean.Sa, x=Year)) +
  geom_line(aes(y=Model.mean.Sa)) +
  scale_y_continuous(limits=c(0.21,0.265), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Savannas", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.Sa.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.Sa.sum <- gg.Model.mean.Sa + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.Sa.sum


####
gg.Sa.sum <- ggarrange(gg.ERA5.Sa.sum, gg.GLDAS.Sa.sum,
                       gg.MERRA2.Sa.sum, gg.Model.mean.Sa.sum,
                       ncol = 4, nrow = 1)
gg.Sa.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Savanna.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################





######## ggplot GL----------------------------------------------------------
ERA5.GL.list <- list(ERA5.GL.slope = sprintf('%0.5f', round(summary(lm(ERA5.GL ~ Year, data=GL.df[c("ERA5.GL", "Year")]))$coefficients[2,1],5), digits = 3),
                     ERA5.GL.r2 = sprintf('%0.3f', round(summary(lm(ERA5.GL ~ Year, data=GL.df[c("ERA5.GL", "Year")]))$r.squared,3), digits = 3),
                     ERA5.GL.p = sprintf('%0.3f', round(summary(lm(ERA5.GL ~ Year, data=GL.df[c("ERA5.GL", "Year")]))$coefficients[2,4],3), digits = 3),
                     ERA5.GL.intercept = sprintf('%0.5f', round(summary(lm(ERA5.GL ~ Year, data=GL.df[c("ERA5.GL", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.GL.list

ERA5.GL.list.s <- substitute(atop(italic(y) == ERA5.GL.slope * x~ + ERA5.GL.intercept~"",
                                  italic(R) ^ 2~ '=' ~ERA5.GL.r2~""~italic(p)~"="~ERA5.GL.p), ERA5.GL.list)

ERA5.GL.list.s

ERA5.GL.pred.int <- predict(lm(ERA5.GL ~ Year, data = GL.df), interval = "confidence", level=0.95)
new.ERA5.GL.df <- cbind(GL.df["ERA5.GL"], ERA5.GL.pred.int)
new.ERA5.GL.df$Year <- c(1981:2017)

##
gg.ERA5.GL <- ggplot(new.ERA5.GL.df, aes(y=ERA5.GL, x=Year)) +
  geom_line(aes(y=ERA5.GL)) +
  scale_y_continuous(limits=c(0.22,0.27), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Grasslands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.GL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.GL.sum <- gg.ERA5.GL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.GL.sum



#######
GLDAS.GL.list <- list(GLDAS.GL.slope = sprintf('%0.5f', round(summary(lm(GLDAS.GL ~ Year, data=GL.df[c("GLDAS.GL", "Year")]))$coefficients[2,1],5), digits = 3),
                      GLDAS.GL.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.GL ~ Year, data=GL.df[c("GLDAS.GL", "Year")]))$r.squared,3), digits = 3),
                      GLDAS.GL.p = sprintf('%0.3f', round(summary(lm(GLDAS.GL ~ Year, data=GL.df[c("GLDAS.GL", "Year")]))$coefficients[2,4],3), digits = 3),
                      GLDAS.GL.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.GL ~ Year, data=GL.df[c("GLDAS.GL", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.GL.list

GLDAS.GL.list.s <- substitute(atop(italic(y) == GLDAS.GL.slope * x~ + GLDAS.GL.intercept~"",
                                   italic(R) ^ 2~ '=' ~GLDAS.GL.r2~""~italic(p)~"="~GLDAS.GL.p), GLDAS.GL.list)

GLDAS.GL.list.s

GLDAS.GL.pred.int <- predict(lm(GLDAS.GL ~ Year, data = GL.df), interval = "confidence", level=0.95)
new.GLDAS.GL.df <- cbind(GL.df["GLDAS.GL"], GLDAS.GL.pred.int)
new.GLDAS.GL.df$Year <- c(1981:2017)

##
gg.GLDAS.GL <- ggplot(new.GLDAS.GL.df, aes(y=GLDAS.GL, x=Year)) +
  geom_line(aes(y=GLDAS.GL)) +
  scale_y_continuous(limits=c(0.08,0.10), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Grasslands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.GL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.GL.sum <- gg.GLDAS.GL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.GL.sum




####
MERRA2.GL.list <- list(MERRA2.GL.slope = sprintf('%0.5f', round(summary(lm(MERRA2.GL ~ Year, data=GL.df[c("MERRA2.GL", "Year")]))$coefficients[2,1],5), digits = 3),
                       MERRA2.GL.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.GL ~ Year, data=GL.df[c("MERRA2.GL", "Year")]))$r.squared,3), digits = 3),
                       MERRA2.GL.p = sprintf('%0.3f', round(summary(lm(MERRA2.GL ~ Year, data=GL.df[c("MERRA2.GL", "Year")]))$coefficients[2,4],3), digits = 3),
                       MERRA2.GL.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.GL ~ Year, data=GL.df[c("MERRA2.GL", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.GL.list

MERRA2.GL.list.s <- substitute(atop(italic(y) == MERRA2.GL.slope * x~ + MERRA2.GL.intercept~"",
                                    italic(R) ^ 2~ '=' ~MERRA2.GL.r2~""~italic(p)~"="~MERRA2.GL.p), MERRA2.GL.list)

MERRA2.GL.list.s

MERRA2.GL.pred.int <- predict(lm(MERRA2.GL ~ Year, data = GL.df), interval = "confidence", level=0.95)
new.MERRA2.GL.df <- cbind(GL.df["MERRA2.GL"], MERRA2.GL.pred.int)
new.MERRA2.GL.df$Year <- c(1981:2017)

##
gg.MERRA2.GL <- ggplot(new.MERRA2.GL.df, aes(y=MERRA2.GL, x=Year)) +
  geom_line(aes(y=MERRA2.GL)) +
  scale_y_continuous(limits=c(0.20,0.24), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Grasslands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.GL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.GL.sum <- gg.MERRA2.GL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.GL.sum



####
Model.mean.GL.list <- list(Model.mean.GL.slope = sprintf('%0.5f', round(summary(lm(Model.mean.GL ~ Year, data=GL.df[c("Model.mean.GL", "Year")]))$coefficients[2,1],5), digits = 3),
                           Model.mean.GL.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.GL ~ Year, data=GL.df[c("Model.mean.GL", "Year")]))$r.squared,3), digits = 3),
                           Model.mean.GL.p = sprintf('%0.3f', round(summary(lm(Model.mean.GL ~ Year, data=GL.df[c("Model.mean.GL", "Year")]))$coefficients[2,4],3), digits = 3),
                           Model.mean.GL.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.GL ~ Year, data=GL.df[c("Model.mean.GL", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.GL.list

Model.mean.GL.list.s <- substitute(atop(italic(y) == Model.mean.GL.slope * x~ + Model.mean.GL.intercept~"",
                                        italic(R) ^ 2~ '=' ~Model.mean.GL.r2~""~italic(p)~"="~Model.mean.GL.p), Model.mean.GL.list)

Model.mean.GL.list.s

Model.mean.GL.pred.int <- predict(lm(Model.mean.GL ~ Year, data = GL.df), interval = "confidence", level=0.95)
new.Model.mean.GL.df <- cbind(GL.df["Model.mean.GL"], Model.mean.GL.pred.int)
new.Model.mean.GL.df$Year <- c(1981:2017)

##
gg.Model.mean.GL <- ggplot(new.Model.mean.GL.df, aes(y=Model.mean.GL, x=Year)) +
  geom_line(aes(y=Model.mean.GL)) +
  scale_y_continuous(limits=c(0.17,0.20), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Grasslands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.GL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.GL.sum <- gg.Model.mean.GL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.GL.sum


####
gg.GL.sum <- ggarrange(gg.ERA5.GL.sum, gg.GLDAS.GL.sum,
                       gg.MERRA2.GL.sum, gg.Model.mean.GL.sum,
                       ncol = 4, nrow = 1)
gg.GL.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Grasslands.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################






######## ggplot CL----------------------------------------------------------
ERA5.CL.list <- list(ERA5.CL.slope = sprintf('%0.5f', round(summary(lm(ERA5.CL ~ Year, data=CL.df[c("ERA5.CL", "Year")]))$coefficients[2,1],5), digits = 3),
                     ERA5.CL.r2 = sprintf('%0.3f', round(summary(lm(ERA5.CL ~ Year, data=CL.df[c("ERA5.CL", "Year")]))$r.squared,3), digits = 3),
                     ERA5.CL.p = sprintf('%0.3f', round(summary(lm(ERA5.CL ~ Year, data=CL.df[c("ERA5.CL", "Year")]))$coefficients[2,4],3), digits = 3),
                     ERA5.CL.intercept = sprintf('%0.5f', round(summary(lm(ERA5.CL ~ Year, data=CL.df[c("ERA5.CL", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.CL.list

ERA5.CL.list.s <- substitute(atop(italic(y) == ERA5.CL.slope * x~ + ERA5.CL.intercept~"",
                                  italic(R) ^ 2~ '=' ~ERA5.CL.r2~""~italic(p)~"="~ERA5.CL.p), ERA5.CL.list)

ERA5.CL.list.s

ERA5.CL.pred.int <- predict(lm(ERA5.CL ~ Year, data = CL.df), interval = "confidence", level=0.95)
new.ERA5.CL.df <- cbind(CL.df["ERA5.CL"], ERA5.CL.pred.int)
new.ERA5.CL.df$Year <- c(1981:2017)

##
gg.ERA5.CL <- ggplot(new.ERA5.CL.df, aes(y=ERA5.CL, x=Year)) +
  geom_line(aes(y=ERA5.CL)) +
  scale_y_continuous(limits=c(0.255,0.31), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Croplands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.CL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.CL.sum <- gg.ERA5.CL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.CL.sum



#######
GLDAS.CL.list <- list(GLDAS.CL.slope = sprintf('%0.5f', round(summary(lm(GLDAS.CL ~ Year, data=CL.df[c("GLDAS.CL", "Year")]))$coefficients[2,1],5), digits = 3),
                      GLDAS.CL.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.CL ~ Year, data=CL.df[c("GLDAS.CL", "Year")]))$r.squared,3), digits = 3),
                      GLDAS.CL.p = sprintf('%0.3f', round(summary(lm(GLDAS.CL ~ Year, data=CL.df[c("GLDAS.CL", "Year")]))$coefficients[2,4],3), digits = 3),
                      GLDAS.CL.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.CL ~ Year, data=CL.df[c("GLDAS.CL", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.CL.list

GLDAS.CL.list.s <- substitute(atop(italic(y) == GLDAS.CL.slope * x~ + GLDAS.CL.intercept~"",
                                   italic(R) ^ 2~ '=' ~GLDAS.CL.r2~""~italic(p)~"="~GLDAS.CL.p), GLDAS.CL.list)

GLDAS.CL.list.s

GLDAS.CL.pred.int <- predict(lm(GLDAS.CL ~ Year, data = CL.df), interval = "confidence", level=0.95)
new.GLDAS.CL.df <- cbind(CL.df["GLDAS.CL"], GLDAS.CL.pred.int)
new.GLDAS.CL.df$Year <- c(1981:2017)

##
gg.GLDAS.CL <- ggplot(new.GLDAS.CL.df, aes(y=GLDAS.CL, x=Year)) +
  geom_line(aes(y=GLDAS.CL)) +
  scale_y_continuous(limits=c(0.09,0.12), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Croplands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.CL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.CL.sum <- gg.GLDAS.CL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.CL.sum




####
MERRA2.CL.list <- list(MERRA2.CL.slope = sprintf('%0.5f', round(summary(lm(MERRA2.CL ~ Year, data=CL.df[c("MERRA2.CL", "Year")]))$coefficients[2,1],5), digits = 3),
                       MERRA2.CL.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.CL ~ Year, data=CL.df[c("MERRA2.CL", "Year")]))$r.squared,3), digits = 3),
                       MERRA2.CL.p = sprintf('%0.3f', round(summary(lm(MERRA2.CL ~ Year, data=CL.df[c("MERRA2.CL", "Year")]))$coefficients[2,4],3), digits = 3),
                       MERRA2.CL.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.CL ~ Year, data=CL.df[c("MERRA2.CL", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.CL.list

MERRA2.CL.list.s <- substitute(atop(italic(y) == MERRA2.CL.slope * x~ + MERRA2.CL.intercept~"",
                                    italic(R) ^ 2~ '=' ~MERRA2.CL.r2~""~italic(p)~"="~MERRA2.CL.p), MERRA2.CL.list)

MERRA2.CL.list.s

MERRA2.CL.pred.int <- predict(lm(MERRA2.CL ~ Year, data = CL.df), interval = "confidence", level=0.95)
new.MERRA2.CL.df <- cbind(CL.df["MERRA2.CL"], MERRA2.CL.pred.int)
new.MERRA2.CL.df$Year <- c(1981:2017)

##
gg.MERRA2.CL <- ggplot(new.MERRA2.CL.df, aes(y=MERRA2.CL, x=Year)) +
  geom_line(aes(y=MERRA2.CL)) +
  scale_y_continuous(limits=c(0.22,0.26), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Croplands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.CL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.CL.sum <- gg.MERRA2.CL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.CL.sum



####
Model.mean.CL.list <- list(Model.mean.CL.slope = sprintf('%0.5f', round(summary(lm(Model.mean.CL ~ Year, data=CL.df[c("Model.mean.CL", "Year")]))$coefficients[2,1],5), digits = 3),
                           Model.mean.CL.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.CL ~ Year, data=CL.df[c("Model.mean.CL", "Year")]))$r.squared,3), digits = 3),
                           Model.mean.CL.p = sprintf('%0.3f', round(summary(lm(Model.mean.CL ~ Year, data=CL.df[c("Model.mean.CL", "Year")]))$coefficients[2,4],3), digits = 3),
                           Model.mean.CL.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.CL ~ Year, data=CL.df[c("Model.mean.CL", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.CL.list

Model.mean.CL.list.s <- substitute(atop(italic(y) == Model.mean.CL.slope * x~ + Model.mean.CL.intercept~"",
                                        italic(R) ^ 2~ '=' ~Model.mean.CL.r2~""~italic(p)~"="~Model.mean.CL.p), Model.mean.CL.list)

Model.mean.CL.list.s

Model.mean.CL.pred.int <- predict(lm(Model.mean.CL ~ Year, data = CL.df), interval = "confidence", level=0.95)
new.Model.mean.CL.df <- cbind(CL.df["Model.mean.CL"], Model.mean.CL.pred.int)
new.Model.mean.CL.df$Year <- c(1981:2017)

##
gg.Model.mean.CL <- ggplot(new.Model.mean.CL.df, aes(y=Model.mean.CL, x=Year)) +
  geom_line(aes(y=Model.mean.CL)) +
  scale_y_continuous(limits=c(0.19,0.23), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Croplands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.CL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.CL.sum <- gg.Model.mean.CL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.CL.sum


####
gg.CL.sum <- ggarrange(gg.ERA5.CL.sum, gg.GLDAS.CL.sum,
                       gg.MERRA2.CL.sum, gg.Model.mean.CL.sum,
                       ncol = 4, nrow = 1)
gg.CL.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Croplands.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################





######## ggplot WL----------------------------------------------------------
ERA5.WL.list <- list(ERA5.WL.slope = sprintf('%0.5f', round(summary(lm(ERA5.WL ~ Year, data=WL.df[c("ERA5.WL", "Year")]))$coefficients[2,1],5), digits = 3),
                     ERA5.WL.r2 = sprintf('%0.3f', round(summary(lm(ERA5.WL ~ Year, data=WL.df[c("ERA5.WL", "Year")]))$r.squared,3), digits = 3),
                     ERA5.WL.p = sprintf('%0.3f', round(summary(lm(ERA5.WL ~ Year, data=WL.df[c("ERA5.WL", "Year")]))$coefficients[2,4],3), digits = 3),
                     ERA5.WL.intercept = sprintf('%0.5f', round(summary(lm(ERA5.WL ~ Year, data=WL.df[c("ERA5.WL", "Year")]))$coefficients[1,1],5), digits = 3)
)
ERA5.WL.list

ERA5.WL.list.s <- substitute(atop(italic(y) == ERA5.WL.slope * x~ + ERA5.WL.intercept~"",
                                  italic(R) ^ 2~ '=' ~ERA5.WL.r2~""~italic(p)~"="~ERA5.WL.p), ERA5.WL.list)

ERA5.WL.list.s

ERA5.WL.pred.int <- predict(lm(ERA5.WL ~ Year, data = WL.df), interval = "confidence", level=0.95)
new.ERA5.WL.df <- cbind(WL.df["ERA5.WL"], ERA5.WL.pred.int)
new.ERA5.WL.df$Year <- c(1981:2017)

##
gg.ERA5.WL <- ggplot(new.ERA5.WL.df, aes(y=ERA5.WL, x=Year)) +
  geom_line(aes(y=ERA5.WL)) +
  scale_y_continuous(limits=c(0.335,0.535), n.breaks=5) +
  labs(x=NULL, y=expression(paste("RZSM"," ","(",m^3," ",m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = Inf, label = "ERA5: Wetlands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.WL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.ERA5.WL.sum <- gg.ERA5.WL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.ERA5.WL.sum



#######
GLDAS.WL.list <- list(GLDAS.WL.slope = sprintf('%0.5f', round(summary(lm(GLDAS.WL ~ Year, data=WL.df[c("GLDAS.WL", "Year")]))$coefficients[2,1],5), digits = 3),
                      GLDAS.WL.r2 = sprintf('%0.3f', round(summary(lm(GLDAS.WL ~ Year, data=WL.df[c("GLDAS.WL", "Year")]))$r.squared,3), digits = 3),
                      GLDAS.WL.p = sprintf('%0.3f', round(summary(lm(GLDAS.WL ~ Year, data=WL.df[c("GLDAS.WL", "Year")]))$coefficients[2,4],3), digits = 3),
                      GLDAS.WL.intercept = sprintf('%0.5f', round(summary(lm(GLDAS.WL ~ Year, data=WL.df[c("GLDAS.WL", "Year")]))$coefficients[1,1],5), digits = 3)
)
GLDAS.WL.list

GLDAS.WL.list.s <- substitute(atop(italic(y) == GLDAS.WL.slope * x~ + GLDAS.WL.intercept~"",
                                   italic(R) ^ 2~ '=' ~GLDAS.WL.r2~""~italic(p)~"="~GLDAS.WL.p), GLDAS.WL.list)

GLDAS.WL.list.s

GLDAS.WL.pred.int <- predict(lm(GLDAS.WL ~ Year, data = WL.df), interval = "confidence", level=0.95)
new.GLDAS.WL.df <- cbind(WL.df["GLDAS.WL"], GLDAS.WL.pred.int)
new.GLDAS.WL.df$Year <- c(1981:2017)

##
gg.GLDAS.WL <- ggplot(new.GLDAS.WL.df, aes(y=GLDAS.WL, x=Year)) +
  geom_line(aes(y=GLDAS.WL)) +
  scale_y_continuous(limits=c(0.09,0.15), n.breaks=4) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "GLDAS: Wetlands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.WL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.GLDAS.WL.sum <- gg.GLDAS.WL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.GLDAS.WL.sum




####
MERRA2.WL.list <- list(MERRA2.WL.slope = sprintf('%0.5f', round(summary(lm(MERRA2.WL ~ Year, data=WL.df[c("MERRA2.WL", "Year")]))$coefficients[2,1],5), digits = 3),
                       MERRA2.WL.r2 = sprintf('%0.3f', round(summary(lm(MERRA2.WL ~ Year, data=WL.df[c("MERRA2.WL", "Year")]))$r.squared,3), digits = 3),
                       MERRA2.WL.p = sprintf('%0.3f', round(summary(lm(MERRA2.WL ~ Year, data=WL.df[c("MERRA2.WL", "Year")]))$coefficients[2,4],3), digits = 3),
                       MERRA2.WL.intercept = sprintf('%0.5f', round(summary(lm(MERRA2.WL ~ Year, data=WL.df[c("MERRA2.WL", "Year")]))$coefficients[1,1],5), digits = 3)
)
MERRA2.WL.list

MERRA2.WL.list.s <- substitute(atop(italic(y) == MERRA2.WL.slope * x~ + MERRA2.WL.intercept~"",
                                    italic(R) ^ 2~ '=' ~MERRA2.WL.r2~""~italic(p)~"="~MERRA2.WL.p), MERRA2.WL.list)

MERRA2.WL.list.s

MERRA2.WL.pred.int <- predict(lm(MERRA2.WL ~ Year, data = WL.df), interval = "confidence", level=0.95)
new.MERRA2.WL.df <- cbind(WL.df["MERRA2.WL"], MERRA2.WL.pred.int)
new.MERRA2.WL.df$Year <- c(1981:2017)

##
gg.MERRA2.WL <- ggplot(new.MERRA2.WL.df, aes(y=MERRA2.WL, x=Year)) +
  geom_line(aes(y=MERRA2.WL)) +
  scale_y_continuous(limits=c(0.23,0.36), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "MERRA2: Wetlands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.WL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.MERRA2.WL.sum <- gg.MERRA2.WL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.MERRA2.WL.sum



####
Model.mean.WL.list <- list(Model.mean.WL.slope = sprintf('%0.5f', round(summary(lm(Model.mean.WL ~ Year, data=WL.df[c("Model.mean.WL", "Year")]))$coefficients[2,1],5), digits = 3),
                           Model.mean.WL.r2 = sprintf('%0.3f', round(summary(lm(Model.mean.WL ~ Year, data=WL.df[c("Model.mean.WL", "Year")]))$r.squared,3), digits = 3),
                           Model.mean.WL.p = sprintf('%0.3f', round(summary(lm(Model.mean.WL ~ Year, data=WL.df[c("Model.mean.WL", "Year")]))$coefficients[2,4],3), digits = 3),
                           Model.mean.WL.intercept = sprintf('%0.5f', round(summary(lm(Model.mean.WL ~ Year, data=WL.df[c("Model.mean.WL", "Year")]))$coefficients[1,1],5), digits = 3)
)
Model.mean.WL.list

Model.mean.WL.list.s <- substitute(atop(italic(y) == Model.mean.WL.slope * x~ + Model.mean.WL.intercept~"",
                                        italic(R) ^ 2~ '=' ~Model.mean.WL.r2~""~italic(p)~"="~Model.mean.WL.p), Model.mean.WL.list)

Model.mean.WL.list.s

Model.mean.WL.pred.int <- predict(lm(Model.mean.WL ~ Year, data = WL.df), interval = "confidence", level=0.95)
new.Model.mean.WL.df <- cbind(WL.df["Model.mean.WL"], Model.mean.WL.pred.int)
new.Model.mean.WL.df$Year <- c(1981:2017)

##
gg.Model.mean.WL <- ggplot(new.Model.mean.WL.df, aes(y=Model.mean.WL, x=Year)) +
  geom_line(aes(y=Model.mean.WL)) +
  scale_y_continuous(limits=c(0.22,0.34), n.breaks=5) +
  labs(x=NULL, y=NULL) +
  annotate("text", x = -Inf, y = Inf, label = "Model mean: Wetlands", vjust = 2, hjust = -0.2, size =6, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.WL.list.s)), colour="black",
           parse = T, size = 6, vjust=-0.1, hjust=-0.1, family = "Italic Times New Roman") 


gg.Model.mean.WL.sum <- gg.Model.mean.WL + mytheme + lwr_line + upr_line + my_geom_smooth + my_geom_point + my_scale_x_continuous
gg.Model.mean.WL.sum


####
gg.WL.sum <- ggarrange(gg.ERA5.WL.sum, gg.GLDAS.WL.sum,
                       gg.MERRA2.WL.sum, gg.Model.mean.WL.sum,
                       ncol = 4, nrow = 1)
gg.WL.sum
ggsave("F:/SCI_02_专家回复/annual_changes/Wetlands.RZSM.pdf",
       width = 18, height=3,
       dpi=1200)
#############################################################

