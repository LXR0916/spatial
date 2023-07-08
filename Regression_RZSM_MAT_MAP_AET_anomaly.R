
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


####
w <- area(ERA5[[1]], na.rm=T) / area(ERA5[[1]], na.rm=T) %>% cellStats("sum")

Model.mean.df <- (Model.mean.y * w) %>% cellStats("sum") %>% as.data.frame()
#names(Model.mean.df) <- c("Model.mean")
#Model.mean.df$Year <- c(1981:2017)



### load annual tmp and pre  and AET--------------------------------
setwd("E:/New PHD/Study data/CRU climate/Annual")

tmp <- brick("cru_ts4.04.1901.2019.tmp.dat.annual.nc")[[81:117]]
pre <- brick("cru_ts4.04.1901.2019.pre.dat.annual.nc")[[81:117]]

AET <- brick("E:/New PHD/Study data/ET/GLEAM_AET/Year/Half_degree/E_1980_2020_GLEAM_v3.5a_0.5_yearly_AET.nc")[[2:38]] %>% mask(pre)
spplot(tmp[[1]])


##### anomaly of tmp, pre, aet
tmp.anomaly <- tmp - mean(tmp)
pre.anomaly <- pre - mean(pre)
AET.anomaly <- AET - mean(AET)

w.climate <- area(tmp.anomaly, na.rm=T) / area(tmp.anomaly, na.rm=T) %>% cellStats("sum")
tmp.anoma.df <- cellStats(tmp.anomaly * w.climate , stat = "sum") 
pre.anoma.df <- cellStats(pre.anomaly * w.climate , stat = "sum") 
AET.anoma.df <- cellStats(AET.anomaly * w.climate , stat = "sum") 


df.anoma <- cbind(Model.mean.df, tmp.anoma.df, pre.anoma.df, AET.anoma.df) %>% as.data.frame()
names(df.anoma) <-  c("RZSM","MAT","MAP", "AET")
head(df.anoma)



### ggplot------------------------------------------

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
        legend.text = element_text(size=20, family = "Times New Roman"),
        axis.text.x=element_text(size=16, family = "Times New Roman"),
        axis.text.y=element_text(size=16, family = "Times New Roman"),
        axis.title.y=element_text(size=15, family = "Times New Roman"),
        axis.title.x=element_text(size=15, family = "Times New Roman"),
        plot.margin = unit(c(0.5,1,0.5,1),"lines") #( top, right, bottom, left )
        #plot.title = element_text(hjust = 0.5, vjust = 0, size = 21)
  )

wr_line <- geom_line(aes(y = lwr), color = "red", linetype = "dashed")
upr_line <-  geom_line(aes(y = upr), color = "red", linetype = "dashed") 

my_geom_smooth <- geom_smooth(method = "lm", se=T, color="black") 

my_geom_point <-  geom_point(pch = 17, size=3) 



names(df.anoma)
df.anoma %>% dplyr::select(RZSM)

#summary(lm(df.anoma$RZSM~df.anoma$MAT))
library(dplyr)

####
MAT.list <- list(MAT.slope = sprintf('%0.5f', round(summary(lm(RZSM ~ MAT, data = df.anoma %>% dplyr::select(c("RZSM","MAT"))))$coefficients[2,1],5), nsmall = 3),
                 MAT.r2 = sprintf('%0.3f', round(summary(lm(RZSM ~ MAT, data = df.anoma %>% dplyr::select(c("RZSM","MAT"))))$r.squared,3), nsmall = 3), 
                 MAT.p = sprintf('%0.3f', round(summary(lm(RZSM ~ MAT, data = df.anoma %>% dplyr::select(c("RZSM","MAT"))))$coefficients[2,4],3), nsmall = 3))
MAT.list

MAT.s <- substitute(atop(italic(Slope)~ '=' ~MAT.slope
                         , italic(R) ^ 2~ '=' ~MAT.r2~""~italic(p)~"="~MAT.p), MAT.list)
MAT.s

MAT.pred <- predict(lm(RZSM ~ MAT, data = df.anoma %>% dplyr::select(c("RZSM","MAT"))), interval = "confidence", level=0.95)
new.RZSM.MAT.df <- cbind(df.anoma %>% dplyr::select(c("RZSM","MAT")), MAT.pred)


##
gg.RZSM.MAT.anoma <- ggplot(new.RZSM.MAT.df, aes(y=RZSM, x=MAT)) +
  scale_x_continuous(limits=c(-0.7, 0.8), breaks=c(-0.6, -0.3, 0, 0.3, 0.6), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.195, 0.212), n.breaks=4) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  labs(x=expression(paste("Temperature anomaly (", degree~C, ")")), 
       y=expression(paste("RZSM"," ","(",m^3," ", m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MAT.s)), colour="black",
           parse = T, size = 5, vjust=0, hjust=-0.1, family = "Italic Times New Roman") 

gg.RZSM.MAT.anoma.sum <- gg.RZSM.MAT.anoma + mytheme  + my_geom_smooth + my_geom_point 
gg.RZSM.MAT.anoma.sum





####
MAP.list <- list(MAP.slope = sprintf('%0.5f', round(summary(lm(RZSM ~ MAP, data = df.anoma %>% dplyr::select(c("RZSM","MAP"))))$coefficients[2,1],5), nsmall = 3),
                 MAP.r2 = sprintf('%0.3f', round(summary(lm(RZSM ~ MAP, data = df.anoma %>% dplyr::select(c("RZSM","MAP"))))$r.squared,3), nsmall = 3), 
                 MAP.p = sprintf('%0.3f', round(summary(lm(RZSM ~ MAP, data = df.anoma %>% dplyr::select(c("RZSM","MAP"))))$coefficients[2,4],3), nsmall = 3))
MAP.list

MAP.s <- substitute(atop(italic(Slope)~ '=' ~MAP.slope
                         , italic(R) ^ 2~ '=' ~MAP.r2~""~italic(p)~"="~MAP.p), MAP.list)
MAP.s

MAP.pred <- predict(lm(RZSM ~ MAP, data = df.anoma %>% dplyr::select(c("RZSM","MAP"))), interval = "confidence", level=0.95)
new.RZSM.MAP.df <- cbind(df.anoma %>% dplyr::select(c("RZSM","MAP")), MAP.pred)


##
gg.RZSM.MAP.anoma <- ggplot(new.RZSM.MAP.df, aes(y=RZSM, x=MAP)) +
  scale_x_continuous(limits=c(-40, 41), breaks=seq(-40, 41, 20), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.195, 0.212), n.breaks=4) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  labs(x=expression(paste("Precipitation anomaly (mm)")), 
       y=expression(paste("RZSM"," ","(",m^3," ", m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MAP.s)), colour="black",
           parse = T, size = 5, vjust=0, hjust=-0.1, family = "Italic Times New Roman") 

gg.RZSM.MAP.anoma.sum <- gg.RZSM.MAP.anoma + mytheme  + my_geom_smooth + my_geom_point 
gg.RZSM.MAP.anoma.sum






####
AET.list <- list(AET.slope = sprintf('%0.5f', round(summary(lm(RZSM ~ AET, data = df.anoma %>% dplyr::select(c("RZSM","AET"))))$coefficients[2,1],5), nsmall = 3),
                 AET.r2 = sprintf('%0.3f', round(summary(lm(RZSM ~ AET, data = df.anoma %>% dplyr::select(c("RZSM","AET"))))$r.squared,3), nsmall = 3), 
                 AET.p = sprintf('%0.3f', round(summary(lm(RZSM ~ AET, data = df.anoma %>% dplyr::select(c("RZSM","AET"))))$coefficients[2,4],3), nsmall = 3))
AET.list

AET.s <- substitute(atop(italic(Slope)~ '=' ~AET.slope
                         , italic(R) ^ 2~ '=' ~AET.r2~""~italic(p)~"="~AET.p), AET.list)
AET.s

AET.pred <- predict(lm(RZSM ~ AET, data = df.anoma %>% dplyr::select(c("RZSM","AET"))), interval = "confidence", level=0.95)
new.RZSM.AET.df <- cbind(df.anoma %>% dplyr::select(c("RZSM","AET")), AET.pred)


##
gg.RZSM.AET.anoma <- ggplot(new.RZSM.AET.df, aes(y=RZSM, x=AET)) +
  scale_x_continuous(limits=c(-13, 14), breaks=c(-12, -8, -4, 0, 4, 8, 12), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.195, 0.212), n.breaks=4) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  labs(x=expression(paste("Evapotranspiarion anomaly (mm)")),
       y=expression(paste("RZSM"," ","(",m^3," ", m^-3," ",yr^-1, ")"))) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(AET.s)), colour="black",
           parse = T, size = 5, vjust=0, hjust=-0.1, family = "Italic Times New Roman") 

gg.RZSM.AET.anoma.sum <- gg.RZSM.AET.anoma + mytheme  + my_geom_smooth + my_geom_point 
gg.RZSM.AET.anoma.sum

##############################




g1 <- egg::ggarrange(gg.RZSM.MAT.anoma.sum +
                       theme(#axis.text.x = element_blank(),
                         # axis.ticks.y = element_blank(),
                         axis.title.y = element_blank() ,
                         plot.margin = margin(r = 8,l=8,t=8,b=0)),
                     gg.RZSM.MAP.anoma.sum +
                       theme(axis.text.y = element_blank(),
                             # axis.ticks.y = element_blank(),
                             axis.title.y = element_blank() ,
                             plot.margin = margin(r = 8,l=0,t=8,b=0)),
                     gg.RZSM.AET.anoma.sum +
                       theme(
                         axis.text.y = element_blank(),
                         axis.title.y = element_blank(),
                         plot.margin = margin(r = 12,l=0,t=0,b=8)),
                     ncol  = 3
)

annotate_figure(g1, left=text_grob(expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")")),
                                   rot = 90, family = "Times New Roman", size=13))


ggsave('F:/SCI_02/RZSM_MAT_MAP_AET_anomalies/RZSM_MAT_MAP_AET_anomaly01.pdf',
       width = 10, height = 3,
       dpi = 1200)
