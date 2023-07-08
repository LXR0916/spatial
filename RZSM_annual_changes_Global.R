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


###
w <- area(ERA5[[1]], na.rm=T) / area(ERA5[[1]], na.rm=T) %>% cellStats("sum")

ERA5.df <- (ERA5 * w) %>% cellStats("sum") %>% as.data.frame()
names(ERA5.df) <- c("ERA5")
ERA5.df$Year <- c(1981:2017)

GLDAS.df <- (GLDAS * w) %>% cellStats("sum") %>% as.data.frame()
names(GLDAS.df) <- c("GLDAS")
GLDAS.df$Year <- c(1981:2017)

MERRA2.df <- (MERRA2 * w) %>% cellStats("sum") %>% as.data.frame()
names(MERRA2.df) <- c("MERRA2")
MERRA2.df$Year <- c(1981:2017)


Model.mean.df <- (Model.mean.y * w) %>% cellStats("sum") %>% as.data.frame()
names(Model.mean.df) <- c("Model.mean")
Model.mean.df$Year <- c(1981:2017)

RZSM.global.df <- cbind(ERA5.df[,1], GLDAS.df[,1], MERRA2.df[,1], Model.mean.df) # m3 m-3
names(RZSM.global.df) <- c("ERA5", "GLDAS", "MERRA2", "Model.mean", "Year")
head(RZSM.global.df)



##### ggplot global RZSM
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
        legend.text = element_text(size=13, family = "Times New Roman"),
        axis.text.x=element_text(size=13, family = "Times New Roman"),
        axis.text.y=element_text(size=13, family = "Times New Roman"),
        axis.title.y=element_text(size=13, family = "Times New Roman"),
        plot.margin = unit(c(1.5,1.5,0.5,1),"lines") #( top, right, bottom, left )
        #plot.title = element_text(hjust = 0.5, vjust = 0, size = 21)
  )


ERA5.list <- list(ERA5.slope = sprintf('%0.5f', round(summary(lm(ERA5 ~ Year, data=ERA5.df))$coefficients[2,1],5), digits = 5),
                  ERA5.r2 = sprintf('%0.3f', round(summary(lm(ERA5 ~ Year, data=ERA5.df))$r.squared,3), digits = 3),
                  ERA5.p = sprintf('%0.3f', round(summary(lm(ERA5 ~ Year, data=ERA5.df))$coefficients[2,4],3), digits = 3),
                  ERA5.intercept = sprintf('%0.5f', round(summary(lm(ERA5 ~ Year, data=ERA5.df))$coefficients[1,1],5), digits = 5)
)
ERA5.list

ERA5.list.s <- substitute(atop(italic(y) == ERA5.slope * x~ + ERA5.intercept~""
                               , italic(R) ^ 2~ '=' ~ERA5.r2~""~italic(p)~"="~ERA5.p), ERA5.list)

ERA5.list.s


ERA5.pred.int <- predict(lm(ERA5 ~ Year, data = ERA5.df), interval = "confidence", level=0.95)

new.ERA5.df <- cbind(ERA5.df, ERA5.pred.int)

ERA5.gg <- ggplot(new.ERA5.df, aes(y=ERA5, x=Year)) +
  geom_point(pch = 17, size=2) + 
  geom_line(aes(y=ERA5)) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se=T, color="black") +
  scale_x_continuous(limits=c(1980, 2017), breaks=seq(1980, 2017, 6), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.25, 0.28), n.breaks=5) +
  labs(x=NULL, y=expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")"))) +
  annotate("text", x = 1998, y = Inf, label = "(a) ERA5", vjust = 3,  size = 4, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(ERA5.list.s)), colour="black",
           parse = T, size = 4, vjust=-0.2, hjust=-0.1, family = "Italic Times New Roman") 


ERA5.gg.sum <- ERA5.gg + mytheme
ERA5.gg.sum




#########################

GLDAS.list <- list(GLDAS.slope = sprintf('%0.5f', round(summary(lm(GLDAS ~ Year, data=GLDAS.df))$coefficients[2,1],5), digits = 5),
                   GLDAS.r2 = sprintf('%0.3f', round(summary(lm(GLDAS ~ Year, data=GLDAS.df))$r.squared,3), digits = 3),
                   GLDAS.p = sprintf('%0.3f', round(summary(lm(GLDAS ~ Year, data=GLDAS.df))$coefficients[2,4],3), digits = 3),
                   GLDAS.intercept = sprintf('%0.5f', round(summary(lm(GLDAS ~ Year, data=GLDAS.df))$coefficients[1,1],5), digits = 5)
)
GLDAS.list

GLDAS.list.s <- substitute(atop(italic(y) == GLDAS.slope * x~ + GLDAS.intercept~""
                                , italic(R) ^ 2~ '=' ~GLDAS.r2~""~italic(p)~"="~GLDAS.p), GLDAS.list)

GLDAS.list.s


GLDAS.pred.int <- predict(lm(GLDAS ~ Year, data = GLDAS.df), interval = "confidence", level=0.95)

new.GLDAS.df <- cbind(GLDAS.df, GLDAS.pred.int)

GLDAS.gg <- ggplot(new.GLDAS.df, aes(y=GLDAS, x=Year)) +
  geom_point(pch = 17, size=2) + 
  geom_line(aes(y=GLDAS)) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se=T, color="black") +
  scale_x_continuous(limits=c(1980, 2017), breaks=seq(1980, 2017, 6), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.10, 0.115), breaks=c(0.10, 0.11)) +
  labs(x=NULL, y=expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")"))) +
  annotate("text", x = 1998, y = Inf, label = "(d) GLDAS", vjust =3, size = 4, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(GLDAS.list.s)), colour="black",
           parse = T, size = 4, vjust=-0.2, hjust=-0.1, family = "Italic Times New Roman") 


GLDAS.gg.sum <- GLDAS.gg + mytheme
GLDAS.gg.sum



########################
MERRA2.list <- list(MERRA2.slope = sprintf('%0.5f', round(summary(lm(MERRA2 ~ Year, data=MERRA2.df))$coefficients[2,1],5), digits = 5),
                    MERRA2.r2 = sprintf('%0.3f', round(summary(lm(MERRA2 ~ Year, data=MERRA2.df))$r.squared,3), digits = 3),
                    MERRA2.p = sprintf('%0.3f', round(summary(lm(MERRA2 ~ Year, data=MERRA2.df))$coefficients[2,4],3), digits = 3),
                    MERRA2.intercept = sprintf('%0.5f', round(summary(lm(MERRA2 ~ Year, data=MERRA2.df))$coefficients[1,1],5), digits = 5)
)
MERRA2.list

MERRA2.list.s <- substitute(atop(italic(y) == MERRA2.slope * x~ + MERRA2.intercept~""
                                 , italic(R) ^ 2~ '=' ~MERRA2.r2~""~italic(p)~"="~MERRA2.p), MERRA2.list)

MERRA2.list.s


MERRA2.pred.int <- predict(lm(MERRA2 ~ Year, data = MERRA2.df), interval = "confidence", level=0.95)

new.MERRA2.df <- cbind(MERRA2.df, MERRA2.pred.int)

MERRA2.gg <- ggplot(new.MERRA2.df, aes(y=MERRA2, x=Year)) +
  geom_point(pch = 17, size=2) + 
  geom_line(aes(y=MERRA2)) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se=T, color="black") +
  scale_x_continuous(limits=c(1980, 2017), breaks=seq(1980, 2017, 6), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.23, 0.25), breaks=c(0.23, 0.24)) +
  labs(x=NULL, y=expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")"))) +
  annotate("text", x = 1998, y = Inf, label = "(b) MERRA-2", vjust = 3,  size = 4, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(MERRA2.list.s)), colour="black",
           parse = T, size = 4, vjust=-0.2, hjust=-0.1, family = "Italic Times New Roman") 


MERRA2.gg.sum <- MERRA2.gg + mytheme
MERRA2.gg.sum





#########################

Model.mean.list <- list(Model.mean.slope = sprintf('%0.5f', round(summary(lm(Model.mean ~ Year, data=Model.mean.df))$coefficients[2,1],5), digits = 5),
                        Model.mean.r2 = sprintf('%0.3f', round(summary(lm(Model.mean ~ Year, data=Model.mean.df))$r.squared,3), digits = 3),
                        Model.mean.p = sprintf('%0.3f', round(summary(lm(Model.mean ~ Year, data=Model.mean.df))$coefficients[2,4],3), digits = 3),
                        Model.mean.intercept = sprintf('%0.5f', round(summary(lm(Model.mean ~ Year, data=Model.mean.df))$coefficients[1,1],5), digits = 5)
)
Model.mean.list

Model.mean.list.s <- substitute(atop(italic(y) == Model.mean.slope * x~ + Model.mean.intercept~""
                                     , italic(R) ^ 2~ '=' ~Model.mean.r2~""~italic(p)~"="~Model.mean.p), Model.mean.list)

Model.mean.list.s


Model.mean.pred.int <- predict(lm(Model.mean ~ Year, data = Model.mean.df), interval = "confidence", level=0.95)

new.Model.mean.df <- cbind(Model.mean.df, Model.mean.pred.int)

Model.mean.gg <- ggplot(new.Model.mean.df, aes(y=Model.mean, x=Year)) +
  geom_point(pch = 17, size=2) + 
  geom_line(aes(y=Model.mean)) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se=T, color="black") +
  scale_x_continuous(limits=c(1980, 2017), breaks=seq(1980, 2017, 6), expand=c(0,0)) +
  scale_y_continuous(limits=c(0.195, 0.215), breaks=c(0.19, 0.20, 0.21)) +
  labs(x=NULL, y=expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")"))) +
  annotate("text", x = 1998, y = Inf, label = "(c) Model mean", vjust = 3,  size = 4, family = "Times New Roman" ) +
  annotate("text", x = -Inf, y = -Inf,label = as.character(as.expression(Model.mean.list.s)), colour="black",
           parse = T, size = 4, vjust=-0.2, hjust=-0.1, family = "Italic Times New Roman") 


Model.mean.gg.sum <- Model.mean.gg + mytheme
Model.mean.gg.sum

#########################
#########################
#########################

g1 <- ggarrange(ERA5.gg.sum + 
                  theme(axis.text.x = element_blank(),
                        #axis.ticks.y = element_blank(),
                        axis.title.y = element_blank() ,
                        plot.margin = margin(r = 8,l=8,t=8,b=0)
                  ),
                MERRA2.gg.sum + 
                  theme(axis.text.x = element_blank(),
                        #axis.ticks.y = element_blank(),
                        axis.title.y = element_blank(),
                        plot.margin = margin(r = 10,l=8,t=0,b=0) ),
                Model.mean.gg.sum + 
                  theme(axis.text.x = element_blank(),
                        # axis.ticks.y = element_blank(),
                        axis.title.y = element_blank(),
                        plot.margin = margin(r = 8,l=8,t=0,b=0) ),
                GLDAS.gg.sum +
                  theme(
                    #axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = margin(r = 12,l=8,t=0,b=8) ),
                nrow = 4
                # labels = c("(a)", "(b)", "(c)", "(d)"), 
                #label.args = list(gp = grid::gpar(font = 1, fontsize = 16,
                #                            face = "plain", fontfamily = "Times New Roman"))
                
)


annotate_figure(g1, left=text_grob(expression(paste(" RZSM"," ","(", m^3," ",m^-3," ", yr^-1, ")")),
                                   rot = 90, family = "Times New Roman", size=13))

ggsave("F:/SCI_02_专家回复/annual_changes/Interannual_change_RZSM_Global_01.pdf",
       height = 8, width = 5,
       dpi = 1200)
