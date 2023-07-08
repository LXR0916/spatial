
library(ppcor)
library(raster)
library(ncdf4)
library(sp)
library(dplyr)

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
#####################################################################




### 1:ERA5
##### load CRU climate data-------------------------------------------
setwd("E:/New PHD/Study data/CRU climate/Annual")

cru.list <- list.files(pattern = ".nc")
cru.list

cru.data <- lapply(cru.list, brick)
cru.data

MAT <- mask(stack(cru.data[[3]])[[81:117]],  lc.tif, maskvalue = NA) #81:117: from 1981 to 2017
names(MAT) <- c(1981:2017)

MAP <- mask(stack(cru.data[[2]])[[81:117]],  lc.tif, maskvalue = NA)
names(MAP) <- c(1981:2017)

##### load ET (GLEAM) data------------------------------------

AET.file <- brick("E:/New PHD/Study data/ET/GLEAM_AET/Year/Half_degree/E_1980_2020_GLEAM_v3.5a_0.5_yearly_AET.nc")
names(AET.file) <- c(1980:2020)  #[[2:38]]
#spplot(AET[[1]])
AET <- mask(AET.file[[2:38]], lc.tif, maskvalue = NA) #1981:2017-[[2:38]]
##########################################################################


## yearly RZSM
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")

ERA5 <- brick("ERA5_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA) # m3 m-3


#####
Global.ERA5.array <- as.array(ERA5)
MAT.array<-as.array(MAT)
MAP.array<-as.array(MAP)
AET.array<-as.array(AET)


ERA5.MAT.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
ERA5.MAT.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

ERA5.MAP.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
ERA5.MAP.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

ERA5.AET.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
ERA5.AET.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])



for (row in c(1:dim(MAP.array)[1])){
  for (col in c(1:dim(MAP.array)[2])){
    Global.ERA5.na.len<-length(which(is.na(Global.ERA5.array[row,col,])))
    MAT.na.len<-length(which(is.na(MAT.array[row,col,])))
    MAP.na.len<-length(which(is.na(MAP.array[row,col,])))
    AET.na.len<-length(which(is.na(AET.array[row,col,])))
    
    
    if ((Global.ERA5.na.len==0)&(MAT.na.len==0)&(MAP.na.len==0)&(AET.na.len==0)) {
      Global.ERA5<-Global.ERA5.array[row,col,]
      Global.MAT<-MAT.array[row,col,]
      Global.MAP<-MAP.array[row,col,]
      Global.AET<-AET.array[row,col,]
      
      ERA5_Global_df_p <- data.frame(Global.ERA5, Global.MAT, Global.MAP, Global.AET)
      if(mean(ERA5_Global_df_p$Global.MAP) !=  ERA5_Global_df_p$Global.MAP[1]){
        colnames(ERA5_Global_df_p)<-c("Global.ERA5","MAT","MAP","AET") 
        
        ERA5.p.cor.Global.MAT <- pcor.test(ERA5_Global_df_p$Global.ERA5,ERA5_Global_df_p$MAT,ERA5_Global_df_p[,c("MAP","AET")])
        ERA5.MAT.coef.matrix_p[row,col]<-ERA5.p.cor.Global.MAT$estimate
        ERA5.MAT.p.matrix_p[row,col]<-ERA5.p.cor.Global.MAT$p.value
        
        ERA5.p.cor.Global.MAP <- pcor.test(ERA5_Global_df_p$Global.ERA5,ERA5_Global_df_p$MAP,ERA5_Global_df_p[,c("MAT","AET")])
        ERA5.MAP.coef.matrix_p[row,col]<-ERA5.p.cor.Global.MAP$estimate
        ERA5.MAP.p.matrix_p[row,col]<-ERA5.p.cor.Global.MAP$p.value
        
        ERA5.p.cor.Global.AET <- pcor.test(ERA5_Global_df_p$Global.ERA5,ERA5_Global_df_p$AET,ERA5_Global_df_p[,c("MAT","MAP")])
        ERA5.AET.coef.matrix_p[row,col]<-ERA5.p.cor.Global.AET$estimate
        ERA5.AET.p.matrix_p[row,col]<-ERA5.p.cor.Global.AET$p.value
        
      }
      
      
    }
  }  
  
}


ERA5.Global.matrix.name<-list(ERA5.MAT.coef.matrix_p, ERA5.MAT.p.matrix_p, ERA5.MAP.coef.matrix_p, ERA5.MAP.p.matrix_p,
                              ERA5.AET.coef.matrix_p, ERA5.AET.p.matrix_p)

ERA5.Global.variable.name<-c("ERA5.MAT.coef.matrix_p", "ERA5.MAT.p.matrix_p", "ERA5.MAP.coef.matrix_p", "ERA5.MAP.p.matrix_p",
                             "ERA5.AET.coef.matrix_p", "ERA5.AET.p.matrix_p")



####
ERA5.Global.raster.matrix <- raster(ERA5.Global.matrix.name[[6]])

xy <- extent(-180,180,-90,90)
projection(ERA5.Global.raster.matrix) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    
extent(ERA5.Global.raster.matrix) <- xy

#spplot(ERA5.Global.raster.matrix)
#ERA5.Global.raster.matrix[ERA5.Global.raster.matrix[] < -1 | ERA5.Global.raster.matrix[] > 1] <- NA
#OutPath <- "F:/New PHD results/test"
writeRaster(ERA5.Global.raster.matrix, "F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/ERA5.RZSM.AET.p.tif",
            format="GTiff",overwrite= TRUE)

#################################################################################################################
#################################################################################################################



#### 2:GLDAS-------------------------------------------------------------------------------------------
##### load CRU climate data-------------------------------------------
setwd("E:/New PHD/Study data/CRU climate/Annual")

cru.list <- list.files(pattern = ".nc")
cru.list

cru.data <- lapply(cru.list, brick)
cru.data

MAT <- mask(stack(cru.data[[3]])[[81:117]],  lc.tif, maskvalue = NA) #81:117: from 1981 to 2017
names(MAT) <- c(1981:2017)

MAP <- mask(stack(cru.data[[2]])[[81:117]],  lc.tif, maskvalue = NA)
names(MAP) <- c(1981:2017)

##### load ET (GLEAM) data------------------------------------

AET.file <- brick("E:/New PHD/Study data/ET/GLEAM_AET/Year/Half_degree/E_1980_2020_GLEAM_v3.5a_0.5_yearly_AET.nc")
names(AET.file) <- c(1980:2020)  #[[2:38]]
#spplot(AET[[1]])
AET <- mask(AET.file[[2:38]], lc.tif, maskvalue = NA) #1981:2017-[[2:38]]
##########################################################################


## yearly RZSM
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")
GLDAS <- brick("GLDAS_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA)# m3 m-3


##### GLDAS RZSM correlated with tmp, pre, AET-------------------------------------------------------------------------------

Global.GLDAS.array <- as.array(GLDAS)
MAT.array<-as.array(MAT)
MAP.array<-as.array(MAP)
AET.array<-as.array(AET)


GLDAS.MAT.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
GLDAS.MAT.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

GLDAS.MAP.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
GLDAS.MAP.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

GLDAS.AET.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
GLDAS.AET.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])



for (row in c(1:dim(MAP.array)[1])){
  for (col in c(1:dim(MAP.array)[2])){
    Global.GLDAS.na.len<-length(which(is.na(Global.GLDAS.array[row,col,])))
    MAT.na.len<-length(which(is.na(MAT.array[row,col,])))
    MAP.na.len<-length(which(is.na(MAP.array[row,col,])))
    AET.na.len<-length(which(is.na(AET.array[row,col,])))
    
    
    if ((Global.GLDAS.na.len==0)&(MAT.na.len==0)&(MAP.na.len==0)&(AET.na.len==0)) {
      Global.GLDAS<-Global.GLDAS.array[row,col,]
      Global.MAT<-MAT.array[row,col,]
      Global.MAP<-MAP.array[row,col,]
      Global.AET<-AET.array[row,col,]
      
      GLDAS_Global_df_p <- data.frame(Global.GLDAS, Global.MAT, Global.MAP, Global.AET)
      if(mean(GLDAS_Global_df_p$Global.MAP) !=  GLDAS_Global_df_p$Global.MAP[1]){
        colnames(GLDAS_Global_df_p)<-c("Global.GLDAS","MAT","MAP","AET") 
        
        GLDAS.p.cor.Global.MAT <- pcor.test(GLDAS_Global_df_p$Global.GLDAS,GLDAS_Global_df_p$MAT,GLDAS_Global_df_p[,c("MAP","AET")])
        GLDAS.MAT.coef.matrix_p[row,col]<-GLDAS.p.cor.Global.MAT$estimate
        GLDAS.MAT.p.matrix_p[row,col]<-GLDAS.p.cor.Global.MAT$p.value
        
        GLDAS.p.cor.Global.MAP <- pcor.test(GLDAS_Global_df_p$Global.GLDAS,GLDAS_Global_df_p$MAP,GLDAS_Global_df_p[,c("MAT","AET")])
        GLDAS.MAP.coef.matrix_p[row,col]<-GLDAS.p.cor.Global.MAP$estimate
        GLDAS.MAP.p.matrix_p[row,col]<-GLDAS.p.cor.Global.MAP$p.value
        
        GLDAS.p.cor.Global.AET <- pcor.test(GLDAS_Global_df_p$Global.GLDAS,GLDAS_Global_df_p$AET,GLDAS_Global_df_p[,c("MAT","MAP")])
        GLDAS.AET.coef.matrix_p[row,col]<-GLDAS.p.cor.Global.AET$estimate
        GLDAS.AET.p.matrix_p[row,col]<-GLDAS.p.cor.Global.AET$p.value
        
      }
      
      
    }
  }  
  
}


GLDAS.Global.matrix.name<-list(GLDAS.MAT.coef.matrix_p, GLDAS.MAT.p.matrix_p, GLDAS.MAP.coef.matrix_p, GLDAS.MAP.p.matrix_p,
                               GLDAS.AET.coef.matrix_p, GLDAS.AET.p.matrix_p)

GLDAS.Global.variable.name<-c("GLDAS.MAT.coef.matrix_p", "GLDAS.MAT.p.matrix_p", "GLDAS.MAP.coef.matrix_p", "GLDAS.MAP.p.matrix_p",
                              "GLDAS.AET.coef.matrix_p", "GLDAS.AET.p.matrix_p")



####
GLDAS.Global.raster.matrix <- raster(GLDAS.Global.matrix.name[[2]])

xy <- extent(-180,180,-90,90)
projection(GLDAS.Global.raster.matrix) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    
extent(GLDAS.Global.raster.matrix) <- xy

#spplot(GLDAS.Global.raster.matrix)
#GLDAS.Global.raster.matrix[GLDAS.Global.raster.matrix[] < -1 | GLDAS.Global.raster.matrix[] > 1] <- NA
#OutPath <- "F:/New PHD results/test"
writeRaster(GLDAS.Global.raster.matrix, "F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/GLDAS.RZSM.MAT.coef.tif",
            format="GTiff",overwrite= TRUE)
###################################################################################################
###################################################################################################



#### 3: MERRA-2--------------------------------------------------------------------------------------
##### load CRU climate data-------------------------------------------
setwd("E:/New PHD/Study data/CRU climate/Annual")

cru.list <- list.files(pattern = ".nc")
cru.list

cru.data <- lapply(cru.list, brick)
cru.data

MAT <- mask(stack(cru.data[[3]])[[81:117]],  lc.tif, maskvalue = NA) #81:117: from 1981 to 2017
names(MAT) <- c(1981:2017)

MAP <- mask(stack(cru.data[[2]])[[81:117]],  lc.tif, maskvalue = NA)
names(MAP) <- c(1981:2017)

##### load ET (GLEAM) data------------------------------------

AET.file <- brick("E:/New PHD/Study data/ET/GLEAM_AET/Year/Half_degree/E_1980_2020_GLEAM_v3.5a_0.5_yearly_AET.nc")
names(AET.file) <- c(1980:2020)  #[[2:38]]
#spplot(AET[[1]])
AET <- mask(AET.file[[2:38]], lc.tif, maskvalue = NA) #1981:2017-[[2:38]]
#################################

## yearly RZSM
setwd("E:/New PHD/Study data/SM_0_1m_Halfdegree_m3_m-3")

MERRA2 <- brick("MERRA2_SM_0_1m_0.5degree_yearly_1981_2017.nc") %>% mask(lc.tif, maskvalue=NA)# m3 m-3


##### MERRA2 RZSWS correlated with tmp, pre, AET--------------------------------
Global.MERRA.array <- as.array(MERRA2)
MAT.array<-as.array(MAT)
MAP.array<-as.array(MAP)
AET.array<-as.array(AET)


MERRA.MAT.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
MERRA.MAT.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

MERRA.MAP.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
MERRA.MAP.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

MERRA.AET.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
MERRA.AET.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])



for (row in c(1:dim(MAP.array)[1])){
  for (col in c(1:dim(MAP.array)[2])){
    Global.MERRA.na.len<-length(which(is.na(Global.MERRA.array[row,col,])))
    MAT.na.len<-length(which(is.na(MAT.array[row,col,])))
    MAP.na.len<-length(which(is.na(MAP.array[row,col,])))
    AET.na.len<-length(which(is.na(AET.array[row,col,])))
    
    
    if ((Global.MERRA.na.len==0)&(MAT.na.len==0)&(MAP.na.len==0)&(AET.na.len==0)) {
      Global.MERRA<-Global.MERRA.array[row,col,]
      Global.MAT<-MAT.array[row,col,]
      Global.MAP<-MAP.array[row,col,]
      Global.AET<-AET.array[row,col,]
      
      MERRA_Global_df_p <- data.frame(Global.MERRA, Global.MAT, Global.MAP, Global.AET)
      if(mean(MERRA_Global_df_p$Global.MAP) !=  MERRA_Global_df_p$Global.MAP[1]){
        colnames(MERRA_Global_df_p)<-c("Global.MERRA","MAT","MAP","AET") 
        
        MERRA.p.cor.Global.MAT <- pcor.test(MERRA_Global_df_p$Global.MERRA,MERRA_Global_df_p$MAT,MERRA_Global_df_p[,c("MAP","AET")])
        MERRA.MAT.coef.matrix_p[row,col]<-MERRA.p.cor.Global.MAT$estimate
        MERRA.MAT.p.matrix_p[row,col]<-MERRA.p.cor.Global.MAT$p.value
        
        MERRA.p.cor.Global.MAP <- pcor.test(MERRA_Global_df_p$Global.MERRA,MERRA_Global_df_p$MAP,MERRA_Global_df_p[,c("MAT","AET")])
        MERRA.MAP.coef.matrix_p[row,col]<-MERRA.p.cor.Global.MAP$estimate
        MERRA.MAP.p.matrix_p[row,col]<-MERRA.p.cor.Global.MAP$p.value
        
        MERRA.p.cor.Global.AET <- pcor.test(MERRA_Global_df_p$Global.MERRA,MERRA_Global_df_p$AET,MERRA_Global_df_p[,c("MAT","MAP")])
        MERRA.AET.coef.matrix_p[row,col]<-MERRA.p.cor.Global.AET$estimate
        MERRA.AET.p.matrix_p[row,col]<-MERRA.p.cor.Global.AET$p.value
        
      }
      
      
    }
  }  
  
}


MERRA.Global.matrix.name<-list(MERRA.MAT.coef.matrix_p, MERRA.MAT.p.matrix_p, MERRA.MAP.coef.matrix_p, MERRA.MAP.p.matrix_p,
                               MERRA.AET.coef.matrix_p, MERRA.AET.p.matrix_p)

MERRA.Global.variable.name<-c("MERRA.MAT.coef.matrix_p", "MERRA.MAT.p.matrix_p", "MERRA.MAP.coef.matrix_p", "MERRA.MAP.p.matrix_p",
                              "MERRA.AET.coef.matrix_p", "MERRA.AET.p.matrix_p")



####
MERRA.Global.raster.matrix <- raster(MERRA.Global.matrix.name[[1]])

xy <- extent(-180,180,-90,90)
projection(MERRA.Global.raster.matrix) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    
extent(MERRA.Global.raster.matrix) <- xy

#spplot(MERRA.Global.raster.matrix)
#MERRA.Global.raster.matrix[MERRA.Global.raster.matrix[] < -1 | MERRA.Global.raster.matrix[] > 1] <- NA
#OutPath <- "F:/New PHD results/test"
writeRaster(MERRA.Global.raster.matrix, "F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/MERRA2.RZSM.MAT.coef.tif",
            format="GTiff",overwrite= TRUE)

#######################################################################
#######################################################################




#### 4: Model mean data--------------------------------------------------------------------------------------
##### load CRU climate data-------------------------------------------
setwd("E:/New PHD/Study data/CRU climate/Annual")

cru.list <- list.files(pattern = ".nc")
cru.list

cru.data <- lapply(cru.list, brick)
cru.data

MAT <- mask(stack(cru.data[[3]])[[81:117]],  lc.tif, maskvalue = NA) #81:117: from 1981 to 2017
names(MAT) <- c(1981:2017)

MAP <- mask(stack(cru.data[[2]])[[81:117]],  lc.tif, maskvalue = NA)
names(MAP) <- c(1981:2017)

##### load ET (GLEAM) data------------------------------------

AET.file <- brick("E:/New PHD/Study data/ET/GLEAM_AET/Year/Half_degree/E_1980_2020_GLEAM_v3.5a_0.5_yearly_AET.nc")
names(AET.file) <- c(1980:2020)  #[[2:38]]
#spplot(AET[[1]])
AET <- mask(AET.file[[2:38]], lc.tif, maskvalue = NA) #1981:2017-[[2:38]]
##########################################################################


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



#####  partial correlation for global SM with MAT, MAP and AET----------------

Global.sm.array<-as.array(Model.mean)
MAT.array<-as.array(MAT)
MAP.array<-as.array(MAP)
AET.array<-as.array(AET)



MAT.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
MAT.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

MAP.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
MAP.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])

AET.coef.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])
AET.p.matrix_p<-matrix(NA,nrow=dim(MAP.array)[1],ncol = dim(MAP.array)[2])



for (row in c(1:dim(MAP.array)[1])){
  for (col in c(1:dim(MAP.array)[2])){
    Global.sm.na.len<-length(which(is.na(Global.sm.array[row,col,])))
    MAT.na.len<-length(which(is.na(MAT.array[row,col,])))
    MAP.na.len<-length(which(is.na(MAP.array[row,col,])))
    AET.na.len<-length(which(is.na(AET.array[row,col,])))
    
    
    if ((Global.sm.na.len==0)&(MAT.na.len==0)&(MAP.na.len==0)&(AET.na.len==0)) {
      Global.sm<-Global.sm.array[row,col,]
      MAP<-MAP.array[row,col,]
      MAT<-MAT.array[row,col,]
      AET<-AET.array[row,col,]
      
      Global_df_p <- data.frame(Global.sm,MAT,MAP,AET)
      colnames(Global_df_p)<-c("Global.sm","MAT","MAP","AET")
      
      p.cor.Global.sm.MAT <- pcor.test(Global_df_p$Global.sm,Global_df_p$MAT,Global_df_p[,c("MAP","AET")])
      MAT.coef.matrix_p[row,col]<-p.cor.Global.sm.MAT$estimate
      MAT.p.matrix_p[row,col]<-p.cor.Global.sm.MAT$p.value
      
      p.cor.Global.sm.MAP <- pcor.test(Global_df_p$Global.sm,Global_df_p$MAP,Global_df_p[,c("MAT","AET")])
      MAP.coef.matrix_p[row,col]<-p.cor.Global.sm.MAP$estimate
      MAP.p.matrix_p[row,col]<-p.cor.Global.sm.MAP$p.value
      
      p.cor.Global.sm.AET <- pcor.test(Global_df_p$Global.sm,Global_df_p$AET,Global_df_p[,c("MAT","MAP")])
      AET.coef.matrix_p[row,col]<-p.cor.Global.sm.AET$estimate
      AET.p.matrix_p[row,col]<-p.cor.Global.sm.AET$p.value
    }
  }  
  
}



Global.matrix.name<-list(MAT.coef.matrix_p, MAT.p.matrix_p, MAP.coef.matrix_p, MAP.p.matrix_p,
                         AET.coef.matrix_p, AET.p.matrix_p)

Global.variable.name<-c("MAT.coef.matrix_p", "MAT.p.matrix_p", "MAP.coef.matrix_p", "MAP.p.matrix_p",
                        "AET.coef.matrix_p", "AET.p.matrix_p")



####
Global.raster.matrix <- raster(Global.matrix.name[[6]])

xy <- extent(-180,180,-90,90)
projection(Global.raster.matrix) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    
extent(Global.raster.matrix) <- xy


Global.raster.matrix[Global.raster.matrix[] < -1 | Global.raster.matrix[] > 1] <- NA
#OutPath <- "F:/New PHD results/test"
writeRaster(Global.raster.matrix, "F:/New PHD results/SM_variations_result_end/SM_Partial_correlation/Model.mean.RZSM.AET.P.tif",
            format="GTiff",overwrite= TRUE)

#################################################################################################################
#################################################################################################################


