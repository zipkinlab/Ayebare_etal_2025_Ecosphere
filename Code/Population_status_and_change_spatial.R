
## Authors: Neil Gilbert & Samuel Ayebare
##Script: Population_status_and_change_spatial.R


## Description : To estimate the status and change in population density of chimpanzees and elephant - 2021 to 2007
  


#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd(".../Data")


#----------------#
#-Load libraries-#
#----------------#

library(sf)
library(raster)

##---------------###
### Champanzees
##---------------###

##---------------###
### Modified model-- Hierarchical distance sampling model 
##---------------###
### 2007 - 2021

load("Mara.Kal_2007_2021_chimps_mhds.RData")

##---------------###
#### Standard model  - Hierarchical distance sampling model 
##---------------###
### 2021 

load("Mara.Kal_2021.V23_chimps_hds.RData")

#####----------------------------------------------------------------------------------###

##---------------###
### Elephants
##---------------###

##---------------###
### Modified model    -- Hierarchical distance sampling model 
##---------------###
### 2007 - 2021

load("Eles_2007_2021_mara_mhds.RData")

##---------------###
#### Standard model  - Hierarchical distance sampling model 
##---------------###
### 2021
#### 2021 - Hierarchical distance sampling model 

load("Eles_2021_mara_hds.RData")



#----------------------------- #
# Importing protected area boundaries #
#---------------------------- #

### Kalinzu - Maramagambo
Kal.mara <- st_read ("Maramagambo_Kalinzu_FRs.shp")

plot(Kal.mara)

## Maramagambo
Mara <- st_read ("Maramagambo_FR.shp")
plot(Mara["Area"] )



#----------------------------- #
# Importing elevation  #
#---------------------------- #
### Both protected areas
elev  <-raster("elev_kal.mara.tif")
elev
plot(elev)


### Scale evelation

elev.sc <- scale(elev)
plot(elev.sc)


### Clip elevation for only maramagambo
## crop and mask
elev.m <- crop(elev, extent(Mara)) 
mara_elev <- mask(elev.m, Mara)
plot(mara_elev)

## Standandize Maramagambo elevation

elev.sc.m <- scale(mara_elev)

## plot

plot (elev.sc.m )

##------------------------------------------------###
##### Modified hierarchical distance sampling model
##------------------------------------------------###

##### Using visits - 1, 2, 3 of the marked sign count method
#### Both forest reserves

#### Chimpanzees

### 2007

### Calculate expected density of chimps in 2007



d2007_chimps <- exp(Mara.Kal_2007_2021_chimps$mean$beta0.V23.2007  + Mara.Kal_2007_2021_chimps$mean$beta1.07  * elev.sc +
                      Mara.Kal_2007_2021_chimps$mean$beta2.07  * elev.sc^2) /(1.09 * 50)
plot(d2007_chimps)

### export expected density of chimps in 2007 as GeoTIFF

writeRaster(d2007_chimps,'d2007_chimps_mhds.tif',overwrite=TRUE,options=c('TFW=YES'))



##------------------------------------------------###
###                    2021
##------------------------------------------------###

### Calculate expected density of chimps in 2021

d2021_chimps <- exp(Mara.Kal_2007_2021_chimps$mean$beta0.V23.2021  + Mara.Kal_2007_2021_chimps$mean$beta1.21  * elev.sc +
                      Mara.Kal_2007_2021_chimps$mean$beta2.21  * elev.sc^2)/ (1.09 * 49)


plot(d2021_chimps)

### export expected density of chimps in 2007 as GeoTIFF

writeRaster(d2021_chimps,'d2021_chimps_mhds.tif',overwrite=TRUE,options=c('TFW=YES'))

##------------------------------------------------###
### Estimate change in expected chimpanzee population density 
### between the 2021 and 2007 surveys
##------------------------------------------------###
pop.change_chimps_2021_2007_mhds <- d2021_chimps - d2007_chimps
plot (pop.change_chimps_2021_2007_mhds)


##------------------------------------------------###
### export change in expected chimpanzee population density between
### 2021 - 2007 as GeoTIFF
##------------------------------------------------###
writeRaster(pop.change_chimps_2021_2007_mhds,'Change_in_density_chimps_21_07.tif',overwrite=TRUE,options=c('TFW=YES'))






##------------------------------------------------###
##### Hierarchical distance sampling model
##------------------------------------------------###

##### Using visits - 2, 3 of the marked sign count method
#### Both forest reserves

#### Chimpanzees


##------------------------------------------------###
###                    2021
##------------------------------------------------###


d2021_chimps.hds <- exp(Mara.Kal_2021.V23.cov$mean$beta0  + Mara.Kal_2021.V23.cov$mean$beta1  * elev.sc +
                          Mara.Kal_2021.V23.cov$mean$beta2  * elev.sc^2)/ (1.09 * 49)

plot(d2021_chimps.hds)

##------------------------------------------------###
### export expected density of chimps in 2007 as GeoTIFF
##------------------------------------------------###
writeRaster(d2021_chimps.hds,'d2021_chimps_hds.tif',overwrite=TRUE,options=c('TFW=YES'))





##------------------------------------###
## Elephants ##
##------------------------------------###

##------------------------------------------------###
###                    2007
##------------------------------------------------###

### Calculate expected density of elephants in 2007


d2007_eles <- exp(Eles_2007_2021_mara$mean$beta0.V23.2007  + Eles_2007_2021_mara$mean$beta1.07  * elev.sc.m +
                    Eles_2007_2021_mara$mean$beta2.07  * elev.sc.m^2)/ (17 * 50)


plot(d2007_eles)

##------------------------------------------------###
### export expected density of elephants in 2007 as GeoTIFF
##------------------------------------------------###
writeRaster(d2007_eles,'d2007_eles_mhds.tif',overwrite=TRUE,options=c('TFW=YES'))



##------------------------------------------------###
###                    2021
##------------------------------------------------###

### Calculate expected density of elephants in 2021


d2021_eles <- exp(Eles_2007_2021_mara$mean$beta0.V23.2021  + Eles_2007_2021_mara$mean$beta1.21  * elev.sc.m +
                    Eles_2007_2021_mara$mean$beta2.21  * elev.sc.m^2)/ (17 * 49)


plot(d2021_eles)

writeRaster(d2007_eles,'d2021_eles_mhds.tif',overwrite=TRUE,options=c('TFW=YES'))

##-------------------------------------------------------###
### Estimate change in expected elephant population density 
### between the 2021 and 2007 surveys
##-------------------------------------------------------###
pop.change_eles_2021_2007_mhds <- d2021_eles - d2007_eles
plot (pop.change_eles_2021_2007_mhds)


##-------------------------------------------------------###
### export change in expected chimpanzee population density between
### 2021 - 2007 as GeoTIFF
##-------------------------------------------------------###

writeRaster(pop.change_eles_2021_2007_mhds,'Change_in_density_eles_21_07.tif',overwrite=TRUE,options=c('TFW=YES'))



##------------------------------------------------###
##### Hierarchical distance sampling model
##------------------------------------------------###

##### Using visits - 2, 3 of the marked sign count method
#### Both forest reserves

#### Elephants


##------------------------------------------------###
###                    2021
##------------------------------------------------###

##-------------------------------------------------------###
### Calculate expected density of chimps in 2021
##-------------------------------------------------------###


d2021_eles.hds <- exp(Mara.V23_2021_eles.cov$mean$beta0  + Mara.V23_2021_eles.cov$mean$beta1  * elev.sc.m +
                        Mara.V23_2021_eles.cov$mean$beta2  * elev.sc.m^2)/ (17 * 49)

plot(d2021_eles.hds)


##-------------------------------------------------------###
### export expected density of chimps in 2007 as GeoTIFF
##-------------------------------------------------------###
writeRaster(d2021_eles.hds,'d2021_eles_hds.tif',overwrite=TRUE,options=c('TFW=YES'))



