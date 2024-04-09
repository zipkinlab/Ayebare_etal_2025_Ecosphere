
### Author: Samuel Ayebare
### Script:  CDS_ML_Maramagambo__2021_eles.R

### elephants
### Maramagambo Forest Reserve 

## Description  : To format the data
##              : Run a conventional distance sampling model - Maximum likelihood inference
##              : Estimate population density for the 2021 survey

#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))

#----------------#
#-Load libraries-#
#----------------#


library(Distance)
library(tidyverse)

### Clear working environment
rm(list=ls())


#------------------#
## 2021 survey ##
#------------------#)

#------------------------------#
#-Import elephant dung data-#   ---- to apply R2 variance estimator   
#------------------------------#

Mara_R2 <- read.csv ("Elephant_mara_R2_2021.csv", header = T)
head(Mara_R2)


#-----------------------------------------------#
## import elephant dung data for O2 & S2 analysis
#-----------------------------------------------#
Mara_S2_O2 <- read.csv ("Elephant_mara_S2_O2_2021.csv", header = T)
head(Mara_S2_O2)


### Estimate density of chimpanzees in Maramagambo FR using the R2 variance estimator
## Plot perpendicular distance data


hist(Mara_R2$distance, xlab="Distance (m)", main="Chimp line transects")


#-----------------------------------------------#
## Specify unit conversions
#-----------------------------------------------#

conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#-----------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 5m
#-----------------------------------------------#
det.R2.m <- ds(Mara_R2, key ="hn", truncation = 5, adjustment=NULL,convert_units = conversion.factor )

#-----------------------------------------------#
## Plot half normal detection function
#-----------------------------------------------#

plot(det.R2.m, main="Half normal detection function", breaks = c(0, 1, 2, 3,4,5))

#----------------------------------------------------------#
### Output a summary of the abundance and density of elephant dung
#----------------------------------------------------------#

summary(det.R2.m)

#-----------------------------------------------------------------------------------------------------------------#
### Convert elephant dung density into animal (elephant) density using conversion factors
#-----------------------------------------------------------------------------------------------------------------#
## We assumed 17 elephant dung piles are produced by each adult elephant per day (Wing and Buss 1970)  
## 49; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------#######

## Creating a conversion factors (i.e., multipliers)
mult2.R2.m <- list(creation = data.frame(rate=17*49, SE=0))
print(mult2.R2.m)


#----------------------------#
### Estimating elephant density
#----------------------------#
### Using the default encounter rate variance estimator R2

chimp.ests.R2.m <- dht2(det.R2.m, flatfile=Mara_R2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.R2.m)


print(chimp.ests.R2.m, report="density")




#---------------------------------------------------#
### Application of encounter rate variance estimators ## S2 and O2 
#--------------------------------------------------#

### Adjacent transects were grouped manually into different strata - to enable application of the "O2" and "S2" variance estimators
## Here are links about post-stratification
## https://workshops.distancesampling.org/online-course/lecturepdfs/Ch5/L5-1%20Variance%20Estimation%20for%20Systematic%20Designs.pdf
##  https://workshops.distancesampling.org/standrews-2019/intro/practicals/Prac_4_Variance_estimation.pdf


#-----------------------------------#
## Plot perpendicular distance data
#-----------------------------------#

hist(Mara_S2_O2$distance, xlab="Distance (m)", main="Detection distances")


#-----------------------------------#
## Specify unit conversions
#-----------------------------------#
conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#---------------------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 5m
#----------------------------------------------------------#


det.S2.O2.m <- ds(Mara_S2_O2, key ="hn", truncation = 5, adjustment=NULL,convert_units = conversion.factor )

#----------------------------------------------------------#
## Plot half normal detection function
#----------------------------------------------------------#

plot(det.S2.O2.m, main="Half normal detection function",breaks = c(0, 1, 2, 3,4,5))

#----------------------------------------------------------#
### Output a summary of the abundance and density of elephant dung
#----------------------------------------------------------#

summary(det.S2.O2.m)

#-----------------------------------------------------------------------------------------------------------------#
### Convert elephant dung density into animal (elephant) density using conversion factors
#-----------------------------------------------------------------------------------------------------------------#
## We assumed 17 elephant dung piles are produced by each adult elephant per day (Wing and Buss 1970)  
## 49; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------##


mult2.S2.O2.m <- list(creation = data.frame(rate=17 *49, SE=0))
print(mult2.S2.O2.m)


#--------------------------------------------#
### Estimate elephant density using S2 variance estimator
#---------------------------------------------#
Mara_S2_O2$Sample.Label <- as.numeric(Mara_S2_O2$Sample.Label)

chimp.ests.S2.m <- dht2(det.S2.O2.m, flatfile=Mara_S2_O2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.S2.O2.m,er_est = "S2")


print(chimp.ests.S2.m, report="density")



#--------------------------------------------#
### Estimate elephant density using O2 variance estimator
#---------------------------------------------#
Mara_S2_O2$Sample.Label <- as.numeric(Mara_S2_O2$Sample.Label)

chimp.ests.O2.m <- dht2(det.S2.O2.m, flatfile=Mara_S2_O2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.S2.O2.m,er_est = "O2")


print(chimp.ests.O2.m, report="density")







