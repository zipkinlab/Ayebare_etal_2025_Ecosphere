
### Author: Samuel Ayebare
### Script: CDS_ML_Maramagambo_Kalinzu_combined_2007.R

### chimpanzees
### Maramagambo and Kalinzu Forest Reserves combined

## Description  : To format the data
##              : Run a conventional distance sampling model - Maximum likelihood inference
##              : Estimate population density for the 2007 survey

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
## 2007 survey ##
#------------------#)

#------------------------------#
#-Import chimpanzee nest data-# ---- to apply R2 variance estimator
#------------------------------#

kalinzu_mara_R2 <- read.csv ("Kalinzu_Mara_combined_R2_2007.csv", header = T)
head(kalinzu_mara_R2)

#------------------------------#
## Plot perpendicular distance data
#------------------------------#
hist(kalinzu_mara_R2$distance, xlab="Distance (m)", main="Chimp line transects")


#------------------------------#
## Specify unit conversions
#------------------------------#
conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#----------------------------------------------------------#
## Fit a half normal detection function with ds function
#----------------------------------------------------------#
## Truncate perpendicular distance measurements at 25m

det.R2 <- ds(kalinzu_mara_R2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )

#----------------------------------------------------------#
## Plot half normal detection function
#----------------------------------------------------------#
plot(det.R2, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))

#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#
summary(det.R2)

#-----------------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#-----------------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------##

mult2 <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2)

#----------------------------#
### Estimating chimp density 
#----------------------------#

### Using the default encounter rate variance estimator R2
chimp.ests.R2 <- dht2(det.R2, flatfile=kalinzu_mara_R2, strat_formula=~1,
                   convert_units=conversion.factor, multipliers=mult2)


print(chimp.ests.R2, report="density")



                             #-----------------------------------#
                            ###Encounter rate variance estimators ## S2 and O2 
                             #-----------------------------------#
### Adjacent transects were grouped manually into different strata - to enable application of the "O2" and "S2" variance estimators
## Here are links about post-stratification
## https://workshops.distancesampling.org/online-course/lecturepdfs/Ch5/L5-1%20Variance%20Estimation%20for%20Systematic%20Designs.pdf
##  https://workshops.distancesampling.org/standrews-2019/intro/practicals/Prac_4_Variance_estimation.pdf




#------------------------------#
#-Import chimpanzee nest data-# ---- to apply S2 and O2  variance estimators
#------------------------------#
kalinzu_mara_S2_O2 <- read.csv ("Kalinzu_Mara_combined_S2_O2_2007.csv", header = T)

head(kalinzu_mara_S2_O2)

#-----------------------------------#
## Plot perpendicular distance data
#-----------------------------------#
hist(kalinzu_mara_S2_O2$distance, xlab="Distance (m)", main="Chimp line transects")

#-----------------------------------#
## Specify unit conversions
#-----------------------------------#
conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")


#---------------------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 25m
#----------------------------------------------------------#
det.S2.O2 <- ds(kalinzu_mara_S2_O2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )

#----------------------------------------------------------#
## Plot half normal detection function
#----------------------------------------------------------#
plot(det.S2.O2, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))


#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#

summary(det.S2.O2)

#----------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#----------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------##

mult2.S2.O2 <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2.S2.O2)

#--------------------------------------------#
### Estimate chimp density using S2 variance estimator
#---------------------------------------------#
kalinzu_mara_S2_O2$Sample.Label <- as.numeric(kalinzu_mara_S2_O2$Sample.Label)

chimp.ests.S2 <- dht2(det.S2.O2, flatfile=kalinzu_mara_S2_O2, strat_formula=~1,
                           convert_units=conversion.factor, multipliers=mult2.S2.O2,er_est = "S2")


print(chimp.ests.S2, report="density")

#-----------------------------------------------#
### Estimate chimp density using O2 variance estimator
#-----------------------------------------------#
kalinzu_mara_S2_O2$Sample.Label <- as.numeric(kalinzu_mara_S2_O2$Sample.Label)

chimp.ests.O2 <- dht2(det.S2.O2, flatfile=kalinzu_mara_S2_O2, strat_formula=~1,
                      convert_units=conversion.factor, multipliers=mult2.S2.O2,er_est = "O2")


print(chimp.ests.O2, report="density")








