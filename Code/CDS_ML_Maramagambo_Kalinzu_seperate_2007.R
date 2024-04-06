
### Author: Samuel Ayebare
### Script: CDS_ML_Maramagambo_Kalinzu_seperate_2007.R

### chimpanzees
### Maramagambo and Kalinzu Forest Reserves - analysed separately

## Description  : To format the data
##              : Run a conventional distance sampling model - Maximum likelihood inference
##              : Estimate population density for the 2007 survey

#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd(".../Data")

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
Kal.Mara <- read.csv ("Kalinzu_Mara_R2_2007.csv", header = T)
head(Kal.Mara)
summary(Kal.Mara)

#------------------------------#
## Filter Maramagambo FR
#------------------------------#
Mara_R2 <- filter(Kal.Mara, Region.Label == "Mara")
head(Mara_R2)

#------------------------------#
## Filter Kalinzu FR
#------------------------------#
Kal_R2 <- filter(Kal.Mara, Region.Label == "Kalinzu")
head(Kal_R2)


#-----------------------------------------------#
## import Maramagambo data for O2 & S2 analysis
#-----------------------------------------------#
Mara_S2_O2 <- read.csv ("Mara_S2_O2_2007.csv", header = T)
head(Mara_S2_O2)

#-----------------------------------------------#
## import kalinzu data for O2 & S2 analysis
#-----------------------------------------------#
Kal_S2_O2 <- read.csv ("Kalinzu_S2_O2_2007.csv", header = T)
head(Kal_S2_O2)

#-----------------------------------------------#
####  Maramagambo Forest Reserve -- 2007
#-----------------------------------------------#

### Estimate density of chimpanzees in Maramagambo FR using the R2 variance estimator
## Plot perpendicular distance data

hist(Mara_R2$distance, xlab="Distance (m)", main="Chimp line transects")

#-----------------------------------------------#
## Specify unit conversions
#-----------------------------------------------#
conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#-----------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 25m
#-----------------------------------------------#

det.R2.m <- ds(Mara_R2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )

#-----------------------------------------------#
## Plot half normal detection function
#-----------------------------------------------#
plot(det.R2.m, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))


#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#

summary(det.R2.m)

#-----------------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#-----------------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------##

mult2.R2.m <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2.R2.m)


#----------------------------#
### Estimating chimp density 
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
## Truncate perpendicular distance measurements at 25m
#----------------------------------------------------------#

det.S2.O2.m <- ds(Mara_S2_O2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )

#----------------------------------------------------------#
## Plot half normal detection function
#----------------------------------------------------------#

plot(det.S2.O2.m, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))

#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#

summary(det.S2.O2.m)

#----------------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#---------------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------#

mult2.S2.O2.m <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2.S2.O2.m)


#--------------------------------------------#
### Estimate chimp density using S2 variance estimator
#---------------------------------------------#
Mara_S2_O2$Sample.Label <- as.numeric(Mara_S2_O2$Sample.Label)

chimp.ests.S2.m <- dht2(det.S2.O2.m, flatfile=Mara_S2_O2, strat_formula=~1,
                      convert_units=conversion.factor, multipliers=mult2.S2.O2.m,er_est = "S2")


print(chimp.ests.S2.m, report="density")



#-----------------------------------------------#
### Estimate chimp density using O2 variance estimator
#-----------------------------------------------#
Mara_S2_O2$Sample.Label <- as.numeric(Mara_S2_O2$Sample.Label)

chimp.ests.O2.m <- dht2(det.S2.O2.m, flatfile=Mara_S2_O2, strat_formula=~1,
                      convert_units=conversion.factor, multipliers=mult2.S2.O2.m,er_est = "O2")


print(chimp.ests.O2.m, report="density")





#-----------------------------------------------#
####  Kalinzu Forest Reserve -- 2007
#-----------------------------------------------#



### Estimate density of chimpanzees in Kalinzu FR using the R2 variance estimator
## Plot perpendicular distance data


hist(Kal_R2$distance, xlab="Distance (m)", main="Chimp line transects")


#-----------------------------------------------#
## Specify unit conversions
#-----------------------------------------------#

conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#-----------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 25m
#-----------------------------------------------#

det.R2.k <- ds(Kal_R2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )


#-----------------------------------------------#
## Plot half normal detection function
#-----------------------------------------------#

plot(det.R2.k, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))

#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#


summary(det.R2.k)

#----------------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#---------------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------#
mult2.R2.k <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2.R2.k)

#----------------------------#
### Estimating chimp density 
#----------------------------#
### Using the default encounter rate variance estimator R2

chimp.ests.R2.k <- dht2(det.R2.k, flatfile=Kal_R2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.R2.k)


print(chimp.ests.R2.k, report="density")




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

hist(Kal_S2_O2$distance, xlab="Distance (m)", main="Detection distances")



#-----------------------------------#
## Specify unit conversions
#-----------------------------------#

conversion.factor  <- convert_units("meter", "kilometer", "square kilometer")

#---------------------------------------------------------#
## Fit a half normal detection function with ds function
## Truncate perpendicular distance measurements at 25m
#----------------------------------------------------------#

det.S2.O2.k <- ds(Kal_S2_O2, key ="hn", truncation = 25, adjustment=NULL,convert_units = conversion.factor )

#----------------------------------------------------------#
## Plot half normal detection function
#----------------------------------------------------------#

plot(det.S2.O2.k, main="Half normal detection function", breaks = c(0, 5, 10, 15,20,25))

#----------------------------------------------------------#
### Output a summary of the abundance and density of chimpanzee nests
#----------------------------------------------------------#

summary(det.S2.O2.k)

#----------------------------------------------------------------------------------------------------------------#
### Convert chimpanzee nest density into animal (chimpanzee) density using conversion factors
#---------------------------------------------------------------------------------------------------------------#
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
##---------------------------------------------------------------------------------------------------------------##

mult2.S2.O2.k <- list(creation = data.frame(rate=1.09 *50, SE=0))
print(mult2.S2.O2.k)

#--------------------------------------------#
### Estimate chimp density using S2 variance estimator
#---------------------------------------------#
Kal_S2_O2$Sample.Label <- as.numeric(Kal_S2_O2$Sample.Label)

chimp.ests.S2.k <- dht2(det.S2.O2.k, flatfile=Kal_S2_O2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.S2.O2.k,er_est = "S2")


print(chimp.ests.S2.k, report="density")


#-----------------------------------------------#
### Estimate chimp density using O2 variance estimator
#-----------------------------------------------#
Kal_S2_O2$Sample.Label <- as.numeric(Kal_S2_O2$Sample.Label)

chimp.ests.O2.k <- dht2(det.S2.O2.k, flatfile=Kal_S2_O2, strat_formula=~1,
                        convert_units=conversion.factor, multipliers=mult2.S2.O2.k,er_est = "O2")


print(chimp.ests.O2.k, report="density")






