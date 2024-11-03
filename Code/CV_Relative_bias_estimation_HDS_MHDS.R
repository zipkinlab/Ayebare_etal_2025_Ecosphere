

### Author: Samuel Ayebare
## CV_Relative_bias_estimation_HDS_MHDS.r

## This script
# estimates: Relative_bias = sum(expected number of signs – observed number of signs )/ sum(observed number of signs)



#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))

#----------------#
#-Load libraries-#
#----------------#



library(tidyverse)


setwd("C:/Spatial_temporal/Quadratic_edit/Revisions/Bias_figures")



### Import data

## Modified hierarchical distance sampling model
## Bias per transect - MHDS 2021
chimps_mhds_2021 <- read.csv("Bias_mhds_2021_chimps_transects.csv", header=TRUE)
head(chimps_mhds_2021 )

## Hierarchical distance sampling model
## Bias per transect - HDS 2021
chimps_hds_2021 <- read.csv("Bias_hds_2021_chimps_transects.csv", header=TRUE)
head(chimps_hds_2021 )

## Modified hierarchical distance sampling model
## Bias per transect - MHDS 2007
chimps_mhds_2007 <- read.csv("Bias_mhds_2007_chimps_transects.csv", header=TRUE)
head(chimps_mhds_2007 )


## Hierarchical distance sampling model
## Bias per transect - HDS 2007
chimps_hds_2007 <- read.csv("Bias_hds_2007_chimps_transects.csv", header=TRUE)
head(chimps_hds_2007 )



############################
####### Chimps 2021
####----------------------####

#### Estimate relative bias for the Modified hierarchical distance sampling model - chimps - 2021
### Relative_bias = sum(expected number of chimp nests – observed number of chimp nests)/ sum(observed number of chimp nests)

### bias_per_transect = expected number of chimp nests – observed number of chimp nests 


Relative_bias_mhds_chimps_2021 <- sum (chimps_mhds_2021$bias_per_transect)/ sum(chimps_mhds_2021$observed_nests)
Relative_bias_mhds_chimps_2021 * 100


#### Estimate relative bias for the hierarchical distance sampling model - chimps - 2021
### Relative_bias = sum(expected number of chimp nests – observed number of chimp nests)/ sum(observed number of chimp nests)

### bias_per_transect = expected number of chimp nests – observed number of chimp nests 


Relative_bias_hds_chimps_2021 <- sum (chimps_hds_2021$bias_per_transect)/ sum(chimps_hds_2021$observed_nests)
Relative_bias_hds_chimps_2021 * 100


      ############################
     ####### Chimps 2007
     ####----------------------####


#### Estimate relative bias for the Modified hierarchical distance sampling model - chimps - 2007
### Relative_bias = sum(expected number of chimp nests – observed number of chimp nests)/ sum(observed number of chimp nests)

### bias_per_transect = expected number of chimp nests – observed number of chimp nests 


Relative_bias_mhds_chimps_2007 <- sum (chimps_mhds_2007$bias_per_transect)/ sum(chimps_mhds_2007$observed_nests)
Relative_bias_mhds_chimps_2007 * 100

#### Estimate relative bias for the hierarchical distance sampling model - chimps - 2007
#### Relative_bias = sum(expected number of chimp nests – observed number of chimp nests)/ sum(observed number of chimp nests)

### bias_per_transect = expected number of chimp nests – observed number of chimp nests 

Relative_bias_hds_chimps_2007 <- sum (chimps_hds_2007$bias_per_transect)/ sum(chimps_hds_2007$observed_nests)
Relative_bias_hds_chimps_2007 * 100






###---------------------------###
##### Elephants
####-------------------------####

### Import data

## Modified hierarchical distance sampling model
## Bias per transect - MHDS 2021
eles_mhds_2021 <- read.csv("Bias_mhds_2021_eles_transects.csv", header=TRUE)
head(eles_mhds_2021 )

## Hierarchical distance sampling model
## Bias per transect - HDS 2021
eles_hds_2021 <- read.csv("Bias_hds_2021_eles_transects.csv", header=TRUE)
head(eles_hds_2021 )


## Modified hierarchical distance sampling model
## Bias per transect - MHDS 2007
eles_mhds_2007 <- read.csv("Bias_mhds_2007_eles_transects.csv", header=TRUE)
head(eles_mhds_2007 )


## Hierarchical distance sampling model
## Bias per transect - HDS 2007
eles_hds_2007 <- read.csv("Bias_hds_2007_eles_transects.csv", header=TRUE)
head(eles_hds_2007 )





############################
####### Elephants 2021
####----------------------####



#### Estimate relative bias for the Modified hierarchical distance sampling model; elephants - 2021
### Relative_bias = sum(expected number of elephant dung – observed number of elephant dung)/ sum(observed number of elephant dung)

### bias_per_transect = xpected number of elephant dung – observed number of elephant dung 


Relative_bias_mhds_eles_2021 <- sum (eles_mhds_2021$bias_per_transect)/ sum(eles_mhds_2021$observed_dung)
Relative_bias_mhds_eles_2021 * 100



#### Estimate relative bias for the hierarchical distance sampling model; elephants - 2021
### Relative_bias = sum(expected number of elephant dung – observed number of elephant dung)/ sum(observed number of elephant dung)

### bias_per_transect = xpected number of elephant dung – observed number of elephant dung 



Relative_bias_hds_eles_2021 <- sum (eles_hds_2021$bias_per_transect)/ sum(eles_hds_2021$observed_dung)
Relative_bias_hds_eles_2021 * 100






############################
####### Elephants 2007
####----------------------####


#### Estimate relative bias for the Modified hierarchical distance sampling model; elephants - 2007
### Relative_bias = sum(expected number of elephant dung – observed number of elephant dung)/ sum(observed number of elephant dung)

### bias_per_transect = xpected number of elephant dung – observed number of elephant dung 

Relative_bias_mhds_eles_2007 <- sum (eles_mhds_2007$bias_per_transect)/ sum(eles_mhds_2007$observed_dung)
Relative_bias_mhds_eles_2007 * 100


#### Estimate relative bias for the Modified hierarchical distance sampling model; elephants - 2007
### Relative_bias = sum(expected number of elephant dung – observed number of elephant dung)/ sum(observed number of elephant dung)

### bias_per_transect = xpected number of elephant dung – observed number of elephant dung 

Relative_bias_hds_eles_2007 <- sum (eles_hds_2007$bias_per_transect)/ sum(eles_hds_2007$observed_dung)
Relative_bias_hds_eles_2007 * 100


