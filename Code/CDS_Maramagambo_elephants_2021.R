### Author: Samuel Ayebare
### Script:  CDS_Maramagambo_elephants_2021.R

### elephants
### Maramagambo Forest Reserve

## Description  : To format the data
##              : Run a conventional distance sampling model 
##              : Estimate population density for the 2021 survey
  
#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd(".../Data")

#----------------#
#-Load libraries-#
#----------------#

library(jagsUI)
library(parallel)
library(tidyverse)

### Clear working environment
rm(list=ls())


#------------------#
## 2021 survey ##
#------------------#)


#------------------------------#
#-Import elephant dung count data-#
#------------------------------#
# Note: Transects 1 to 36 are located in Maramagambo Forest Reserve


### Chimp nest distance observations- second & third visits##
V23.2021 <- read.csv("Ele_V23_2021.csv", header=TRUE)
head(V23.2021)

### Chimp nest observations- aggregated per transect
V123.2021 <- read.csv("Ele_2021.csv", header=TRUE)
head(V123.2021)



######## Visit 2 & 3  ##########
################################
Rd23 <- tibble::as_tibble(V23.2021)
head(Rd23)
dim(Rd23)


# Trucant the data: Visit 2&3
Rd23_obs <- (filter(Rd23, dist <= 5))  ## Distance category 5 corresponds to 5m of transect width
dim(Rd23_obs)
head(Rd23_obs)
summary(Rd23_obs)

# creating observation data: VisIt 1
obs_Rd23 <- uncount(Rd23_obs, eldung)
dim(obs_Rd23 )
head(obs_Rd23)



########## eldung  per transect- ecological model#################
############################################################
eledung <- as_tibble(V123.2021)
head(eledung)
dim(eledung)


##### Number of nests detected per transect- Visit 2 &3####
##########################################################
V23 <- eledung$V23

## Area per transect - offset
A <- eledung$A

#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

# distance class per observation
dclass23 <- obs_Rd23$dist

#Width of distance classes
delta23 <- 1  #  meter

# perpendicular distance meters
B23 <- 5  #  # Effective strip width

#Distance class midpoint ID
midpt23 <-  c(0.5,1.5,2.5,3.5,4.5)

# Total number of nests observed

nind23 <- length(dclass23)

#Number of distance classes
db23 <- length(midpt23)

#Number of survey transects
nsites <- 36




# Bundle data set
data_V23_cds <- list(nsites=nsites, nind23=nind23, B23 = B23, V23 =V23, db23 =db23, midpt23=midpt23,
                 delta23=delta23,dclass23=dclass23, A= A) 


# BUGS model specification 
cat("
model{
# Priors
## scale parameter for the half normal detection function
sigma23 ~ dunif(1, 5)


for(i in 1:nind23){
   dclass23[i] ~ dcat(fc23[]) # Part 1 of HM
}


# Construct cell probabilities for nD multinomial cells
  for(g in 1:db23){                 # midpt = mid-point of each cell
    log(p23[g]) <- -midpt23[g] * midpt23[g] / (2*sigma23*sigma23)
    pi23[g] <- delta23/ B23          # probability per interval 
    f23[g] <- p23[g] * pi23[g]
    fc23[g] <- f23[g] / pcap23
  }
  pcap23 <- sum(f23[])            # Average probability of detection 




# Derived parameters
##-------------------------

###Total number of elephant dung observed in the surveyed area corrected for imperfect detection
###------------------------------------------------------------------------------------##
N23 <- nind23/pcap23

## Number of elephant dung per transect after accounting for imperfect detection 
Tr <- V23 /pcap23

##Elephant density per transect
Density_Tr <- Tr/ (A *17*49)


##---------------------------##
### Realized density
##---------------------------##
## We assumed 17 elephant dung piles are produced by each adult elephant per day (Wing and Buss 1970)  
## 49; represents the number of days (i.e., the time interval between the first, second, and third visits
## 1.09328; represents the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##


DMara23 <- N23 /(17 *49* 1.09328)


}
",fill=TRUE, file = "Mara.V23_2021_cds.txt")




# Inits
inits <- function(){list( sigma23=runif(1,1,5))}

# Params to save
params <- c(  "N23","Tr", "Density_Tr", "DMara23", "pcap23", "sigma23")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS  and summarize posteriors

Mara.V23_2021.cds <- jags(data_V23_cds, inits, params, "Mara.V23_2021_cds.txt", n.thin=nt,
                                  n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)


### Note: There is a warning "At least one Rhat value could not be calculated"
#### This occurs because at some transects there were no nest observations were made
Mara.V23_2021.cds


### save output
save(Mara.V23_2021.cds, file = "Eles_2021_mara_cds.RData")


### Write output as a .csv

Mara.2021_V23_cds  <- Mara.V23_2021.cds$summary

write.csv(Mara.2021_V23_cds, "Mara_V23_2021_output_cds_eles.csv")








