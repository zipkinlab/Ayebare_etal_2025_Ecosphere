### Author: Samuel Ayebare
### Script: CDS_Kalinzu_chimpanzees_2007.R

### Chimpanzees
### Kalinzu Forest Reserves

## Description : To format the data
##             : Run a conventional distance sampling model 
##             : Estimate population density for the 2007 survey

#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd("..../Data")

#----------------#
#-Load libraries-#
#----------------#

library(jagsUI)
library(parallel)
library(tidyverse)

### Clear working environment
rm(list=ls())


#------------------#
## 2007 survey ##
#------------------#


### Chimp nest distance observations- second & third visits##
V23.2007 <- read.csv("Kalinzu_mara_V23_2007.csv", header=TRUE)
### filter kalinzu observations 
V23.2007 <- filter(V23.2007, Tr > 36)
head(V23.2007)

### Chimp nest observations- per transect - Visits 1, 2, & 3
V123.2007 <- read.csv("Kalinzu_mara_2007.csv", header=TRUE)
V123.2007 <- filter(V123.2007, Tr > 36)
head(V123.2007)



#---------------#
## Visits 2 & 3  
#--------------#

## Convert visits 2 & 3 data to a tibble
Rd23 <- tibble::as_tibble(V23.2007)
head(Rd23)
dim(Rd23)


#---------------------------#
## Trucante data: Visits 2&3
#----------------------------#
Rd23_obs <- (filter(Rd23, dist <= 5))  ## Distance category 5 corresponds to 25m of transect width
dim(Rd23_obs)
head(Rd23_obs)
summary(Rd23_obs)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23 <- uncount(Rd23_obs, cnest)
dim(obs_Rd23 )
head(obs_Rd23)


#---------------------------------------#
## Chimpanzee nests per transect- abundance model
#---------------------------------------#
Tnests <- as_tibble(V123.2007)
head(Tnests)
dim(Tnests)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 &3
#-------------------------------------------------------#
V23 <- Tnests$V23

#----------------------------#
## Area per transect
#---------------------------#
A <- Tnests$A




#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

#--------------------------------#
## distance class per observation
#--------------------------------#
dclass23 <- obs_Rd23$dist

#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23 <- 5  #  meters

# perpendicular distance meters
B23 <- 25  #  Effective strip width

#Distance class midpoint ID
midpt23 <- c(2.5,7.5,12.5,17.5,22.5)

#--------------------------------------#
## Total number of chimp nests observed
## during visits 2 & 3
#--------------------------------------#

nind23 <- length(dclass23)

#---------------------------#
## Number of distance classes
#----------------------------#
db23 <- length(midpt23)


#-----------------------------#
#Number of transects surveyed
#-----------------------------#
nsites <- 46



# Bundle and summarize data set
data_V23_cds <- list(nsites=nsites, nind23=nind23, B23 = B23, V23 =V23, db23 =db23, midpt23=midpt23,
                 delta23=delta23,dclass23=dclass23, A= A) 


# BUGS model specification 
cat("
model{
# Priors
## scale parameter for the half normal detection function
sigma23 ~ dunif(1, 25)

### Categorical distribution observation model : V23 - 2007
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

###Total number of chimpanzee nests observed in the surveyed area corrected for imperfect detection
###------------------------------------------------------------------------------------##
N23 <- nind23/pcap23

## Number of nests per transect after accounting for imperfect detection 
Tr <- V23 /pcap23

## Chimp density per transect
Density_Tr <- Tr/ (A *1.09*50)


##---------------------------##
### Realized density
##---------------------------##
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
## 1.5; represents the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##

DKalinzu23 <- N23 /(1.09*50* 1.5026)


}
",fill=TRUE, file = "Kalinzu.V23_2007_cds.txt")




# Initial values
inits <- function(){list( sigma23=runif(1,1,10))}

# Params to save
params <- c(  "N23","Tr", "Density_Tr", "DKalinzu23", "pcap23", "sigma23")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS  and summarize posteriors

Kalinzu.V23_2007.cds <- jags(data_V23_cds, inits, params, "Kalinzu.V23_2007_cds.txt", n.thin=nt,
                             n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)



### Note: There is a warning "At least one Rhat value could not be calculated"
#### This occurs because at some transects there were no nest observations were made
Kalinzu.V23_2007.cds


### save output
save(Kalinzu.V23_2007.cds, file = "Kal_2007.V23_chimps_cds.RData")


### Write output as a .csv

Kalinzu_2007_V23_cds  <- Kalinzu.V23_2007.cds$summary

write.csv(Kalinzu_2007_V23_cds, "Kalinzu.V23_2007_output_cds.csv")








