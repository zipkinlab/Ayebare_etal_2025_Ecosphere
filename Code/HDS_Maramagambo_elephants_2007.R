### Author: Samuel Ayebare
### Script: HDS_Maramagambo_elephants_2007.R

### elephants
### Maramagambo Forest Reserve

## Description : To format the data
##             : Run a modified hierarchical distance sampling model 
##             : Estimate population density for the 2007 survey
  
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

library(jagsUI)
library(parallel)
library(tidyverse)

### Clear working environment
rm(list=ls())


#------------------#
## 2007 survey ##
#------------------#)

#------------------------------#
#-Import elephant dung count data-#
#------------------------------#
# Note: Transects 1 to 36 are located in Maramagambo Forest Reserve


### Chimp nest distance observations- second & third visits##
V23 <- read.csv("Ele_V23_2007.csv", header=TRUE)
head(V23)

### Chimp nest observations- aggregated per transect
V123 <- read.csv("Ele_2007.csv", header=TRUE)
head(V123)


#------------------#
## Covariates
#------------------#

# Note: Annual mean precipitation (bio12) and temperature covariates (bio1) were not used in the model


#----------------------#
#-Import covariate data#
#----------------------#
Pred.var <- read.csv("Pred_var.csv", header=TRUE)
head(Pred.var)

#-------------------------------#
## Filter Kalinzu elevation values
#-------------------------------#
elev <- Pred.var$elev[1:36]
elev

#-------------------------#
## Scale elevation data
#-------------------------#

elev <- (elev - mean(elev))/sd(elev)

elev




#---------------#
## Visits 2 & 3  
#--------------#

## Convert visits 2 & 3 data to a tibble
Rd23 <- tibble::as_tibble(V23)
head(Rd23)
dim(Rd23)

#---------------------------#
## Trucante data: Visits 2&3
#----------------------------#
Rd23_obs <- (filter(Rd23, dist <= 5))  ## Distance category 5 corresponds to 5m of transect width
dim(Rd23_obs)
head(Rd23_obs)
sum(Rd23_obs$eldung)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23 <- uncount(Rd23_obs, eldung )
dim(obs_Rd23 )
head(obs_Rd23)


#---------------------------------------#
## elphant dung per transect- abundance model
#---------------------------------------#
eledung <- as_tibble(V123)
head(eledung)
dim(eledung)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 & 3
#-------------------------------------------------------#
V23 <- eledung$V23
sum(V23)

##----------------------------#
## Area per transect - offset
#---------------------------#
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

#--------------------------------------#
## Total number of elephant dung observed
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
nsites <- 36




# Bundle data (2007 survey) into a list
data_V23.cov <- list(nsites=nsites, nind23=nind23, B23=B23, db23=db23, midpt23=midpt23,
                 delta23=delta23,dclass23=dclass23, V23=V23, A = A, elev = elev) 


# BUGS model specification 
cat("
model{
# Priors

## scale parameter for the half normal detection function
sigma23 ~ dunif(1, 5)

### Intercept- visits 2 & 3
beta0 ~ dnorm(0,0.1)

### Effect of elevation- visits 2 & 3
beta1 ~ dnorm(0,0.1)
beta2 ~ dnorm(0,0.1)

#Overdispersion
r.N23 ~ dunif(1,3)            

### Categorical distribution observation model : V23 - 2007
for(i in 1:nind23){
   dclass23[i] ~ dcat(fc23[]) # Part 1 of HM
}


# Construct cell probabilities 
  for(g in 1:db23){                 # midpt = mid-point of each cell
    log(p23[g]) <- -midpt23[g] * midpt23[g] / (2*sigma23*sigma23)
    pi23[g] <- delta23/ B23          # probability per interval 
    f23[g] <- p23[g] * pi23[g]
    fc23[g] <- f23[g] / pcap23
  }
  pcap23 <- sum(f23[])            # Average probability of detection 

  ##--------------------##
  ### Abundance model 2007
  ##--------------------##

for(s in 1:nsites){
  V23[s] ~ dbin(pcap23, N23[s])   # Part 2 of HM
  N23[s] ~ dpois(lambda.star23[s])         # Part 3 of HM
  log(lambda23[s]) <- beta0 + beta1 * elev[s] + beta2 * pow(elev[s], 2) + log(A[s]) # linear model abundance

  
  
  #Expected Number of Groups
    lambda.star23[s] <- rho23[s] * lambda23[s]

    #Overdispersion parameter for Expected Number of Groups
    rho23[s] ~ dgamma(r.N23, r.N23)
  
}


# Derived parameters
##-------------------------
###Total number of elephant dung observed in the surveyed area corrected for imperfect detection
###-------------------------------------------------------------------------------------------##
Mara23 <- sum(N23[])

##---------------------------##
### Realized density
##---------------------------##
## We assumed 17 elephant dung piles are produced by each adult elephant per day (Wing and Buss 1970)  
## 50; represents the number of days (i.e., the time interval between the first, second, and third visits
## 1.0506; represents the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##
DMara23 <- Mara23 /(17 *50* 1.0506)

##--------------------------------------------------##
# Total number of expected elephant dung in the surveyed area
##---------------------------------------------------#
Mara.lambda23 <- sum(lambda23[])

}
",fill=TRUE, file = "Mara.V23_2007_cov_eles.txt")




# Initial values
Nst23 <- V23 + 1
#inits <- function(){list( beta0=runif(1, 0, 1), N=Nst, sigma=runif(1,1,10))}
inits <- function(){list(beta0=runif(1, 0, 1), beta1=runif(1, 0, 1), beta2=runif(1, 0, 1), N23=Nst23, sigma23=runif(1, 1,5))}

# Params to save
params <- c( "beta0","beta1", "beta2", "N23", "lambda23", "lambda.star23", "Mara23", "DMara23", "Mara.lambda23" ,"pcap23", "sigma23")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Mara.V23_2007_eles.cov <- jags(data_V23.cov, inits, params, "Mara.V23_2007_cov_eles.txt", n.thin=nt,
                      n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)

Mara.V23_2007_eles.cov


### save output
save(Mara.V23_2007_eles.cov, file = "Eles_2007_mara_hds.RData")


### Write output as a .csv

Mara.2007_V23_cov  <- Mara.V23_2007_eles.cov$summary

write.csv(Mara.2007_V23_cov, "Mara.V23_2007_output_eles_cov_q.csv")



















