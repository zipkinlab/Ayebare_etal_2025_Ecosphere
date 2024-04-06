### Author: Samuel Ayebare
### Script: HDS_Maramagambo_Kalinzu_chimpanzees_2021.R

### Chimpanzees
### Maramagambo and Kalinzu Forest Reserves combined

## Description : To format the data
##             : Run a hierarchical distance sampling model 
##             : Estimate population density for the 2021 survey

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
## 2021 survey ##
#------------------#


#------------------------------#
#-Import chimp nest count data-#
#------------------------------#

### Chimp nest distance observations- second & third visits##
V23 <- read.csv("Kalinzu_mara_V23_2021.csv", header=TRUE)
head(V23)


### Chimp nest observations- per transect - Visits 1, 2, & 3
V123 <- read.csv("Kalinzu_mara_2021.csv", header=TRUE)

#------------------#
## Covariates
#------------------#

# Note: Annual mean precipitation and temperature covariates were not used in the model


#----------------------#
#-Import covariate data#
#----------------------#

Pred.var <- read.csv("Pred_var.csv", header=TRUE)

head(Pred.var)

#-------------------------#
## Scale elevation data
#-------------------------#
elev <- (Pred.var$elev - mean(Pred.var$elev))/sd(Pred.var$elev)

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
Rd23_obs <- (filter(Rd23, dist <= 5)) ## Distance category 5 corresponds to 25m of transect width
dim(Rd23_obs)
head(Rd23_obs)
summary(Rd23_obs)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23 <- uncount(Rd23_obs, cnest)
dim(obs_Rd23 )
head(obs_Rd23 )


#---------------------------------------#
## Chimpanzee nests per transect- abundance model
#---------------------------------------#
Tnests <- as_tibble(V123)
head(Tnests)
dim(Tnests)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 &3
#-------------------------------------------------------#
V23.2021 <- Tnests$V23

#----------------------------#
## Area per transect - offset
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
delta23 <- 5  #  max distance meters

#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23 <- 25  #  #  # Effective strip width 

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




# Bundle data (2021 survey) into a list
data_V23.cov <- list(nsites=nsites, nind23=nind23, B23=B23, db23=db23, midpt23=midpt23,
                     delta23=delta23,dclass23=dclass23, V23.2021 = V23.2021, A = A, elev = elev) 


# BUGS model specification
cat("
model{
## scale parameter for the half normal detection function
sigma23 ~ dunif(1, 25)

### Intercept- visits 2 & 3
beta0 ~ dnorm(0,0.1)

### Effect of elevation- visits 2 & 3
beta1 ~ dnorm(0,0.1)
beta2 ~ dnorm(0,0.1)

#Overdispersion
r.N23 ~ dunif(1,3)            


### Categorical distribution observation model : V23 - 2021
for(i in 1:nind23){
   dclass23[i] ~ dcat(fc23[]) 
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
  ### Abundance model 2021
  ##--------------------##

  for(s in 1:nsites){
     V23.2021[s] ~ dbin(pcap23, N23[s])   # Part 2 of HM
     N23[s] ~ dpois(lambda.star23[s])         # Part 3 of HM
     log(lambda23[s]) <- beta0 + beta1 * elev[s] + beta2 * pow(elev[s], 2) + log(A[s]) # linear model abundance

  
  
    #Negative binomial formulation
    lambda.star23[s] <- rho23[s] * lambda23[s]

    #Overdispersion parameter for Expected nests
    rho23[s] ~ dgamma(r.N23, r.N23)
  
}

# Derived parameters
##-------------------------

###Total number of chimpanzee nests observed in the surveyed area corrected for imperfect detection
###------------------------------------------------------------------------------------##
Mara_Kalinzu.V23  <- sum(N23[])

##---------------------------##
### Realized density
##---------------------------##
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 49; represents the number of days (i.e., the time interval between the first, second, and third visits
## 7; represents the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##
DMara_Kalinzu23 <- Mara_Kalinzu.V23  /(1.09*49* 7)

##--------------------------------------------------##
# Total number of expected signs in the surveyed area
##---------------------------------------------------#
Mara_Kalinzu.lambda23 <- sum(lambda23[])

}
",fill=TRUE, file = "Kalinzu.mara.V23_2021_cov.txt")




# Initial values
Nst23 <- V23.2021 + 1

inits <- function(){list(beta0=runif(1, 0, 1),beta1=runif(1, 0, 1), beta2=runif(1, 0, 1),  N23=Nst23, sigma23=runif(1,1,10))}

# Params to save
params <- c( "beta0", "beta1",  "beta2", "N23", "lambda23", "lambda.star23", "Mara23",
             "DMara_Kalinzu23", "Mara_Kalinzu.lambda23","pcap23", "sigma23")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Mara.Kal_2021.V23.cov <- jags(data_V23.cov, inits, params, "Kalinzu.mara.V23_2021_cov.txt", n.thin=nt,
                              n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)

Mara.Kal_2021.V23.cov

### save output

save(Mara.Kal_2021.V23.cov, file = "Mara.Kal_2021.V23_chimps_hds.RData")


### Write output as a .csv

Mara.Kal_2021_V23_cov  <- Mara.Kal_2021.V23.cov$summary

write.csv(Mara.Kal_2021_V23_cov, "Mara.Kal.V23_2021_output_cov_q.csv")

















