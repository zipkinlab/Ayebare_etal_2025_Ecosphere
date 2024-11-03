### Author: Samuel Ayebare
### Script: CV_Maramagambo_Kalinzu_chimpanzees_2021_MHDS.R

### Chimpanzee
### Maramagambo and Kalinzu Forest Reserves

## Description : To format the data
##             : Run a modified hierarchical distance sampling model 
##             : Assess bias by using cross validation (2021 survey)

### ### This code omits transect one
### Same code is used for all the 46 transects

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
## 2021 survey ##
#------------------#


#------------------------------#
#-Import chimp nest count data-#
#------------------------------#

### Chimp nest distance observations- first visit
V1.2021 <- read.csv("Kalinzu_mara_V1_2021.csv", header=TRUE)
head(V1.2021)


### Chimp nest distance observations- second & third visits
V23.2021 <- read.csv("Kalinzu_mara_V23_2021.csv", header=TRUE)
head(V23.2021)

### Chimp nest observations-  per transect
V123.2021 <- read.csv("Kalinzu_mara_2021.csv", header=TRUE)

head(V123.2021)

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



#------------------------------------------#
####      2021 survey                  ####
#------------------------------------------#

#------------------#
## Visit 1 - 2021 
#------------------#

## Convert visit one data to a tibble
Rd1.2021 <- tibble::as_tibble(V1.2021)
head(Rd1.2021)
dim(Rd1.2021)


#-------------------------#
## Truncate data: Visit 1
#-------------------------#
Rd1_obs.2021 <- (filter(Rd1.2021, dist <= 5)) ## Distance category 5 corresponds to 25m of transect width
dim(Rd1_obs.2021)
head(Rd1_obs.2021)
summary(Rd1_obs.2021)

#----------------------------------#
## Create observation data: Visit 1
#---------------------------------#
obs_Rd1.2021 <- uncount(Rd1_obs.2021, cnest)
dim(obs_Rd1.2021 )
head(obs_Rd1.2021)




#####################################################
#---------------#
## Visits 2 & 3  
#--------------#
##########################################################
## Convert visits 2 & 3 data to a tibble
Rd23.2021 <- tibble::as_tibble(V23.2021)
head(Rd23.2021)
dim(Rd23.2021)


#---------------------------#
## Trucante data: Visits 2&3
#----------------------------#
Rd23_obs.2021 <- (filter(Rd23.2021, dist <= 5))  ## Distance category 5 corresponds to 25m of transect width
dim(Rd23_obs.2021)
head(Rd23_obs.2021)
summary(Rd23_obs.2021)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23.2021 <- uncount(Rd23_obs.2021, cnest)
dim(obs_Rd23.2021 )
head(obs_Rd23.2021)

#--------------------------------------#
## Remove perpendicular distance observations for transect - one
## For cross validation
#--------------------------------------#


obs_Rd23.2021 <- filter (obs_Rd23.2021, Tr != 1)
dim(obs_Rd23.2021)
head(obs_Rd23.2021)

#################################################################

#-------------------------#
#-Create distance classes-# Visit 1 - 2021
#-------------------------#
#################################################################


#--------------------------------#
## distance class per observation
#-------------------------------#
dclass1.2021 <- obs_Rd1.2021$dist


#------------------------------------------#
## Width of distance classes (i.e., 5 meters)
#------------------------------------------#
delta1.2021 <- 5  # meters


#------------------------------#
## perpendicular distance meters
#------------------------------#
B1.2021 <- 25   # Effective strip width 


#----------------------------#
## Distance class midpoint ID
#---------------------------#
midpt1.2021 <- c(2.5,7.5,12.5,17.5,22.5)



#--------------------------------------#
## Total number of chimp nests observed
## during visit one
#--------------------------------------#
nind1.2021 <- length(dclass1.2021)


#--------------------------------------#
##Number of distance classes - visit one
#--------------------------------------#
db1.2021 <- length(midpt1.2021)


#-----------------------------#
#Number of transects surveyed
#-----------------------------#
nsites <- 46



#################################################################
  ## #-------------------------#
    #-Create distance classes-# Visits 2&3
 ##-------------------------#
#################################################################


#--------------------------------#
## distance class per observation
#--------------------------------#
dclass23.2021 <- obs_Rd23.2021$dist


#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23.2021 <- 5  #  meters


#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23.2021 <- 25   # Effective strip width 


#--------------------------------#
## Distance class midpoint ID
#--------------------------------#
midpt23.2021 <- c(2.5,7.5,12.5,17.5,22.5)


#--------------------------------------#
## Total number of chimp nests observed
## during visits 2 & 3
#--------------------------------------#
nind23.2021 <- length(dclass23.2021)



#---------------------------#
## Number of distance classes
#----------------------------#
db23.2021 <- length(midpt23.2021)




#---------------------------------------#
## Chimpanzee nests per transect- abundance model
#---------------------------------------#
Tnests.2021 <- as_tibble(V123.2021)
head(Tnests.2021)
dim(Tnests.2021)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 1
#-------------------------------------------------------#
V1.2021 <- Tnests.2021$V1

#-------------------------------------------------------#
## Number of nests detected per transect- Visits 2 &3
#-------------------------------------------------------#
V23.2021 <- Tnests.2021$V23

#-------------------------------------------------------#
## Replace observed nests at transect one with - NA
#-------------------------------------------------------#

V23.2021_tr1 <- replace(V23.2021, 1, NA)
V23.2021_tr1
## Area per transect - offset
A.2021 <- Tnests.2021$A





# Bundle data (2007 & 2021 survey) into a list
mhds_data_V23.tr1 <- list(nsites=nsites, nind1.2021 =nind1.2021, nind23.2021 =nind23.2021, 
                        B1.2021 =B1.2021, B23.2021 = B23.2021, db1.2021 = db1.2021,
                        db23.2021 = db23.2021, midpt1.2021 = midpt1.2021,
                       midpt23.2021 = midpt23.2021,  delta1.2021=delta1.2021, 
                       delta23.2021=delta23.2021,dclass1.2021 =dclass1.2021,
                      dclass23.2021 =dclass23.2021, V1.2021=V1.2021, 
                      V23.2021_tr1 = V23.2021_tr1,A.2021 = A.2021, elev =elev) 




####################################################
######### Joint likelihood- Visit1 & Visits 2,3 
####################################################


# BUGS model specification (# Figure 1)
cat("
model{
# Priors

## scale parameter for the half normal detection function

sigma1.2021 ~ dunif(1, 25)

sigma23.2021 ~ dunif(1, 25)


### Intercept- visits 1, 2 & 3


beta0.V1.2021 ~ dnorm(0,0.1)

beta0.V23.2021 ~ dnorm(0,0.1)


### Effect of elevation- visits 1, 2 & 3

beta1.21 ~ dnorm(0,0.1)
beta2.21 ~ dnorm(0,0.1)


#Overdispersion
 
r.N1.2021 ~ dunif(1,3)    

r.N23.2021 ~ dunif(1,3)            


### Categorical distribution observation model : V1 - 2021

for(i in 1:nind1.2021){
   dclass1.2021[i] ~ dcat(fV1.2021[]) 
}



### Categorical distribution observation model - V23 -  2021

for(i in 1:nind23.2021){
   dclass23.2021[i] ~ dcat(fV23.2021[]) 
}


##-------------##
### 2021 survey
##-------------##

##-------------##
# Visit 1 - 2021 
##-------------##
## Construct cell probabilities 
 ##---------------------------
 
 
  for(g in 1:db1.2021){    
  
    log(p1.2021[g]) <- -midpt1.2021[g] * midpt1.2021[g] / (2*sigma1.2021*sigma1.2021)
    pi1.2021[g] <- delta1.2021/ B1.2021          # probability per interval
    f1.2021[g] <- p1.2021[g] * pi1.2021[g]
    fV1.2021[g] <- f1.2021[g] / pcap1.2021
    
  }
  pcap1.2021 <- sum(f1.2021[])            # Average probability of detection 
  
  
  
 ##-------------------------------------------
# Visit 23 - 2021 ## Construct cell probabilities 
 ##--------------------------------------------
 
  for(g in 1:db23.2021){    
  
    log(p23.2021[g]) <- -midpt23.2021[g] * midpt23.2021[g] / (2*sigma23.2021*sigma23.2021)
    pi23.2021[g] <- delta23.2021/ B23.2021          # probability per interval
    f23.2021[g] <- p23.2021[g] * pi23.2021[g]
    fV23.2021[g] <- f23.2021[g] / pcap23.2021
    
  }
  pcap23.2021 <- sum(f23.2021[])           # Average probability of detection 
  
  


  ##----------------------##
  ### Abundance models 2021
  ##----------------------##
  
  
   
  ##---------------------------
  # Visit 1 - 2021 (submodel)
  ##---------------------------
   
    for(s in 1:nsites){
    
  V1.2021[s] ~ dbin(pcap1.2021, N1.2021[s])   # Part 2 of HM
  N1.2021[s] ~ dpois(lambda.star1.2021[s])         # Part 3 of HM
  log(lambda1.2021[s]) <- beta0.V1.2021 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2021[s]) # linear model abundance

  
  
  #Negative binomial formulation
  lambda.star1.2021[s] <- rho1.2021[s] * lambda1.2021[s]

  #Overdispersion parameter
  rho1.2021[s] ~ dgamma(r.N1.2021, r.N1.2021)
  
  }


  ##---------------------------  
  # Visits 2 & 3 - 2021 (submodel)
  ##---------------------------
   
for(s in 1:nsites){

  V23.2021_tr1[s] ~ dbin(pcap23.2021, N23.2021[s])   # Part 2 of HM
  N23.2021[s] ~ dpois(lambda.star23.2021[s])         # Part 3 of HM
  log(lambda23.2021[s]) <- beta0.V23.2021 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2021[s]) # linear model abundance

  #Negative binomial formulation
  lambda.star23.2021[s] <- rho23.2021[s] * lambda23.2021[s]

  #Overdispersion parameter 
   rho23.2021[s] ~ dgamma(r.N23.2021, r.N23.2021)
  
  }

 



##-------------------------
# Derived parameters
##-------------------------

##--------------------------------------------------##
# Expected number of nests at Transect - one
##---------------------------------------------------#

Expected_no_nest_tr1<- lambda23.2021[1]


##--------------------------------------------------##
### Observed number of nests at transect - one
##--------------------------------------------------##

observed_no_nests_tr1 <- N23.2021[1]


##--------------------------------------------------##
### Bias transect - one
##--------------------------------------------------##

bias_tr1 <- (Expected_no_nest_tr1 -  observed_no_nests_tr1)




}
",fill=TRUE, file = "Chimps_2021_mhds_tr1.txt")




# Initial values


Nst1.2021 <- V1.2021 + 1


Nst23.2021 <- V23.2021 + 1


inits <- function(){list(  beta0.V1.2021 =runif(1, 0, 1),beta0.V23.2021=runif(1, 0, 1),
                           beta1.21=runif(1, 0, 1), beta2.21=runif(1, 0, 1), beta1.21_hds =runif(1, 0, 1), 
                           N23.2021 = Nst23.2021, N1.2021 = Nst1.2021,
                           sigma1.2021=runif(1,1,10), sigma23.2021=runif(1,1,10))}

# Params to save
params <- c(  "observed_no_nests_tr1",  "Expected_no_nest_tr1",  "bias_tr1")




# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Chimps_2021_mhds.tr1 <- jags(mhds_data_V23.tr1, inits, params, "Chimps_2021_mhds_tr1.txt", n.thin=nt,
                          n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)


Chimps_2021_mhds.tr1

### save output

save(Chimps_2021_mhds.tr1 , file = "Chimps_2021_mhds.tr1.RData")


### Write output as a .csv

Chimps_2021_mhds_tr1 <- Chimps_2021_mhds.tr1$summary

write.csv(Chimps_2021_mhds_tr1, "Chimps_2021_mhds_tr1_ouput_CV.csv")






