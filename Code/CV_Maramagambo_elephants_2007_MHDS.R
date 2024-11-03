### Author: Samuel Ayebare
### Script: CV_Maramagambo_elephants_2007_MHDS.R

### Elephants
### Maramagambo Forest Reserve

## Description : To format the data
##             : Run a modified hierarchical distance sampling model 
##             : Assess bias by using cross validation (2007 survey)

### ### This code omits transect one
### Same code is used for all the 36 transects

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


#------------------------------#
#-Import elephant dung count data-#
#------------------------------#

### elephant dung distance observations- first visit
V1.2007 <- read.csv("Ele_V1_2007.csv", header=TRUE)
head(V1.2007)


### elephant dung distance observations- second & third visits
V23.2007 <- read.csv("Ele_V23_2007.csv", header=TRUE)
head(V23.2007)

### elephant dung observations-  per transect
V123.2007 <- read.csv("Ele_2007.csv", header=TRUE)

head(V123.2007)

#------------------#
## Covariates
#------------------#

# Note: Annual mean precipitation and temperature covariates were not used in the model


#----------------------#
#-Import covariate data#
#----------------------#

Pred.var <- read.csv("Pred_var.csv", header=TRUE)

head(Pred.var)


#### Filter Mara - elevation values
elev <- Pred.var$elev[1:36]
elev

#### Scale predictor variable

elev <- (elev - mean(elev))/sd(elev)

elev


#------------------------------------------#
####      2007 survey                  ####
#------------------------------------------#

#------------------#
## Visit 1 - 2007 
#------------------#

## Convert visit one data to a tibble
Rd1.2007 <- tibble::as_tibble(V1.2007)
head(Rd1.2007)
dim(Rd1.2007)


#-------------------------#
## Trucante data: Visit 1
#-------------------------#
Rd1_obs.2007 <- (filter(Rd1.2007, dist <= 5)) ## Distance category 5 corresponds to 5m of transect width
dim(Rd1_obs.2007)
head(Rd1_obs.2007)
summary(Rd1_obs.2007)

#----------------------------------#
## Create observation data: Visit 1
#---------------------------------#
obs_Rd1.2007 <- uncount(Rd1_obs.2007, eldung)
dim(obs_Rd1.2007 )
head(obs_Rd1.2007)


#---------------#
## Visits 2 & 3  
#--------------#

## Convert visits 2 & 3 data to a tibble
Rd23.2007 <- tibble::as_tibble(V23.2007)
head(Rd23.2007)
dim(Rd23.2007)


#---------------------------#
## Trucante data: Visits 2&3
#----------------------------#
Rd23_obs.2007 <- (filter(Rd23.2007, dist <= 5))  ## Distance category 5 corresponds to 5m of transect width
dim(Rd23_obs.2007)
head(Rd23_obs.2007)
summary(Rd23_obs.2007)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23.2007 <- uncount(Rd23_obs.2007, eldung)
dim(obs_Rd23.2007 )
head(obs_Rd23.2007)

#--------------------------------------#
## Remove perpendicualr distance observations for transect - one
#--------------------------------------#

obs_Rd23.2007 <- filter (obs_Rd23.2007, Tr != 1)
dim(obs_Rd23.2007)
head(obs_Rd23.2007)


#-------------------------#
#-Create distance classes-# Visit 1 - 2007
#-------------------------#

#--------------------------------#
## distance class per observation
#-------------------------------#
dclass1.2007 <- obs_Rd1.2007$dist


#------------------------------------------#
## Width of distance classes (i.e., 1 meter)
#------------------------------------------#
delta1.2007 <- 1  # meters


#------------------------------#
## perpendicular distance meters
#------------------------------#
B1.2007 <- 5   # Effective strip width 


#----------------------------#
## Distance class midpoint ID
#---------------------------#
midpt1.2007 <- c(0.5,1.5,2.5,3.5,4.5)



#--------------------------------------#
## Total number of Elephant dung observed
## during visit one
#--------------------------------------#
nind1.2007 <- length(dclass1.2007)


#--------------------------------------#
##Number of distance classes - visit one
#--------------------------------------#
db1.2007 <- length(midpt1.2007)


#-----------------------------#
#Number of transects surveyed
#-----------------------------#
nsites <- 36


#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

#--------------------------------#
## distance class per observation
#--------------------------------#
dclass23.2007 <- obs_Rd23.2007$dist


#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23.2007 <- 1  #  meters


#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23.2007 <- 5   # Effective strip width 


#--------------------------------#
## Distance class midpoint ID
#--------------------------------#
midpt23.2007 <- c(0.5,1.5,2.5,3.5,4.5)


#--------------------------------------#
## Total number of Elephant dung observed
## during visits 2 & 3
#--------------------------------------#
nind23.2007 <- length(dclass23.2007)



#---------------------------#
## Number of distance classes
#----------------------------#
db23.2007 <- length(midpt23.2007)




#---------------------------------------#
## elephant dung per transect- abundance model
#---------------------------------------#
eledung.2007 <- as_tibble(V123.2007)
head(eledung.2007)
dim(eledung.2007)


#-------------------------------------------------------#
## Number of elephant dung s detected per transect- Visit 1
#-------------------------------------------------------#
V1.2007 <- eledung.2007$V1

#-------------------------------------------------------#
## Number of elephant dung s detected per transect- Visits 2 &3
#-------------------------------------------------------#
V23.2007 <- eledung.2007$V23

#-------------------------------------------------------#
## Replace observed elephant dung at transect one with - NA
#-------------------------------------------------------#

V23.2007_tr1 <- replace(V23.2007, 1, NA)
V23.2007_tr1
## Area per transect - offset
A.2007 <- eledung.2007$A



# Bundle data (2007 survey) into a list
mhds_data_V23.tr1 <- list(nsites=nsites, nind1.2007 =nind1.2007, nind23.2007 =nind23.2007, 
                        B1.2007 =B1.2007, B23.2007 = B23.2007, db1.2007 = db1.2007,
                        db23.2007 = db23.2007, midpt1.2007 = midpt1.2007,
                        midpt23.2007 = midpt23.2007,  delta1.2007=delta1.2007, 
                        delta23.2007=delta23.2007,dclass1.2007 =dclass1.2007,
                        dclass23.2007 =dclass23.2007, V1.2007=V1.2007, 
                        V23.2007_tr1 = V23.2007_tr1,A.2007 = A.2007, elev =elev) 


####################################################
######### Joint likelihood- Visit1 & Visits 2,3 
####################################################


# BUGS model specification (# Figure 1)
cat("
model{
# Priors

## scale parameter for the half normal detection function

sigma1.2007 ~ dunif(1, 5)

sigma23.2007 ~ dunif(1, 5)


### Intercept- visits 1, 2 & 3


beta0.V1.2007 ~ dnorm(0,0.1)

beta0.V23.2007 ~ dnorm(0,0.1)


### Effect of elevation- visits 1, 2 & 3

beta1.21 ~ dnorm(0,0.1)
beta2.21 ~ dnorm(0,0.1)


#Overdispersion
 
r.N1.2007 ~ dunif(1,3)    

r.N23.2007 ~ dunif(1,3)            


### Categorical distribution observation model : V1 - 2007

for(i in 1:nind1.2007){
   dclass1.2007[i] ~ dcat(fV1.2007[]) 
}



### Categorical distribution observation model - V23 -  2007

for(i in 1:nind23.2007){
   dclass23.2007[i] ~ dcat(fV23.2007[]) 
}



##-------------##
### 2007 survey
##-------------##

##-------------##
# Visit 1 - 2007 
##-------------##
## Construct cell probabilities 

  for(g in 1:db1.2007){    
  
    log(p1.2007[g]) <- -midpt1.2007[g] * midpt1.2007[g] / (2*sigma1.2007*sigma1.2007)
    pi1.2007[g] <- delta1.2007/ B1.2007          # probability per interval
    f1.2007[g] <- p1.2007[g] * pi1.2007[g]
    fV1.2007[g] <- f1.2007[g] / pcap1.2007
    
  }
  pcap1.2007 <- sum(f1.2007[])            # Average probability of detection 
  
  
  

# Visit 23 - 2007 ## Construct cell probabilities 

  for(g in 1:db23.2007){    
  
    log(p23.2007[g]) <- -midpt23.2007[g] * midpt23.2007[g] / (2*sigma23.2007*sigma23.2007)
    pi23.2007[g] <- delta23.2007/ B23.2007          # probability per interval
    f23.2007[g] <- p23.2007[g] * pi23.2007[g]
    fV23.2007[g] <- f23.2007[g] / pcap23.2007
    
  }
  pcap23.2007 <- sum(f23.2007[])           # Average probability of detection 
  
  

    
  ##----------------------##
  ### Abundance models 2007
  ##----------------------##
  
  # Visit 1 - 2007 (submodel)
  ##---------------------------
   
    for(s in 1:nsites){
    
  V1.2007[s] ~ dbin(pcap1.2007, N1.2007[s])   # Part 2 of HM
  N1.2007[s] ~ dpois(lambda.star1.2007[s])         # Part 3 of HM
  log(lambda1.2007[s]) <- beta0.V1.2007 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2007[s]) # linear model abundance

  
  
  #Negative binomial formulation
  lambda.star1.2007[s] <- rho1.2007[s] * lambda1.2007[s]

  #Overdispersion parameter 
  rho1.2007[s] ~ dgamma(r.N1.2007, r.N1.2007)
  
  }


  ##---------------------------  
  # Visits 2 & 3 - 2007 (submodel)
  ##---------------------------
   
for(s in 1:nsites){

  V23.2007_tr1[s] ~ dbin(pcap23.2007, N23.2007[s])   # Part 2 of HM
  N23.2007[s] ~ dpois(lambda.star23.2007[s])         # Part 3 of HM
  log(lambda23.2007[s]) <- beta0.V23.2007 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2007[s]) # linear model abundance

  #Negative binomial formulation
  lambda.star23.2007[s] <- rho23.2007[s] * lambda23.2007[s]

  #Overdispersion parameter 
   rho23.2007[s] ~ dgamma(r.N23.2007, r.N23.2007)
  
  }




# Derived parameters
##---------------------------##

##--------------------------------------------------##
# Expected number of elephant dung at Transect - One
##---------------------------------------------------#

Expected_no_dung_tr1 <- lambda23.2007[1]


##--------------------------------------------------##
### Observed number of elephant dung at transect - One
##--------------------------------------------------##

observed_no_dung_tr1 <- N23.2007[1]


##--------------------------------------------------##
### Bias - transect one
##--------------------------------------------------##

bias_tr1 <- (Expected_no_dung_tr1 -  observed_no_dung_tr1)



}
",fill=TRUE, file = "Eles_2007_mhds_tr1.txt")




# Initial values


Nst1.2007 <- V1.2007 + 1


Nst23.2007 <- V23.2007 + 1


inits <- function(){list(  beta0.V1.2007 =runif(1, 0, 1),beta0.V23.2007=runif(1, 0, 1),
                           beta1.21=runif(1, 0, 1), beta2.21=runif(1, 0, 1), 
                           N23.2007 = Nst23.2007, N1.2007 = Nst1.2007,
                           sigma1.2007=runif(1,1,5), sigma23.2007=runif(1,1,5))}

# Params to save
params <- c("observed_no_dung_tr1",  "Expected_no_dung_tr1",  "bias_tr1")



# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Eles_2007_mhds.tr1 <- jags(mhds_data_V23.tr1, inits, params, "Eles_2007_mhds_tr1.txt", n.thin=nt,
                          n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)



Eles_2007_mhds.tr1

### save output

save(Eles_2007_mhds.tr1  , file = "Eles_2007_mhds.tr1.RData")


### Write output as a .csv
### Write output as a .csv

Eles_2007_mhds_tr1 <- Eles_2007_mhds.tr1$summary

write.csv(Eles_2007_mhds_tr1, "Eles_2007_mhds_tr1_output_CV.csv")



