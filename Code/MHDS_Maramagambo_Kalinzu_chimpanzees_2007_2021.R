### Author: Samuel Ayebare
### Script: MHDS_Maramagambo_Kalinzu_chimpanzees_2007_2021.R

### Chimpanzees
### Maramagambo and Kalinzu Forest Reserves combined

## Description : To format the data
##             : Run a modified hierarchical distance sampling model 
##        1- Estimate population density for the 2007 and 2021 surveys
##        2- Assess population change between two survey periods (i.e., 2007 & 2021)
##        3- Estimate nest decay rate for the 2007 and 2021 surveys
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
#------------------#


#------------------------------#
#-Import chimp nest count data-#
#------------------------------#

### Chimp nest distance observations- first visit
V1.2007 <- read.csv("Kalinzu_mara_V1_2007.csv", header=TRUE)
head(V1.2007)

### Chimp nest distance observations- second & third visits
V23.2007 <- read.csv("Kalinzu_mara_V23_2007.csv", header=TRUE)
head(V23.2007)

### Chimp nest observations- per transect - Visits 1, 2, & 3
V123.2007 <- read.csv("Kalinzu_mara_2007.csv", header=TRUE)
head(V123.2007)




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




#------------------#
## 2007 survey ##
#------------------#

#-------------------------#
## Visit 1 - 2007 
#-------------------------#

## Convert visit one data to a tibble
Rd1.2007 <- tibble::as_tibble(V1.2007)
head(Rd1.2007)
dim(Rd1.2007)


#-------------------------#
## Trucante data: Visit 1
#-------------------------#
Rd1_obs.2007 <- (filter(Rd1.2007, dist <= 5))  ## Distance category 5 corresponds to 25m of transect width
dim(Rd1_obs.2007)
head(Rd1_obs.2007)
summary(Rd1_obs.2007)



#----------------------------------#
## Create observation data: Visit 1
#---------------------------------#
obs_Rd1.2007 <- uncount(Rd1_obs.2007, cnest)
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
Rd23_obs.2007 <- (filter(Rd23.2007, dist <= 5)) ## Distance category 5 corresponds to 25m of transect width
dim(Rd23_obs.2007)
head(Rd23_obs.2007)
summary(Rd23_obs.2007)


#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23.2007 <- uncount(Rd23_obs.2007, cnest)
dim(obs_Rd23.2007 )
head(obs_Rd23.2007)


#---------------------------------------#
## Chimpanzee nests per transect- abundance model
#---------------------------------------#
Tnests.2007 <- as_tibble(V123.2007)
head(Tnests.2007)
dim(Tnests.2007)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 1
#-------------------------------------------------------#
V1.2007 <- Tnests.2007$V1


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 &3
#-------------------------------------------------------#
V23.2007 <- Tnests.2007$V23


#----------------------------#
## Area per transect - offset
#---------------------------#
A.2007 <- Tnests.2007$A
A.2007



#----------------------------------------#
## -Create distance classes-# Visit 1 - 2007
#---------------------------------------#


#--------------------------------#
## distance class per observation
#-------------------------------#
dclass1.2007 <- obs_Rd1.2007$dist
dclass1.2007


#------------------------------------------#
## Width of distance classes (i.e., 5 meters)
#------------------------------------------#
delta1.2007 <- 5  


#------------------------------#
## perpendicular distance meters
#------------------------------#
B1.2007 <- 25  # Effective strip width 


#----------------------------#
## Distance class midpoint ID
#---------------------------#
midpt1.2007 <- c(2.5,7.5,12.5,17.5,22.5)


#--------------------------------------#
## Total number of chimp nests observed
## during visit one
#--------------------------------------#
nind1.2007 <- length(dclass1.2007)
nind1.2007 


#--------------------------------------#
##Number of distance classes - visit one
#--------------------------------------#
db1.2007 <- length(midpt1.2007)


#-----------------------------#
#Number of transects surveyed
#-----------------------------#
nsites <- 46


#--------------------------#
##-Create distance classes-# Visits 2&3
#-------------------------#

#--------------------------------#
## distance class per observation
#--------------------------------#
dclass23.2007 <- obs_Rd23.2007$dist

#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23.2007 <- 5  #  meters


#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23.2007 <- 25  #  # Effective strip width 


#Distance class midpoint ID
midpt23.2007 <- c(2.5,7.5,12.5,17.5,22.5)


#--------------------------------------#
## Total number of chimp nests observed
## during visits 2 & 3
#--------------------------------------#
nind23.2007 <- length(dclass23.2007)


#---------------------------#
## Number of distance classes
#----------------------------#
db23.2007 <- length(midpt23.2007)




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
## Trucante data: Visit 1
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


#---------------#
## Visits 2 & 3  
#--------------#

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



#-------------------------#
#-Create distance classes-# Visit 1 - 2021
#-------------------------#

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


#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

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

## Area per transect - offset
A.2021 <- Tnests.2021$A








# Bundle data (2007 & 2021 survey) into a list
data_C2007n2021 <- list(nsites=nsites, nind1.2007 =nind1.2007, nind1.2021 =nind1.2021, nind23.2007 =nind23.2007, nind23.2021 =nind23.2021, 
                        B1.2007 =B1.2007, B1.2021 =B1.2021,B23.2007 =B23.2007, B23.2021 = B23.2021, db1.2007=db1.2007, db1.2021 = db1.2021,
                       db23.2007=db23.2007, db23.2021 = db23.2021, midpt1.2007=midpt1.2007, midpt1.2021 = midpt1.2021,
                       midpt23.2007=midpt23.2007, midpt23.2021 = midpt23.2021, delta1.2007=delta1.2007, delta1.2021=delta1.2021, 
                       delta23.2007=delta23.2007, delta23.2021=delta23.2021, dclass1.2007 =dclass1.2007,dclass1.2021 =dclass1.2021,
                       dclass23.2007 =dclass23.2007, dclass23.2021 =dclass23.2021,  V1.2007= V1.2007, V23.2007=V23.2007,
                       V1.2021=V1.2021, V23.2021 = V23.2021, A.2007 = A.2007,A.2021 = A.2021, elev =elev) 


####################################################
######### Joint likelihood- Visit1 & Visits 2,3 
####################################################


# BUGS model specification (# Figure 1)
cat("
model{
# Priors

## scale parameter for the half normal detection function
sigma1.2007 ~ dunif(1, 25)
sigma1.2021 ~ dunif(1, 25)
sigma23.2007 ~ dunif(1, 25)
sigma23.2021 ~ dunif(1, 25)

### Intercept- visits 1, 2 & 3

beta0.V1.2007 ~ dnorm(0,0.1)
beta0.V1.2021 ~ dnorm(0,0.1)
beta0.V23.2007 ~ dnorm(0,0.1)
beta0.V23.2021 ~ dnorm(0,0.1)

### Effect of elevation- visits 1, 2 & 3
beta1.07 ~ dnorm(0,0.1)
beta2.07 ~ dnorm(0,0.1)
beta1.21 ~ dnorm(0,0.1)
beta2.21 ~ dnorm(0,0.1)



#Overdispersion
r.N1.2007 ~ dunif(1,3)  
r.N1.2021 ~ dunif(1,3)    
r.N23.2007 ~ dunif(1,3) 
r.N23.2021 ~ dunif(1,3)            



### Categorical distribution observation model : V1 - 2007

for(i in 1:nind1.2007){
   dclass1.2007[i] ~ dcat(fV1.2007[])
}

### Categorical distribution observation model : V1 - 2021

for(i in 1:nind1.2021){
   dclass1.2021[i] ~ dcat(fV1.2021[]) 
}


### Categorical distribution observation model : V23 - 2007

for(i in 1:nind23.2007){
   dclass23.2007[i] ~ dcat(fV23.2007[]) 
}

### Categorical distribution observation model - V23 -  2021

for(i in 1:nind23.2021){
   dclass23.2021[i] ~ dcat(fV23.2021[]) 
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
    pi1.2007[g] <- delta1.2007/ B1.2007         # probability per interval 
    f1.2007[g] <- p1.2007[g] * pi1.2007[g]
    fV1.2007[g] <- f1.2007[g] / pcap1.2007
  }
  pcap1.2007 <- sum(f1.2007[])            # Average probability of detection 



##-------------##
# Visit 23 - 2007
##-------------##
## Construct cell probabilities 

  for(g in 1:db23.2007){                 
  
    
    log(p23.2007[g]) <- -midpt23.2007[g] * midpt23.2007[g] / (2*sigma23.2007*sigma23.2007)
    pi23.2007[g] <- delta23.2007/ B23.2007         # probability per interval
    f23.2007[g] <- p23.2007[g] * pi23.2007[g]
    fV23.2007[g] <- f23.2007[g] / pcap23.2007
  }
  pcap23.2007 <- sum(f23.2007[])           # Average probability of detection 




##-------------##
### 2021 survey
##-------------##

##-------------##
# Visit 1 - 2021 
##-------------##
## Construct cell probabilities 

  for(g in 1:db1.2021){    
  
    log(p1.2021[g]) <- -midpt1.2021[g] * midpt1.2021[g] / (2*sigma1.2021*sigma1.2021)
    pi1.2021[g] <- delta1.2021/ B1.2021          # probability per interval
    f1.2021[g] <- p1.2021[g] * pi1.2021[g]
    fV1.2021[g] <- f1.2021[g] / pcap1.2021
    
  }
  pcap1.2021 <- sum(f1.2021[])            # Average probability of detection 
  
  
  

# Visit 23 - 2021 ## Construct cell probabilities 

  for(g in 1:db23.2021){    
  
    log(p23.2021[g]) <- -midpt23.2021[g] * midpt23.2021[g] / (2*sigma23.2021*sigma23.2021)
    pi23.2021[g] <- delta23.2021/ B23.2021          # probability per interval
    f23.2021[g] <- p23.2021[g] * pi23.2021[g]
    fV23.2021[g] <- f23.2021[g] / pcap23.2021
    
  }
  pcap23.2021 <- sum(f23.2021[])           # Average probability of detection 
  
  
  
  ##--------------------##
  ### Abundance model 2007
  ##--------------------##
  
  
  # Visit 1 - 2007 (submodel)
  ##---------------------------
  
  for(s in 1:nsites){
  
  V1.2007[s] ~ dbin(pcap1.2007, N1.2007[s])   # Part 2 of HM
  N1.2007[s] ~ dpois( lambda.star1.2007[s])         # Part 3 of HM
  log(lambda1.2007[s]) <- beta0.V1.2007 + beta1.07 * elev[s] + beta2.07 * pow(elev[s], 2) + log(A.2007[s]) # linear model abundance

  
  
  #Negative binomial formulation
  lambda.star1.2007[s] <- rho1.2007[s] * lambda1.2007[s]

  #Overdispersion parameter for Expected nests
  rho1.2007[s] ~ dgamma(r.N1.2007, r.N1.2007)
  
  }
            
            
  ##---------------------------  
  # Visits 2 & 3 - 2007 (submodel)
  ##---------------------------
  
  for(s in 1:nsites){
  
  V23.2007[s] ~ dbin(pcap23.2007, N23.2007[s])   # Part 2 of HM
  N23.2007[s] ~ dpois( lambda.star23.2007[s])         # Part 3 of HM
  log(lambda23.2007[s]) <- beta0.V23.2007 + beta1.07 * elev[s] + beta2.07 * pow(elev[s], 2) + log(A.2007[s]) # linear model abundance

  
  #Negative binomial formulation
  lambda.star23.2007[s] <- rho23.2007[s] * lambda23.2007[s]

  #Overdispersion parameter 
  rho23.2007[s] ~ dgamma(r.N23.2007, r.N23.2007)
    
  }
    
    
    
  ##----------------------##
  ### Abundance models 2021
  ##----------------------##
  
  # Visit 1 - 2021 (submodel)
  ##---------------------------
   
    for(s in 1:nsites){
    
  V1.2021[s] ~ dbin(pcap1.2021, N1.2021[s])   # Part 2 of HM
  N1.2021[s] ~ dpois(lambda.star1.2021[s])         # Part 3 of HM
  log(lambda1.2021[s]) <- beta0.V1.2021 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2021[s]) # linear model abundance

  
  
  #Negative binomial formulation
  lambda.star1.2021[s] <- rho1.2021[s] * lambda1.2021[s]

  #Overdispersion parameter for Expected nests
  rho1.2021[s] ~ dgamma(r.N1.2021, r.N1.2021)
  
  }


  ##---------------------------  
  # Visits 2 & 3 - 2021 (submodel)
  ##---------------------------
   
for(s in 1:nsites){

  V23.2021[s] ~ dbin(pcap23.2021, N23.2021[s])   # Part 2 of HM
  N23.2021[s] ~ dpois(lambda.star23.2021[s])         # Part 3 of HM
  log(lambda23.2021[s]) <- beta0.V23.2021 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) + log(A.2021[s]) # linear model abundance

  #Negative binomial formulation
  lambda.star23.2021[s] <- rho23.2021[s] * lambda23.2021[s]

  #Overdispersion parameter 
   rho23.2021[s] ~ dgamma(r.N23.2021, r.N23.2021)
  
  }


# Derived parameters
##---------------------------##

###Total number of chimpanzee nests observed in the surveyed area corrected for imperfect detection
###------------------------------------------------------------------------------------##
Mara_Kalinzu.V1.2007  <- sum(N1.2007[])
Mara_Kalinzu.V1.2021  <- sum(N1.2021[])
Mara_Kalinzu.V23.2007 <- sum(N23.2007[])
Mara_Kalinzu.V23.2021 <- sum(N23.2021[])

##---------------------------##
### Realized density
##---------------------------##
## We assumed 1.09 nests are built by each adult chimpanzee per day (Plumptre and Reynolds 1997) 
## 50 & 49 : represent number of days (i.e., the time interval between the first, second, and third visits
## 6.8 and 7: represent the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##

DMara_Kalinzu23.2007  <- Mara_Kalinzu.V23.2007 /(1.09*50* 6.7556)
DMara_Kalinzu23.2021  <- Mara_Kalinzu.V23.2021 /(1.09*49* 7)

##---------------------------------------------------------------------------------------------------------------##
#### posterior distribution of a change in population density between two survey periods (i.e., 2007 and 2021)
##---------------------------------------------------------------------------------------------------------------##
DMara_Kalinzu_pop_change = DMara_Kalinzu23.2021  - DMara_Kalinzu23.2007 

##---------------------------##
## Nest decay rate
##---------------------------##
No.days.V1.2007 <- Mara_Kalinzu.V1.2007   / (DMara_Kalinzu23.2007 * 1.09*6.7556)
No.days.V1.2021 <- Mara_Kalinzu.V1.2021  / (DMara_Kalinzu23.2021 * 1.09*7)

##--------------------------------------------------##
# Total number of expected signs in the surveyed area
##---------------------------------------------------##
Mara_Kalinzu.lambda.V1.07 <- sum(lambda1.2007[])
Mara_Kalinzu.lambda.V23.07 <- sum(lambda23.2007[])
Mara_Kalinzu.lambda.V1.21 <- sum(lambda1.2021[])
Mara_Kalinzu.lambda.V23.21 <- sum(lambda23.2021[])

}
",fill=TRUE, file = "Kalinzu.mara.pop_spatial_temporal.txt")




# Initial values

Nst1.2007 <- V1.2007 + 1
Nst1.2021 <- V1.2021 + 1

Nst23.2007 <- V23.2007 + 1
Nst23.2021 <- V23.2021 + 1


inits <- function(){list( beta0.V1.2007 =runif(1, 0, 1),beta0.V23.2007=runif(1, 0, 1), beta0.V1.2021 =runif(1, 0, 1),beta0.V23.2021=runif(1, 0, 1),
                          beta1.07 =runif(1, 0, 1), beta2.07 =runif(1, 0, 1),beta1.21=runif(1, 0, 1), beta2.21=runif(1, 0, 1),
                          N23.2007 = Nst23.2007, N23.2021 = Nst23.2021, N1.2007 = Nst1.2007, N1.2021 = Nst1.2021,  sigma1.2007=runif(1,1,10),sigma1.2021=runif(1,1,10),
                          sigma23.2007=runif(1,1,10),sigma23.2021=runif(1,1,10))}

# Params to save
params <- c( "beta0.V1.2007", "beta0.V23.2007", " beta0.V1.2021", " beta0.V23.2021","N1.2007", "No.days.V1.2007","No.days.V1.2021",
             "N23.2007", "N1.2021" ,"N23.2021", " lambda1.2007", " beta1.07", " beta2.07", "beta1.21", "beta2.21", 
             "lambda23.2007","lambda23.2007", "lambda23.2021", "Mara_Kalinzu.lambda.V1.07","Mara_Kalinzu.V1.2007",
             "Mara_Kalinzu.lambda.V23.07","Mara_Kalinzu.V23.2007", "Mara_Kalinzu.lambda.V1.21", "Mara_Kalinzu.V1.2021", 
             "Mara_Kalinzu.lambda.V23.21", "Mara_Kalinzu.V23.2021", "DMara_Kalinzu23.2007","DMara_Kalinzu23.2021",
             "DMara_Kalinzu_pop_change", "pcap23.2007","pcap23.2021", "sigma23.2007","sigma23.2021")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Mara.Kal_2007_2021_chimps <- jags(data_C2007n2021, inits, params, "Kalinzu.mara.pop_spatial_temporal.txt", n.thin=nt,
                          n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)


Mara.Kal_2007_2021_chimps


### save output

save(Mara.Kal_2007_2021_chimps , file = "Mara.Kal_2007_2021_chimps_mhds.RData")


### Write output as a .csv

Mara.Kal_2007_2021.chimps <- Mara.Kal_2007_2021_chimps$summary

write.csv(Mara.Kal_2007_2021.chimps, "Mara.Kal_2007_2021_ouput_chimps_q.csv")










###-----------------------------##
### Population change assessment
###-----------------------------##

#### Maramagambo and Kalinzu Forest Reserve
### Load 
load("....../Mara.Kal_2007_2021_chimps_mhds.RData")

ls()

####----------------------------------------------------------##
## Explore the posterior distribution of population differences
####----------------------------------------------------------##

###Change in population density between the 2007 and 2021 surveys
hist(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change)

### Estimate the probability that the population has decreased
decrease <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change  < 0)/15000)
decrease

### Estimate the probability that the population has increased
increase <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change  > 0)/15000)
increase




# Explore population change at specific percentages 
## Multiply by a negative (-1) to compute hypothesized decline

####------------------------------##
### 5 percent decrease in density
####------------------------------##
five.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 5/100)
five.pct.decrease

### Probability that the population has decreased by 5 percent
prob.decrease.5pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < five.pct.decrease)/15000)
prob.decrease.5pct

####------------------------------##
### 10 percent decrease in density
####------------------------------##
ten.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 10/100)
ten.pct.decrease

### Probability that the population has decreased by 10 percent
prob.decrease.10pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < ten.pct.decrease)/15000)
prob.decrease.10pct

####------------------------------##
### 20 percent decrease in density
####------------------------------##
twenty.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 20/100)
twenty.pct.decrease
### Probability that the population has decreased by 20 percent
prob.decrease.20pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < twenty.pct.decrease)/15000)
prob.decrease.20pct

####------------------------------##
### 30 percent decrease in density
####------------------------------##
thirty.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 30/100)
thirty.pct.decrease
### Probability that the population has decreased by 30 percent
prob.decrease.30pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < thirty.pct.decrease)/15000)
prob.decrease.30pct

####------------------------------##
### 40 percent decrease in density
####------------------------------##
forty.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 40/100)
forty.pct.decrease
### Probability that the population has decreased by 40 percent
prob.decrease.40pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < forty.pct.decrease)/15000)
prob.decrease.40pct

####------------------------------##
### 50 percent decrease in density
####------------------------------##
fifty.pct.decrease <- -1 * (Mara.Kal_2007_2021_chimps$mean$DMara_Kalinzu23.2007 * 50/100)
fifty.pct.decrease
### Probability that the population has decreased by 50 percent
prob.decrease.50pct <- (sum(Mara.Kal_2007_2021_chimps$sims.list$DMara_Kalinzu_pop_change < fifty.pct.decrease)/15000)
prob.decrease.50pct





























