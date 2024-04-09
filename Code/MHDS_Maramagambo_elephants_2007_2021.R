### Author: Samuel Ayebare
### Script:  MHDS_Maramagambo_elephants_2007_2021.R

### elephants
### Maramagambo Forest Reserve

## Description : To format the data
##             : Run a modified hierarchical distance sampling model 
##        1- Estimate population density for the 2007 and 2021 surveys
##        2- Assess population change between two survey periods (i.e., 2007 & 2021)
##        3- Estimate elephant dung decay rate for the 2007 and 2021 surveys
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

### elephant dung distance observations- first visit
V1.2007 <- read.csv("Ele_V1_2007.csv", header=TRUE)
head(V1.2007)

### elephant dung distance observations- second & third visits##
V23.2007 <- read.csv("Ele_V23_2007.csv", header=TRUE)
head(V23.2007)

### elephant dung observations- per transect - Visits 1, 2, & 3
V123.2007 <- read.csv("Ele_2007.csv", header=TRUE)
head(V123.2007)




#------------------#
## 2021 survey ##
#------------------#

### elephant dung distance observations- first visit
V1.2021 <- read.csv("Ele_V1_2021.csv", header=TRUE)
head(V1.2021)


### elephant dung distance observations- second & third visits##
V23.2021 <- read.csv("Ele_V23_2021.csv", header=TRUE)
head(V23.2021)

### elephant dung observations- per transect - Visits 1, 2, & 3
V123.2021 <- read.csv("Ele_2021.csv", header=TRUE)

head(V123.2021)

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

#------------------#
## 2007 survey ##
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
obs_Rd1.2007 <- uncount(Rd1_obs.2007, eldung )
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
Rd23_obs.2007 <- (filter(Rd23.2007, dist <= 5)) ## Distance category 5 corresponds to 5m of transect width
dim(Rd23_obs.2007)
head(Rd23_obs.2007)
summary(Rd23_obs.2007)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23.2007 <- uncount(Rd23_obs.2007, eldung )
dim(obs_Rd23.2007 )
head(obs_Rd23.2007)


#---------------------------------------#
## elphant dung per transect- abundance model
#---------------------------------------#
eledung.2007 <- as_tibble(V123.2007)
head(eledung.2007)
dim(eledung.2007)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 1
#-------------------------------------------------------#
V1.2007 <- eledung.2007$V1


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 & 3
#-------------------------------------------------------#
V23.2007 <- eledung.2007$V23

##----------------------------#
## Area per transect - offset
#---------------------------#
A.2007 <- eledung.2007$A




#----------------------------------------#
## -Create distance classes-# Visit 1 - 2007
#---------------------------------------#

#--------------------------------#
## distance class per observation
#-------------------------------#
dclass1.2007 <- obs_Rd1.2007$dist

#------------------------------------------#
## Width of distance classes (i.e., 1 meter)
#-----------------------------------------#
delta1.2007 <- 1  

#------------------------------#
## perpendicular distance meters
#------------------------------#
B1.2007 <- 5  #  Effective strip width 

#----------------------------#
## Distance class midpoint ID
#---------------------------#

midpt1.2007 <- c(0.5,1.5,2.5,3.5,4.5)

#--------------------------------------#
## Total number of elephant dung observed
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
nsites <- 36


#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

# distance class per observation
dclass23.2007 <- obs_Rd23.2007$dist

#Width of distance classes
delta23.2007 <- 1  #   meter

# perpendicular distance meters
B23.2007 <- 5  #  # Effective strip width

#Distance class midpoint ID
midpt23.2007 <-  c(0.5,1.5,2.5,3.5,4.5)

#--------------------------------------#
## Total number of elephant dung observed
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
Rd1_obs.2021 <- (filter(Rd1.2021, dist <= 5)) ## Distance category 5 corresponds to 5m of transect width
dim(Rd1_obs.2021)
head(Rd1_obs.2021)
summary(Rd1_obs.2021)

#----------------------------------#
## Create observation data: Visit 1
#---------------------------------#
obs_Rd1.2021 <- uncount(Rd1_obs.2021, eldung )
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
Rd23_obs.2021 <- (filter(Rd23.2021, dist <= 5))
dim(Rd23_obs.2021)
head(Rd23_obs.2021)
summary(Rd23_obs.2021)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23.2021 <- uncount(Rd23_obs.2021, eldung )
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
delta1.2021 <- 1  

#------------------------------#
## perpendicular distance meters
#------------------------------#
B1.2021 <- 5  # Effective strip width

#----------------------------#
## Distance class midpoint ID
#---------------------------#

midpt1.2021 <-  c(0.5,1.5,2.5,3.5,4.5)

#--------------------------------------#
## Total number of elephant dung observed
## during visit one
#--------------------------------------#

nind1.2021 <- length(dclass1.2021)
nind1.2021

#--------------------------------------#
##Number of distance classes - visit one
#--------------------------------------#
db1.2021 <- length(midpt1.2021)


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
dclass23.2021 <- obs_Rd23.2021$dist

#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23.2021 <- 1  #   meter

#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23.2021 <- 5  # # Effective strip width 


#--------------------------------#
## Distance class midpoint ID
#--------------------------------#
midpt23.2021 <-  c(0.5,1.5,2.5,3.5,4.5)

#--------------------------------------#
## Total number of elephant dung observed
## during visits 2 & 3
#--------------------------------------#

nind23.2021 <- length(dclass23.2021)

#---------------------------#
## Number of distance classes
#----------------------------#
db23.2021 <- length(midpt23.2021)



#---------------------------------------#
## elephant dung per transect- abundance model
#---------------------------------------#
eledung.2021 <- as_tibble(V123.2021)
head(eledung.2021)
dim(eledung.2021)



#-------------------------------------------------------#
## Number of dung detected per transect- Visit 1
#-------------------------------------------------------#
V1.2021 <- eledung.2021$V1

#-------------------------------------------------------#
## Number of dung detected per transect- Visits 2 &3
#-------------------------------------------------------#
V23.2021 <- eledung.2021$V23

#----------------------------#
## Area per transect - offset
#---------------------------#
A.2021 <- eledung.2021$A





# Bundle and summarize data set
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


# BUGS model specification (Figure 1)
cat("
model{
# Priors

## scale parameter for the half normal detection function
sigma1.2007 ~ dunif(1, 5)
sigma1.2021 ~ dunif(1, 5)
sigma23.2007 ~ dunif(1, 5)
sigma23.2021 ~ dunif(1, 5)

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


### Categorical observation model - V1 - 2007

for(i in 1:nind1.2007){
   dclass1.2007[i] ~ dcat(fV1.2007[]) 
}

### Categorical observation model - V1 - 2021

for(i in 1:nind1.2021){
   dclass1.2021[i] ~ dcat(fV1.2021[]) 
}


### Categorical observation model - V23 - 2007

for(i in 1:nind23.2007){
   dclass23.2007[i] ~ dcat(fV23.2007[]) 
}

### Categorical observation model - V23 -  2021

for(i in 1:nind23.2021){
   dclass23.2021[i] ~ dcat(fV23.2021[]) # Part 1 of HM
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
  pcap23.2007 <- sum(f23.2007[])            # Average probability of detection 




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
  pcap23.2021 <- sum(f23.2021[])            # Average probability of detection 
  
  
  
  
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
  log(lambda1.2021[s]) <- beta0.V1.2021 + beta1.21 * elev[s] + beta2.21 * pow(elev[s], 2) +  log(A.2021[s]) # linear model abundance

  
  
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

###Total number of elephant dung observed in the surveyed area corrected for imperfect detection
###-------------------------------------------------------------------------------------------##
Ele.V1.2007  <- sum(N1.2007[])
Ele.V1.2021  <- sum(N1.2021[])
Ele.V23.2007 <- sum(N23.2007[])
Ele.V23.2021 <- sum(N23.2021[])

##---------------------------##
### Realized density
##---------------------------##
## We assumed 17 elephant dung piles are produced by each adult elephant per day (Wing and Buss 1970) 
## 50 & 49 : represent number of days (i.e., the time interval between the first, second, and third visits
## 1.0506 and 1.0932: represent the total area surveyed in sqkm
##---------------------------------------------------------------------------------------------------------------##
DEle23.2007  <- Ele.V23.2007 /(17 *50* 1.0506)
DEle23.2021  <- Ele.V23.2021 /(17 *49* 1.09328)

##---------------------------------------------------------------------------------------------------------------##
#### posterior distribution of a change in population density between two survey periods (i.e., 2007 and 2021)
##---------------------------------------------------------------------------------------------------------------##
DEle_pop_change = DEle23.2021  - DEle23.2007 

##---------------------------##
## Dung decay rate
##---------------------------##
No.days.V1.2007 <- Ele.V1.2007   / (DEle23.2007 * 17 * 1.0506)
No.days.V1.2021 <- Ele.V1.2021  / (DEle23.2021 * 17 * 1.09328)

##--------------------------------------------------##
# Total number of expected elephant dung in the surveyed area
##---------------------------------------------------##
Ele.lambda.V1.07 <- sum(lambda1.2007[])
Ele.lambda.V23.07 <- sum(lambda23.2007[])
Ele.lambda.V1.21 <- sum(lambda1.2021[])
Ele.lambda.V23.21 <- sum(lambda23.2021[])

}
",fill=TRUE, file = "Eles.pop_spatial_temporal.txt")




# Inits

Nst1.2007 <- V1.2007 + 1
Nst1.2021 <- V1.2021 + 1

Nst23.2007 <- V23.2007 + 1
Nst23.2021 <- V23.2021 + 1


inits <- function(){list( beta0.V1.2007 =runif(1, 0, 1),beta0.V23.2007=runif(1, 0, 1), beta0.V1.2021 =runif(1, 0, 1),beta0.V23.2021=runif(1, 0, 1),
                          beta1.07 =runif(1, 0, 1), beta2.07 =runif(1, 0, 1), beta1.21=runif(1, 0, 1), beta2.21 =runif(1, 0, 1), 
                          N23.2007 = Nst23.2007, N23.2021 = Nst23.2021, N1.2007 = Nst1.2007, N1.2021 = Nst1.2021,  sigma1.2007=runif(1,1,5),
                          sigma1.2021=runif(1,1,5), sigma23.2007=runif(1,1,5),sigma23.2021=runif(1,1,5))}

# Params to save
params <- c( "beta0.V1.2007", "beta0.V23.2007", " beta0.V1.2021", " beta0.V23.2021","N1.2007", "No.days.V1.2007","No.days.V1.2021",
             "N23.2007", "N1.2021" ,"N23.2021", " lambda1.2007", " beta1.07", " beta2.07"," beta1.21", " beta2.21","lambda23.2007",
              "lambda23.2007", "lambda23.2021", "Ele.lambda.V1.07","Ele.V1.2007", "Ele.lambda.V23.07","Ele.V23.2007", "Ele.lambda.V1.21",
             "Ele.V1.2021", "Ele.lambda.V23.21", "Ele.V23.2021", "DEle23.2007","DEle23.2021",
             "DEle_pop_change", "pcap23.2007","pcap23.2021", "sigma23.2007","sigma23.2021")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Eles_2007_2021_mara <- jags(data_C2007n2021, inits, params, "Eles.pop_spatial_temporal.txt", n.thin=nt,
                          n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)


Eles_2007_2021_mara


save(Eles_2007_2021_mara , file = "Eles_2007_2021_mara_mhds.RData")


#### 

Eles_2007_2021.mara <- Eles_2007_2021_mara$summary

write.csv(Eles_2007_2021.mara, "Eles_2007_2021_ouput_mara_q2.csv")




###-----------------------------##
### Population change assessment
###-----------------------------##

#### Maramangambo Forest Reserve
### Load 
load("..../Eles_2007_2021_mara.RData")

ls()
####----------------------------------------------------------##
## Explore the posterior distribution of population differences
####----------------------------------------------------------##

##Change in population density between the 2007 and 2021 surveys
hist(Eles_2007_2021_mara$sims.list$DEle_pop_change)


### Estimate the probability that the population has decreased
increase <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change  > 0)/15000)
increase


# Explore population change at specific percentages 

####------------------------------##
### 5 percent increase in density
####------------------------------##

five.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 5/100
five.pct.increase
### Probability that the population has increased by 5 percent
prob.increase.5pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > five.pct.increase)/15000)
prob.increase.5pct


####------------------------------##
### 10 percent increase in density
####------------------------------##
ten.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 10/100
ten.pct.increase
### Probability that the population has increased by 10 percent
prob.increase.10pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > ten.pct.increase)/15000)
prob.increase.10pct

####------------------------------##
### 20 percent increase in density
####------------------------------##
twenty.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 20/100
twenty.pct.increase
### Probability that the population has increased by 20 percent
prob.increase.20pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > twenty.pct.increase)/15000)
prob.increase.20pct

####------------------------------##
### 30 percent increase in density
####------------------------------##
thirty.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 30/100
thirty.pct.increase
### Probability that the population has increased by 30 percent
prob.increase.30pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > thirty.pct.increase)/15000)
prob.increase.30pct

####------------------------------##
### 40 percent increase in density
####------------------------------##
forty.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 40/100
forty.pct.increase
### Probability that the population has increased by 40 percent
prob.increase.40pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > forty.pct.increase)/15000)
prob.increase.40pct

####------------------------------##
### 50 percent increase in density
####------------------------------##
fifty.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 50/100
fifty.pct.increase
### Probability that the population has increased by 50 percent
prob.increase.50pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change >  fifty.pct.increase)/15000)
prob.increase.50pct


####------------------------------##
### 100 percent increase in density
####------------------------------##
one.00.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 100/100
one.00.pct.increase
### Probability that the population has increased by 50 percent
prob.increase.100pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > one.00.pct.increase)/15000)
prob.increase.100pct

####------------------------------##
### 125 percent increase in density
####------------------------------##
one.25.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 125/100
one.25.pct.increase
### Probability that the population has increased by 50 percent
prob.increase.125pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > one.25.pct.increase)/15000)
prob.increase.125pct

####------------------------------##
### 150 percent increase in density
####------------------------------##
one.50.pct.increase <- Eles_2007_2021_mara$mean$DEle23.2007 + Eles_2007_2021_mara$mean$DEle23.2007 * 150/100
one.50.pct.increase

### Probability that the population has increased by 50 percent
prob.increase.150pct <- (sum(Eles_2007_2021_mara$sims.list$DEle_pop_change > one.50.pct.increase)/15000)
prob.increase.150pct





































