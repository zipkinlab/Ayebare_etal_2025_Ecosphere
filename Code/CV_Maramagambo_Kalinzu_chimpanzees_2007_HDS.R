### Author: Samuel Ayebare
### Script: CV_Maramagambo_Kalinzu_chimpanzees_2007_HDS.R

### Chimpanzee
### Maramagambo and Kalinzu Forest Reserves

## Description : To format the data
##             : Run a hierarchical distance sampling model 
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
## 2007 survey ##
#------------------#


#------------------------------#
#-Import chimp nest count data-#
#------------------------------#

### Chimp nest distance observations- second & third visits##
V23 <- read.csv("Kalinzu_mara_V23_2007.csv", header=TRUE)
head(V23)


### Chimp nest observations- per transect - Visits 1, 2, & 3
V123 <- read.csv("Kalinzu_mara_2007.csv", header=TRUE)

#------------------#
## Covariates
#------------------#


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
## Visits 2 & 3  ### 2007 surveys
#--------------#

#------------------------------------------#
## Convert visits 2 & 3 data to a tibble
#------------------------------------------#
Rd23_cv <- tibble::as_tibble(V23)
head(Rd23_cv)
dim(Rd23_cv)


#---------------------------#
## Truncate data: Visits 2&3
#----------------------------#
Rd23_obs_cv <- (filter(Rd23_cv, dist <= 5)) ## Distance category 5 corresponds to 25m of transect width
dim(Rd23_obs_cv)
head(Rd23_obs_cv)
summary(Rd23_obs_cv)

#--------------------------------------#
## create observation data: Visits 2 & 3
#--------------------------------------#
obs_Rd23_cv <- uncount(Rd23_obs_cv, cnest)
dim(obs_Rd23_cv )
head(obs_Rd23_cv )

#--------------------------------------#
## Remove perpendicular distance observations for transect - one
## For cross validation
#--------------------------------------#

obs_Rd23_cv <- filter (obs_Rd23_cv, Tr != 1)
dim(obs_Rd23_cv)
head(obs_Rd23_cv)

#---------------------------------------#
## Chimpanzee nests per transect- abundance model
#---------------------------------------#
Tnests_cv <- as_tibble(V123)
head(Tnests_cv)
dim(Tnests_cv)


#-------------------------------------------------------#
## Number of nests detected per transect- Visit 2 &3
#-------------------------------------------------------#
V23.2007_cv <- Tnests_cv$V23

#-------------------------------------------------------#
## Replace observed nests at transect one with - NA
#-------------------------------------------------------#
V23.2007_cv <- Tnests_cv$V23
V23.2007_tr1 <- replace(V23.2007_cv, 1, NA)

#----------------------------#
## Area per transect - offset
#---------------------------#
A <- Tnests_cv$A



#-------------------------#
#-Create distance classes-# Visits 2&3
#-------------------------#

#--------------------------------#
## distance class per observation
#--------------------------------#
dclass23_cv <- obs_Rd23_cv$dist

#--------------------------------#
## Width of distance classes
#--------------------------------#
delta23_cv <- 5  #  max distance meters

#--------------------------------#
## perpendicular distance meters
#--------------------------------#
B23_cv <- 25  #  #  # Effective strip width 

#Distance class midpoint ID
midpt23_cv <- c(2.5,7.5,12.5,17.5,22.5)

#--------------------------------------#
## Total number of chimp nests observed
## during visits 2 & 3
#--------------------------------------#

nind23_cv <- length(dclass23_cv)

#---------------------------#
## Number of distance classes
#----------------------------#
db23_cv <- length(midpt23_cv)

#-----------------------------#
#Number of transects surveyed
#-----------------------------#
nsites <- 46




# Bundle data (2007 survey) into a list
data_V23.tr1 <- list(nsites=nsites, nind23_cv= nind23_cv, B23_cv =B23_cv, db23_cv= db23_cv, midpt23_cv =midpt23_cv,
                     delta23_cv =delta23_cv,dclass23_cv =dclass23_cv, V23.2007_tr1 = V23.2007_tr1,
                     A = A, elev = elev) 


# BUGS model specification
cat("
model{



## scale parameter for the half normal detection function
sigma23_cv ~ dunif(1, 25)

### Intercept- visits 2 & 3
beta0_cv ~ dnorm(0,0.1)

### Effect of elevation- visits 2 & 3
beta1_cv ~ dnorm(0,0.1)
beta2_cv ~ dnorm(0,0.1)

#Overdispersion
r.N23_cv ~ dunif(1,3)            


### Categorical distribution observation model : V23 - 2007
for(i in 1:nind23_cv){
   dclass23_cv[i] ~ dcat(fc23_cv[]) 
}


# Construct cell probabilities 
  for(g in 1:db23_cv){                 # midpt = mid-point of each cell
    log(p23_cv[g]) <- -midpt23_cv[g] * midpt23_cv[g] / (2*sigma23_cv*sigma23_cv)
    pi23_cv[g] <- delta23_cv/ B23_cv          # probability per interval
    f23_cv[g] <- p23_cv[g] * pi23_cv[g]
    fc23_cv[g] <- f23_cv[g] / pcap23_cv
  }
  pcap23_cv <- sum(f23_cv[])            # Average probability of detection 


   ##--------------------##
  ### Abundance model 2007 - cross validation
  ##--------------------##

  for(s in 1:nsites){
     V23.2007_tr1[s] ~ dbin(pcap23_cv, N23_cv[s])   # Part 2 of HM
     N23_cv[s] ~ dpois(lambda.star23_cv[s])         # Part 3 of HM
     log(lambda23_cv[s]) <- beta0_cv + beta1_cv * elev[s] + beta2_cv * pow(elev[s], 2) + log(A[s]) # linear model abundance

  
  
    #Negative binomial formulation
    lambda.star23_cv[s] <- rho23_cv[s] * lambda23_cv[s]

    #Overdispersion parameter for Expected nests
    rho23_cv[s] ~ dgamma(r.N23_cv, r.N23_cv)
  
  }



##-------------------------
# Derived parameters
##-------------------------

##--------------------------------------------------##
# Expected number of nests at Transect - one
##---------------------------------------------------#

Expected_no_nest_tr1 <- lambda23_cv[1]


##--------------------------------------------------##
### Observed number of nests at transect - one
##--------------------------------------------------##

observed_no_nests_tr1 <- N23_cv[1]


##--------------------------------------------------##
### Bias transect - one
##--------------------------------------------------##

bias_tr1 <- (Expected_no_nest_tr1 -  observed_no_nests_tr1)


}
",fill=TRUE, file = "Chimps_2007_hds_tr1.txt")




# Initial values

Nst23_cv <- V23.2007_cv + 1

inits <- function(){list(beta0_cv=runif(1, 0, 1),beta1_cv=runif(1, 0, 1), beta2_cv=runif(1, 0, 1),  N23_cv=Nst23_cv, sigma23_cv=runif(1,1,10))}

# Params to save
params <- c(  "observed_no_nests_tr1",  "Expected_no_nest_tr1",  "bias_tr1")

# MCMC settings
ni <- 60000   ;   nb <- 10000   ;   nt <- 10   ;   nc <- 3

# Run JAGS and summarize posteriors

Chimps_2007_hds.tr1 <- jags(data_V23.tr1, inits, params, "Chimps_2007_hds_tr1.txt", n.thin=nt,
                                  n.chains=nc, n.burnin=nb, n.iter=ni,  parallel = T)

Chimps_2007_hds.tr1 

### save output

save(Chimps_2007_hds.tr1 , file = "Chimps_2007_hds_tr1.RData")


Chimps_2007_hds_tr1  <- Chimps_2007_hds.tr1$summary

write.csv(Chimps_2007_hds_tr1, "Chimps_2007_hds_tr1_ouput.csv")

















