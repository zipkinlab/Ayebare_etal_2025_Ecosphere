

## Authors: Neil Gilbert & Samuel Ayebare
##Script:  Figure6_cds_density_estimation.R


## Description : To plot figure6 - Population density estimates for chimpanzees and elephants (2007 and 2021 surveys), 
##                                   averaged across the study area using conventional distance sampling (visits 2 & 3). 


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
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(extrafont)
library(parallel)
library(svglite)


#### Load estimates of population density from conventional distance sampling - Bayesian inference

#-----------------------#
#### Chimpanzees 2007
#-----------------------#
load("Mara.Kal_2007.V23_chimps_cds.RData")

#-----------------------#
#### Chimpanzees 2021
#-----------------------#
load("Mara.Kal_2021.V23_chimps_cds.RData")


#-----------------------#
### Elephants 2007
#-----------------------#

load("Eles_2007_mara_cds.RData")

#-----------------------#
### Elephants 2021
#-----------------------#

load("Eles_2021_mara_cds.RData")


#-----------------------#
#Chimpanzees
#-----------------------#


#------------------------------------------------------------------------#
### Maramangambo - Kalinzu Forest Reserves - combined - 2007
#------------------------------------------------------------------------#

#-----------------------#
#### Bayesian inference - JAGS
#-----------------------#
D2007.b <- cbind(Mara_Kalinzu.V23_2007.cds$q2.5$DMara_Kalinzu23,
                 Mara_Kalinzu.V23_2007.cds$mean$DMara_Kalinzu23, 
                 Mara_Kalinzu.V23_2007.cds$q97.5$DMara_Kalinzu23)


bayes.b <- "JAGS"

values.b.2007 <- data.frame(2007, "chimpanzee",bayes.b , D2007.b)
colnames(values.b.2007) <- c("year","Sp" ,"method", "lower.ci", "mean.d", "upper.ci")
values.b.2007

#-----------------------#
#### Maximum likelihood
#-----------------------#

#-----------------------#
#### R2 - Variance estimation
#-----------------------#
D2007.ml.R2 <- cbind(0.7048, 1.2043, 2.0578)

ml.d.R2 <- "R2"


values.ml.R2.2007 <- data.frame(2007, "chimpanzee",ml.d.R2, D2007.ml.R2)
colnames(values.ml.R2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.R2.2007

#-----------------------#
#### O2 - Variance estimation
#-----------------------#
D2007.ml.O2 <- cbind(0.8782, 1.2043, 1.6515)

ml.d.O2 <- "O2"


values.ml.O2.2007 <- data.frame(2007, "chimpanzee",ml.d.O2, D2007.ml.O2)
colnames(values.ml.O2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.O2.2007

#-----------------------#
#### S2 - Variance estimation
#-----------------------#
D2007.ml.S2 <- cbind(0.5961, 1.2043, 2.4328)

ml.d.S2 <- "S2"

values.ml.S2.2007 <- data.frame(2007, "chimpanzee",ml.d.S2, D2007.ml.S2)
colnames(values.ml.S2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.S2.2007


#-----------------------#
#Chimpanzees -- 2021
#-----------------------#

#------------------------------------------------------------------------#
## Maramangambo - Kalinzu Forest Reserves - combined - 2021
#------------------------------------------------------------------------#



#-----------------------#
#### Bayesian inference - JAGS
#-----------------------#


D2021.b <- cbind(Mara_Kalinzu.V23_2021.cds$q2.5$DMara_Kalinzu23,
                 Mara_Kalinzu.V23_2021.cds$mean$DMara_Kalinzu23, 
                 Mara_Kalinzu.V23_2021.cds$q97.5$DMara_Kalinzu23)


bayes.b <- "JAGS"

values.b.2021 <- data.frame(2021, "chimpanzee",bayes.b , D2021.b)
colnames(values.b.2021) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.b.2021

#-----------------------#
#### Maximum likelihood
#-----------------------#

#-----------------------#
#### R2 - Variance estimation
#-----------------------#
D2021.ml.R2 <- cbind(0.6013, 0.9018, 1.3527)

ml.d.R2 <- "R2"


values.ml.R2.2021 <- data.frame(2021, "chimpanzee",ml.d.R2, D2021.ml.R2)
colnames(values.ml.R2.2021) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.R2.2021

#-----------------------#
#### O2 - Variance estimation
#-----------------------#
D2021.ml.O2 <- cbind(0.6484, 0.9018, 1.2544)

ml.d.O2 <- "O2"

values.ml.O2.2021 <- data.frame(2021, "chimpanzee",ml.d.O2, D2021.ml.O2)
colnames(values.ml.O2.2021) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.O2.2021

#-----------------------#
#### S2 - Variance estimation
#-----------------------#
D2021.ml.S2 <- cbind(0.6988, 0.9018, 1.1639)

ml.d.S2 <- "S2"

values.ml.S2.2021 <- data.frame(2021, "chimpanzee",ml.d.S2, D2021.ml.S2)
colnames(values.ml.S2.2021) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.ml.S2.2021


#-----------------------#
####### Elephants 2007
#-----------------------#
#Elephants


#-----------------------#
### Maramagambo - FR
#-----------------------#


#-----------------------#
#### Bayesian inference - JAGS
#-----------------------#

D2021.m.b <- cbind(Mara.V23_2007.cds$q2.5$DMara23,
                   
                   Mara.V23_2007.cds$mean$DMara23, 
                   
                   Mara.V23_2007.cds$q97.5$DMara23)


bayes.m.b <- "JAGS"

values.m.b.2007 <- data.frame(2007, "Elephant",bayes.m.b , D2021.m.b)
colnames(values.m.b.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.b.2007 

#-----------------------#
#### Maximum likelihood
#-----------------------#

#-----------------------#
#### R2 - Variance estimation
#-----------------------#

D2021.m.ml.R2 <- cbind(0.0762, 0.1531, 0.3074)

ml.m.d.R2 <- "R2"


values.m.ml.R2.2007 <- data.frame(2007, "Elephant",ml.m.d.R2, D2021.m.ml.R2)
colnames(values.m.ml.R2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.R2.2007

#-----------------------#
#### O2 - Variance estimation
#-----------------------#
D2021.m.ml.O2 <- cbind(0.093, 0.1531, 0.2521)

ml.m.d.O2 <- "O2"


values.m.ml.O2.2007 <- data.frame(2007, "Elephant",ml.m.d.O2, D2021.m.ml.O2)
colnames(values.m.ml.O2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.O2.2007

#-----------------------#
#### S2 - Variance estimation
#-----------------------#
D2021.m.ml.S2 <- cbind(0.0839, 0.1531 , 0.2793)

ml.m.d.S2 <- "S2"


values.m.ml.S2.2007 <- data.frame(2007, "Elephant",ml.m.d.S2, D2021.m.ml.S2)
colnames(values.m.ml.S2.2007) <- c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.S2.2007 

#-----------------------#
### Elephants - 2021
#-----------------------#

#-----------------------#
#### Maramangambo - FR
#-----------------------#


#-----------------------#
#### Bayesian inference - JAGS
#-----------------------#

D2021.m.b <- cbind(Mara.V23_2021.cds$q2.5$DMara23,
                   
                   Mara.V23_2021.cds$mean$DMara23, 
                   
                   Mara.V23_2021.cds$q97.5$DMara23)


bayes.m.b <- "JAGS"

values.m.b.2021 <- data.frame(2021, "Elephant",bayes.m.b , D2021.m.b)
colnames(values.m.b.2021) <-  c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.b.2021

#-----------------------#
#### Maximum likelihood
#-----------------------#

#-----------------------#
#### R2 - Variance estimation
#-----------------------#

D2021.m.ml.R2 <- cbind(0.36, 0.5699, 0.9023)

ml.m.d.R2 <- "R2"


values.m.ml.R2.2021 <- data.frame(2021, "Elephant",ml.m.d.R2, D2021.m.ml.R2)
colnames(values.m.ml.R2.2021) <-  c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.R2.2021


#-----------------------#
#### O2 - Variance estimation
#-----------------------#
D2021.m.ml.O2 <- cbind(0.3194, 0.5683, 1.0113)

ml.m.d.O2 <- "O2"


values.m.ml.O2.2021 <- data.frame(2021, "Elephant",ml.m.d.O2, D2021.m.ml.O2)
colnames(values.m.ml.O2.2021) <-  c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.O2.2021

#-----------------------#
#### S2 - Variance estimation
#-----------------------#
D2021.m.ml.S2 <- cbind(0.3256, 0.5683, 0.992)

ml.m.d.S2 <- "S2"


values.m.ml.S2.2021 <- data.frame(2021, "Elephant",ml.m.d.S2, D2021.m.ml.S2)
colnames(values.m.ml.S2.2021) <-  c("year","Sp" ,"method", "lower.ci",  "mean.d", "upper.ci")
values.m.ml.S2.2021


### Create a dataframe

cds <- rbind(values.b.2007, values.ml.R2.2007, values.ml.O2.2007, values.ml.S2.2007, values.b.2021,values.ml.O2.2021,
      values.ml.S2.2021,values.m.b.2007,values.m.ml.R2.2007,values.m.ml.O2.2007,values.m.ml.S2.2007,values.m.b.2021,values.ml.R2.2021,
      values.m.ml.R2.2021,values.m.ml.O2.2021,values.m.ml.S2.2021)

cds$method <- factor(cds$method , levels=c("JAGS", "O2", "S2", "R2"))



## plotting

ggplot(data = cds) + 
  facet_grid(Sp~year, scales = "free_y") +
  geom_errorbar(aes(x = method, ymin = lower.ci, ymax = upper.ci ), color = "black", size = 0.5, width = 0) +
  geom_point(aes (x = method,y=mean.d),size = 1,  color ="black") +

labs(x = NULL, 
     y = "Density (animals per)" ~ km^2, 
     fill = "Model", 
     color = "Model") + 
  theme_classic() +
  theme(panel.spacing = unit(.08, "lines"),
       strip.background = element_rect(color = NA),
        #strip.background = element_rect(color = "black", size = 1),
         panel.border = element_rect(color = "black", fill = NA, size = 0.5),
         text = element_text(family = "Arial", size = 42),
         axis.line = element_line(color = "black", size = 0.3),
         axis.ticks = element_line(color = "black", size = 0.2), 
         axis.text = element_text(color = "black", size = 10), 
         axis.title = element_text(color = "black", size = 11), 
         strip.text = element_text(color = "black", size = 11), 
         legend.title = element_text(color = "black", size = 11), 
         legend.text = element_text(color = "black", size = 10), 
         legend.position = "none")

ggsave(
  "Figure6_CDS.jpg", 
  width = 4,
  height = 4, 
  units = "in", 
  dpi = 300
)

# panel.spacing = unit(.5, "lines"),

