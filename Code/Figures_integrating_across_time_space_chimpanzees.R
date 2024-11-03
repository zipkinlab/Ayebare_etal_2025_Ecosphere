
### Author: Samuel Ayebare
## Figures_integrating_across_time_space.r

## This script
# generates 
#  a) Figures 5; 
#  b) Supplementary materials – Appendix S1: Effect of covariates (Figures S5)
#  c) Supplementary materials – Appendix S1: Expected sign density along transects (Figure S7)

#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))

#----------------#
#-Load libraries-#
#----------------#

library(jagsUI)
library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)
library(extrafont)


#----------------------#
#-Chimpanzees - 2021#
#----------------------#

#########################################
### Load model results from the hierarchical distance sampling model - Visit 1 ; 2021 survey
#########################################

load("Mara.Kal_2021.V1_chimps_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visit 1 (beta0)
##############################################################

Mara.Kal.V1.2021_beta0 <- cbind(Mara.Kal_2021.V1.cov$q2.5$beta0,
                                Mara.Kal_2021.V1.cov$q25$beta0, 
                                Mara.Kal_2021.V1.cov$mean$beta0, 
                                Mara.Kal_2021.V1.cov$q75$beta0, 
                                Mara.Kal_2021.V1.cov$q97.5$beta0)


Visit_1.beta0 <- c("Visit.1") 

values_1.beta0 <- data.frame(Visit_1.beta0, Mara.Kal.V1.2021_beta0)
colnames(values_1.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#########################################
### Load model results from the hierarchical distance sampling model - Visits 3 & 3 ; 2021 survey
#########################################
load("Mara.Kal_2021.V23_chimps_hds.RData")

##################################################################
###### Average effect of elevation (Intercept) - Visits  2 & 3 (beta0)
##################################################################

Mara.Kal.V23.2021_beta0 <- cbind(Mara.Kal_2021.V23.cov$q2.5$beta0,
                                 Mara.Kal_2021.V23.cov$q25$beta0, 
                                 Mara.Kal_2021.V23.cov$mean$beta0, 
                                 Mara.Kal_2021.V23.cov$q75$beta0, 
                                 Mara.Kal_2021.V23.cov$q97.5$beta0)



Visits_23.beta0 <- c("Visit.23") 

values_23.beta0 <- data.frame(Visits_23.beta0, Mara.Kal.V23.2021_beta0)
colnames(values_23.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")




#########################################
### Load model results from the modified hierarchical distance sampling model - 1, 2 & 3; 2021 survey
#########################################
#################################################################################################
load("Mara.Kal_2007_2021_chimps_mhds.RData")




#################################################
###### Average effect of elevation (Intercept) - Visits  1, 2 & 3 ((beta0)
#################################################


Mara.Kal.V123.2021_beta0 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta0.V23.2021,
                                  Mara.Kal_2007_2021_chimps$q25$beta0.V23.2021, 
                                  Mara.Kal_2007_2021_chimps$mean$beta0.V23.2021, 
                                  Mara.Kal_2007_2021_chimps$q75$beta0.V23.2021, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta0.V23.2021)





Visits_123.beta0 <- c("Visit.123") 

values_123.beta0 <- data.frame(Visits_123.beta0, Mara.Kal.V123.2021_beta0)
colnames(values_123.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#############################
###### Box - plots - for the intercept (beta0)
###############################

average_effect_of_elev_2021 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  
  
  
  ggtitle("Chimpanzee (2021)") +
  
  coord_cartesian(ylim = c(2.5,5)) +
  geom_hline(yintercept = 3.417625 , alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 4.236298, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Effect of elevation (intercept)"), x = expression())



ggsave(file = "average_effect_of_elev_2021_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "average__effect_of_elev_2021_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)



############################################################################################################################################
      ###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 1 (beta1) - 2021 survey
#####################################

Mara.Kal.V1.2021_beta1 <- cbind(Mara.Kal_2021.V1.cov$q2.5$beta1,
                                Mara.Kal_2021.V1.cov$q25$beta1, 
                                Mara.Kal_2021.V1.cov$mean$beta1, 
                                Mara.Kal_2021.V1.cov$q75$beta1, 
                                Mara.Kal_2021.V1.cov$q97.5$beta1)




Visit_1.beta1 <- c("Visit.1") 

values_1.beta1 <- data.frame(Visit_1.beta1, Mara.Kal.V1.2021_beta1)
colnames(values_1.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



Mara.Kal.V23.2021_beta1 <- cbind(Mara.Kal_2021.V23.cov$q2.5$beta1,
                                 Mara.Kal_2021.V23.cov$q25$beta1, 
                                 Mara.Kal_2021.V23.cov$mean$beta1, 
                                 Mara.Kal_2021.V23.cov$q75$beta1, 
                                 Mara.Kal_2021.V23.cov$q97.5$beta1)


#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 2 & 3 (beta1) - 2021 survey
#####################################


Mara.Kal.V23.2021_beta1 <- cbind(Mara.Kal_2021.V23.cov$q2.5$beta1,
                                 Mara.Kal_2021.V23.cov$q25$beta1, 
                                 Mara.Kal_2021.V23.cov$mean$beta1, 
                                 Mara.Kal_2021.V23.cov$q75$beta1, 
                                 Mara.Kal_2021.V23.cov$q97.5$beta1)


Visits_23.beta1 <- c("Visit.23") 

values_23.beta1 <- data.frame(Visits_23.beta1, Mara.Kal.V23.2021_beta1)
colnames(values_23.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



# Modified hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 1, 2 & 3 (beta1) - 2021 survey
#####################################

Mara.Kal.V123.2021_beta1 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta1.21,
                                  Mara.Kal_2007_2021_chimps$q25$beta1.21, 
                                  Mara.Kal_2007_2021_chimps$mean$beta1.21, 
                                  Mara.Kal_2007_2021_chimps$q75$beta1.21, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta1.21)



Visits_123.beta1 <- c("Visit.123") 

values_123.beta1 <- data.frame(Visits_123.beta1, Mara.Kal.V123.2021_beta1)
colnames(values_123.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



### Boxplots

Linear_effect_of_elev_2021 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  
  
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  
  
  
  ggtitle("Chimpanzee (2021)") +
  
  coord_cartesian(ylim = c(0, 2.5)) +
  geom_hline(yintercept = 0.6920453, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 1.566195, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Linear effect of elevation"), x = expression())



ggsave(file = "Linear_effect_of_elev_2021_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Linear_effect_of_elev_2021_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)



############################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visit 1 (beta2) - 2021 survey
#####################################

Mara.Kal.V1.2021_beta2 <- cbind(Mara.Kal_2021.V1.cov$q2.5$beta2,
                                Mara.Kal_2021.V1.cov$q25$beta2, 
                                Mara.Kal_2021.V1.cov$mean$beta2, 
                                Mara.Kal_2021.V1.cov$q75$beta2, 
                                Mara.Kal_2021.V1.cov$q97.5$beta2)




Visit_1.beta2 <- c("Visit.1") 

values_1.beta2 <- data.frame(Visit_1.beta2, Mara.Kal.V1.2021_beta2)
colnames(values_1.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 2 & 3 (beta2) - 2021 survey
#####################################

Mara.Kal.V23.2021_beta2 <- cbind(Mara.Kal_2021.V23.cov$q2.5$beta2,
                                 Mara.Kal_2021.V23.cov$q25$beta2, 
                                 Mara.Kal_2021.V23.cov$mean$beta2, 
                                 Mara.Kal_2021.V23.cov$q75$beta2, 
                                 Mara.Kal_2021.V23.cov$q97.5$beta2)


Visits_23.beta2 <- c("Visit.23") 

values_23.beta2 <- data.frame(Visits_23.beta2, Mara.Kal.V23.2021_beta2)
colnames(values_23.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")

# Modified hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 1, 2 & 3 (beta2.21) - 2021 survey
#####################################

Mara.Kal.V123.2021_beta2 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta2.21,
                                  Mara.Kal_2007_2021_chimps$q25$beta2.21, 
                                  Mara.Kal_2007_2021_chimps$mean$beta2.21, 
                                  Mara.Kal_2007_2021_chimps$q75$beta2.21, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta2.21)





Visits_123.beta2 <- c("Visit.123") 

values_123.beta2 <- data.frame(Visits_123.beta2, Mara.Kal.V123.2021_beta2)
colnames(values_123.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#### Boxplots
######################

Quadratic_effect_of_elev_2021 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  
  
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  
  
  
  ggtitle("Chimpanzee (2021)") +
  
  coord_cartesian(ylim = c(-1,1)) +
  geom_hline(yintercept = -0.5476015 , alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 0.09639164, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Quadratic effect of elevation"), x = expression())



ggsave(file = "Quadratic_effect_of_elev_2021_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Quadratic_effect_of_elev_2021_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)




############################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####



####-----------------------####
###############################
#####Chimpanzees - 2007 surveys
###############################
####-----------------------####

#########################################
### Load model results from the hierarchical distance sampling model - Visit 1 ; 2007 survey
#########################################


load("Mara.Kal_2007.V1_chimps_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visit 1 (beta0); 2007 survey
##############################################################

Mara.Kal.V1.2007_beta0 <- cbind(Mara.Kal_2007.V1.cov$q2.5$beta0,
                                Mara.Kal_2007.V1.cov$q25$beta0, 
                                Mara.Kal_2007.V1.cov$mean$beta0, 
                                Mara.Kal_2007.V1.cov$q75$beta0, 
                                Mara.Kal_2007.V1.cov$q97.5$beta0)




Visit_1.beta0 <- c("Visit.1") 

values_1.beta0 <- data.frame(Visit_1.beta0, Mara.Kal.V1.2007_beta0)
colnames(values_1.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#########################################
### Load model results from the hierarchical distance sampling model - Visits 2 & 3 ; 2007 survey
#########################################


load("Mara.Kal_2007.V23_chimps_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visits 2 & 3 (beta0); 2007 survey
##############################################################
Mara.Kal.V23.2007_beta0 <- cbind(Mara.Kal_2007.V23.cov$q2.5$beta0,
                                 Mara.Kal_2007.V23.cov$q25$beta0, 
                                 Mara.Kal_2007.V23.cov$mean$beta0, 
                                 Mara.Kal_2007.V23.cov$q75$beta0, 
                                 Mara.Kal_2007.V23.cov$q97.5$beta0)


ls()


Visits_23.beta0 <- c("Visit.23") 

values_23.beta0 <- data.frame(Visits_23.beta0, Mara.Kal.V23.2007_beta0)
colnames(values_23.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")




#########################################
### Load model results from the modified hierarchical distance sampling model - Visits 1, 2 & 3 ; 2007 survey
#########################################
load("Mara.Kal_2007_2021_chimps_mhds.RData")

#############################################################
###### Average effect of elevation (Intercept) - Visits 1, 2 & 3 (beta0.V23.2007
##############################################################

Mara.Kal.V123.2007_beta0 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta0.V23.2007,
                                  Mara.Kal_2007_2021_chimps$q25$beta0.V23.2007, 
                                  Mara.Kal_2007_2021_chimps$mean$beta0.V23.2007, 
                                  Mara.Kal_2007_2021_chimps$q75$beta0.V23.2007, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta0.V23.2007)

Visits_123.beta0 <- c("Visit.123") 

values_123.beta0 <- data.frame(Visits_123.beta0, Mara.Kal.V123.2007_beta0)
colnames(values_123.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


##############
### Boxplots
###################

average_effect_of_elev_2007 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  
  
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta0, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  ggtitle("Chimpanzee (2007)") +
  
  coord_cartesian(ylim = c(3,6)) +
  geom_hline(yintercept = 3.73267 , alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 4.565584, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Effect of elevation (intercept)"), x = expression())



ggsave(file = "average_effect_of_elev_2007_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "average__effect_of_elev_2007_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)


###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####



#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 1 (beta1) ; 2007 survey
#####################################
Mara.Kal.V1.2007_beta1 <- cbind(Mara.Kal_2007.V1.cov$q2.5$beta1,
                                Mara.Kal_2007.V1.cov$q25$beta1, 
                                Mara.Kal_2007.V1.cov$mean$beta1, 
                                Mara.Kal_2007.V1.cov$q75$beta1, 
                                Mara.Kal_2007.V1.cov$q97.5$beta1)




Visit_1.beta1 <- c("Visit.1") 

values_1.beta1 <- data.frame(Visit_1.beta1, Mara.Kal.V1.2007_beta1)
colnames(values_1.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 2 & 3 (beta1) ; 2007 survey
#####################################
Mara.Kal.V23.2007_beta1 <- cbind(Mara.Kal_2007.V23.cov$q2.5$beta1,
                                 Mara.Kal_2007.V23.cov$q25$beta1, 
                                 Mara.Kal_2007.V23.cov$mean$beta1, 
                                 Mara.Kal_2007.V23.cov$q75$beta1, 
                                 Mara.Kal_2007.V23.cov$q97.5$beta1)


Visits_23.beta1 <- c("Visit.23") 

values_23.beta1 <- data.frame(Visits_23.beta1, Mara.Kal.V23.2007_beta1)
colnames(values_23.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")




#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 1, 2 & 3 (beta1.21) ; 2007 survey
#####################################


Mara.Kal.V123.2007_beta1 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta1.07,
                                  Mara.Kal_2007_2021_chimps$q25$beta1.07, 
                                  Mara.Kal_2007_2021_chimps$mean$beta1.07, 
                                  Mara.Kal_2007_2021_chimps$q75$beta1.07, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta1.07)


Visits_123.beta1 <- c("Visit.123") 

values_123.beta1 <- data.frame(Visits_123.beta1, Mara.Kal.V123.2007_beta1)
colnames(values_123.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



### Boxplots
###############


Linear_effect_of_elev_2007 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  
  
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta1, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  
  
  
  ggtitle("Chimpanzee (2007)") +
  
  coord_cartesian(ylim = c(0.5, 2.5)) +
  geom_hline(yintercept = 1.110924, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 1.962739, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Linear effect of elevation"), x = expression())



ggsave(file = "Linear_effect_of_elev_2007_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Linear_effect_of_elev_2007_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)



###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visit 1 (beta2) - 2007 
#####################################
Mara.Kal.V1.2007_beta2 <- cbind(Mara.Kal_2007.V1.cov$q2.5$beta2,
                                Mara.Kal_2007.V1.cov$q25$beta2, 
                                Mara.Kal_2007.V1.cov$mean$beta2, 
                                Mara.Kal_2007.V1.cov$q75$beta2, 
                                Mara.Kal_2007.V1.cov$q97.5$beta2)




Visit_1.beta2 <- c("Visit.1") 

values_1.beta2 <- data.frame(Visit_1.beta2, Mara.Kal.V1.2007_beta2)
colnames(values_1.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 2 & 3 (beta2) - 2007 
#####################################

Mara.Kal.V23.2007_beta2 <- cbind(Mara.Kal_2007.V23.cov$q2.5$beta2,
                                 Mara.Kal_2007.V23.cov$q25$beta2, 
                                 Mara.Kal_2007.V23.cov$mean$beta2, 
                                 Mara.Kal_2007.V23.cov$q75$beta2, 
                                 Mara.Kal_2007.V23.cov$q97.5$beta2)


Visits_23.beta2 <- c("Visit.23") 

values_23.beta2 <- data.frame(Visits_23.beta2, Mara.Kal.V23.2007_beta2)
colnames(values_23.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#modified hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 1, 2 & 3 (beta2.07) - 2007 
#####################################

Mara.Kal.V123.2007_beta2 <- cbind(Mara.Kal_2007_2021_chimps$q2.5$beta2.07,
                                  Mara.Kal_2007_2021_chimps$q25$beta2.07, 
                                  Mara.Kal_2007_2021_chimps$mean$beta2.07, 
                                  Mara.Kal_2007_2021_chimps$q75$beta2.07, 
                                  Mara.Kal_2007_2021_chimps$q97.5$beta2.07)





Visits_123.beta2 <- c("Visit.123") 

values_123.beta2 <- data.frame(Visits_123.beta2, Mara.Kal.V123.2007_beta2)
colnames(values_123.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")




Quandratic_effect_of_elev_2007 <- ggplot() + 
  
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_1.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "black"), 
                width = 0.1) +
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "black"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_23.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "black"), 
                size = 6, width = 0)  +
  
  
  
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = mean.cov, ymax = mean.cov, color = "orange"), 
                width = 0.1) +
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = lower, ymax = upper, color = "orange"), 
                size = 1.25, width = 0) +
  geom_errorbar(data = values_123.beta2, aes(x = Visit, ymin = l25, ymax = u75, color = "orange"), 
                size = 6, width = 0) +
  
  
  
  
  ggtitle("Chimpanzee (2007)") +
  
  coord_cartesian(ylim = c(-1,0)) +
  geom_hline(yintercept = -0.8460823 , alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = -0.2567458, alpha = 0.75, linetype = "dashed" ) +
  
  
  scale_color_manual(name = "", values = c("black" = "black","black" = "black","orange" = "orange")) +
  theme_few() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 42),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5,size=42, color= "black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42,color = "black"),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        legend.position = "none") +
  
  
  scale_x_discrete(limits =c("Visit.1", "Visit.123", "Visit.23"),
                   labels = c("Visit.1" = "Visit 1", "Visit.23" = "Visits 2 & 3", 
                              "Visit.123" = "Visits 1, 2 & 3")) +
  
  
  
  labs(y = expression("Quadratic effect of elevation"), x = expression())



ggsave(file = "Quadratic_effect_of_elev_2007_Mara_Kal_chimps.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Quadratic_effect_of_elev_2007_Mara_Kal_chimps.jpg", bg = NULL, dpi = 400, width = 15, height = 10)





###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#####Appendix S1: Figure S8: Estimated expected density of chimpanzee nests for the 2021 surveys. 
###------------------------------------------------------------------------------------------------------------------------------####



library(tidyverse)
library(ggthemes)
library(ggtext)
library(extrafont)


#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))


expected_chimp_density <- read.csv("chimps_v123_expected_density_2021.csv", header=TRUE)
head(expected_chimp_density)

###############################
### First sixteen transects
################################

### Filter data
chimp_e_density_16 <- expected_chimp_density %>% 
  filter(Tr_id <= 16)



###################
#### plot
################

ggplot(chimp_e_density_16, aes( x=Trasects )) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Visit`, fill = `Visit`,
                      shape = `Visit` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(23,22,21)) +
  scale_color_manual(values=c("#2166ac","#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#2166ac","#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(chimp_e_density_16$l25),
                         max(chimp_e_density_16$u95) + .5)) +
  scale_x_discrete(limits = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16"))+
  
  labs(y = expression("Expected nest density"), x = expression( "Transects") ) + 
  ggtitle("Chimpanzee (2021): Transects 1 - 16") +
  
  
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 35),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 0.25),
        axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        axis.text.y = element_text(hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        line = element_line(linewidth = 1))

ggsave(file = "Expected_density_chimps_2021_tr1_16.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_chimps_2021_tr1_16.svg", bg = NULL, dpi = 300, width = 15, height = 10)




###############################
### Transects - Seventeen to  thirty two
################################

### Filter data
chimp_e_density_17_32  <- expected_chimp_density  %>% 
  filter(Tr_id > 16  & Tr_id <= 32 )

chimp_e_density_17_32 

###################
#### plot
################

ggplot(chimp_e_density_17_32, aes( x=Trasects )) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Visit`, fill = `Visit`,
                      shape = `Visit` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(23,22,21)) +
  scale_color_manual(values=c("#2166ac","#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#2166ac","#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(chimp_e_density_17_32$l25),
                         max(chimp_e_density_17_32$u95) + .5)) +
  scale_x_discrete(limits = c("T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",
                              "T29","T30","T31","T32"))+
  
  labs(y = expression("Expected nest density"), x = expression( "Transects") ) + 
  ggtitle("Chimpanzee (2021): Transects 17 - 32") +
  
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 35),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 0.25),
        axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        axis.text.y = element_text(hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        line = element_line(linewidth = 1))

ggsave(file = "Expected_density_chimps_2021_T17_32.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_chimps_2021_T17_32.svg", bg = NULL, dpi = 300, width = 15, height = 10)





###############################
### Transects -  thirty three to forty six
################################

### Filter data
chimp_e_density_33_46  <- expected_chimp_density  %>% 
  filter(Tr_id > 32 )

chimp_e_density_33_46

###################
#### plot
################

ggplot(chimp_e_density_33_46, aes( x=Trasects )) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Visit`, fill = `Visit`,
                      shape = `Visit` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(23,22,21)) +
  scale_color_manual(values=c("#2166ac","#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#2166ac","#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(chimp_e_density_33_46$l25),
                         max(chimp_e_density_33_46$u95) + .5)) +
  scale_x_discrete(limits = c("T33","T34","T35","T36","T37","T38","T39","T40","T41",
                              "T42","T43","T44","T45","T46"))+
  
  labs(y = expression("Expected nest density"), x = expression( "Transects") ) + 
  ggtitle("Chimpanzee (2021): Transects 17 - 32") +
  
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 35),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 0.25),
        axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        axis.text.y = element_text(hjust = 0.5,vjust = 0.5,size=35, color= "black"),
        legend.text=element_text(size=30),
        legend.title=element_text(size=30),
        axis.title.x = element_text( size = 42,colour = "black"),
        axis.title.y = element_text( size = 42,colour = "black"),
        line = element_line(linewidth = 1))

ggsave(file = "Expected_density_chimps_2021_T33_46.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_chimps_2021_T33_46.svg", bg = NULL, dpi = 300, width = 15, height = 10)












