
### Author: Samuel Ayebare
## Figures_integrating_across_time_space_Elephants.r

## This script
# generates 
#  b) Supplementary materials – Appendix S1: Effect of covariates (Figure S6)
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
#-Elephants - 2021#
#----------------------#

#########################################
### Load model results from the hierarchical distance sampling model - Visit 1 ; 2021 survey
#########################################

load("Eles_2021.V1_mara_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visit 1 (beta0)
##############################################################

Mara.V1.2021_beta0 <- cbind(Mara.V1_2021_eles.cov$q2.5$beta0,
                            Mara.V1_2021_eles.cov$q25$beta0, 
                            Mara.V1_2021_eles.cov$mean$beta0, 
                            Mara.V1_2021_eles.cov$q75$beta0, 
                            Mara.V1_2021_eles.cov$q97.5$beta0)



Visit_1.beta0 <- c("Visit.1") 

values_1.beta0 <- data.frame(Visit_1.beta0, Mara.V1.2021_beta0)
colnames(values_1.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#########################################
### Load model results from the hierarchical distance sampling model - Visits 3 & 3 ; 2021 survey
#########################################
load("Eles_2021.V23_mara_hds.RData")

##################################################################
###### Average effect of elevation (Intercept) - Visits  2 & 3 (beta0)
##################################################################
Mara.V23.2021_beta0 <- cbind(Mara.V23_2021_eles.cov$q2.5$beta0,
                             Mara.V23_2021_eles.cov$q25$beta0, 
                             Mara.V23_2021_eles.cov$mean$beta0, 
                             Mara.V23_2021_eles.cov$q75$beta0, 
                             Mara.V23_2021_eles.cov$q97.5$beta0)


Visits_23.beta0 <- c("Visit.23") 

values_23.beta0 <- data.frame(Visits_23.beta0, Mara.V23.2021_beta0)
colnames(values_23.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")





#########################################
### Load model results from the modified hierarchical distance sampling model - 1, 2 & 3; 2021 survey
#########################################
#################################################################################################
load("Eles_2007_2021_mara_mhds.RData")


#################################################
###### Average effect of elevation (Intercept) - Visits  1, 2 & 3 ((beta0)
#################################################


Mara.V123.2021_beta0 <- cbind(Eles_2007_2021_mara$q2.5$beta0.V23.2021,
                              Eles_2007_2021_mara$q25$beta0.V23.2021, 
                              Eles_2007_2021_mara$mean$beta0.V23.2021, 
                              Eles_2007_2021_mara$q75$beta0.V23.2021, 
                              Eles_2007_2021_mara$q97.5$beta0.V23.2021)



Visits_123.beta0 <- c("Visit.123") 

values_123.beta0 <- data.frame(Visits_123.beta0, Mara.V123.2021_beta0)
colnames(values_123.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#############################
###### Box - plots - for the intercept (beta0)
###############################


average_effect_of_elev_2021  <- ggplot() + 
  
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
  
  
  
  
  ggtitle("Elephant (2021)") +
  
  coord_cartesian(ylim = c(5, 7)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = 5.785602, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 6.572358, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Average_effect_of_elev_2021_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Average_effect_of_elev_2021_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)



############################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 1 (beta1) - 2021 survey
#####################################

Mara.V1.2021_beta1 <- cbind(Mara.V1_2021_eles.cov$q2.5$beta1,
                            Mara.V1_2021_eles.cov$q25$beta1, 
                            Mara.V1_2021_eles.cov$mean$beta1, 
                            Mara.V1_2021_eles.cov$q75$beta1, 
                            Mara.V1_2021_eles.cov$q97.5$beta1)



Visit_1.beta1 <- c("Visit.1") 

values_1.beta1 <- data.frame(Visit_1.beta1, Mara.V1.2021_beta1)
colnames(values_1.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")

#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 2 & 3 (beta1) - 2021 survey
#####################################


Mara.V23.2021_beta1 <- cbind(Mara.V23_2021_eles.cov$q2.5$beta1,
                             Mara.V23_2021_eles.cov$q25$beta1, 
                             Mara.V23_2021_eles.cov$mean$beta1, 
                             Mara.V23_2021_eles.cov$q75$beta1, 
                             Mara.V23_2021_eles.cov$q97.5$beta1)


Visits_23.beta1 <- c("Visit.23") 

values_23.beta1 <- data.frame(Visits_23.beta1, Mara.V23.2021_beta1)
colnames(values_23.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



# Modified hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 1, 2 & 3 (beta1) - 2021 survey
#####################################


Mara.V123.2021_beta1 <- cbind(Eles_2007_2021_mara$q2.5$beta1.21,
                              Eles_2007_2021_mara$q25$beta1.21, 
                              Eles_2007_2021_mara$mean$beta1.21, 
                              Eles_2007_2021_mara$q75$beta1.21, 
                              Eles_2007_2021_mara$q97.5$beta1.21)



Visits_123.beta1 <- c("Visit.123") 

values_123.beta1 <- data.frame(Visits_123.beta1, Mara.V123.2021_beta1)
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
  
  
  
  
  ggtitle("Elephant (2021)") +
  
  coord_cartesian(ylim = c(-1.5, 0)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = -1.2175, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept =  -0.5599361, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Linear_effect_of_elev_2021_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Linear_effect_of_elev_2021_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)




############################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visit 1 (beta2) - 2021 survey
#####################################

Mara.V1.2021_beta2 <- cbind(Mara.V1_2021_eles.cov$q2.5$beta2,
                            Mara.V1_2021_eles.cov$q25$beta2, 
                            Mara.V1_2021_eles.cov$mean$beta2, 
                            Mara.V1_2021_eles.cov$q75$beta2, 
                            Mara.V1_2021_eles.cov$q97.5$beta2)



Visit_1.beta2 <- c("Visit.1") 

values_1.beta2 <- data.frame(Visit_1.beta2, Mara.V1.2021_beta2)
colnames(values_1.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 2 & 3 (beta2) - 2021 survey
#####################################


Mara.V23.2021_beta2 <- cbind(Mara.V23_2021_eles.cov$q2.5$beta2,
                             Mara.V23_2021_eles.cov$q25$beta2, 
                             Mara.V23_2021_eles.cov$mean$beta2, 
                             Mara.V23_2021_eles.cov$q75$beta2, 
                             Mara.V23_2021_eles.cov$q97.5$beta2)


Visits_23.beta2 <- c("Visit.23") 

values_23.beta2 <- data.frame(Visits_23.beta2, Mara.V23.2021_beta2)
colnames(values_23.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


# Modified hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 1, 2 & 3 (beta2.21) - 2021 survey
#####################################


Mara.V123.2021_beta2 <- cbind(Eles_2007_2021_mara$q2.5$beta2.21,
                              Eles_2007_2021_mara$q25$beta2.21, 
                              Eles_2007_2021_mara$mean$beta2.21, 
                              Eles_2007_2021_mara$q75$beta2.21, 
                              Eles_2007_2021_mara$q97.5$beta2.21)



Visits_123.beta2 <- c("Visit.123") 

values_123.beta2 <- data.frame(Visits_123.beta2, Mara.V123.2021_beta2)
colnames(values_123.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")






Quadratic_effect_of_elev_2021  <- ggplot() + 
  
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
  
  
  
  
  ggtitle("Elephant (2021)") +
  
  coord_cartesian(ylim = c(-1.2, 0.5)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = -0.5388523, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = -0.0117424, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Quadratic_effect_of_elev_2021_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Quadratic_effect_of_elev_2021_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)







############################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####



####-----------------------####
###############################
#####Elephants - 2007 surveys
###############################
####-----------------------####

#########################################
### Load model results from the hierarchical distance sampling model - Visit 1 ; 2007 survey
#########################################


load("Eles_2007.V1_mara_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visit 1 (beta0); 2007 survey
##############################################################

Mara.V1.2007_beta0 <- cbind(Mara.V1_2007_eles.cov$q2.5$beta0,
                            Mara.V1_2007_eles.cov$q25$beta0, 
                            Mara.V1_2007_eles.cov$mean$beta0, 
                            Mara.V1_2007_eles.cov$q75$beta0, 
                            Mara.V1_2007_eles.cov$q97.5$beta0)



Visit_1.beta0 <- c("Visit.1") 

values_1.beta0 <- data.frame(Visit_1.beta0, Mara.V1.2007_beta0)
colnames(values_1.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#########################################
### Load model results from the hierarchical distance sampling model - Visits 2 & 3 ; 2007 survey
#########################################

load("Eles_2007.V23_mara_hds.RData")


#############################################################
###### Average effect of elevation (Intercept) - Visits 2 & 3 (beta0); 2007 survey
##############################################################

Mara.V23.2007_beta0 <- cbind(Mara.V23_2007_eles.cov$q2.5$beta0,
                             Mara.V23_2007_eles.cov$q25$beta0, 
                             Mara.V23_2007_eles.cov$mean$beta0, 
                             Mara.V23_2007_eles.cov$q75$beta0, 
                             Mara.V23_2007_eles.cov$q97.5$beta0)



Visits_23.beta0 <- c("Visit.23") 

values_23.beta0 <- data.frame(Visits_23.beta0, Mara.V23.2007_beta0)
colnames(values_23.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#########################################
### Load model results from the modified hierarchical distance sampling model - Visits 1, 2 & 3 ; 2007 survey
#########################################
load("Eles_2007_2021_mara_mhds.RData")

#############################################################
###### Average effect of elevation (Intercept) - Visits 1, 2 & 3 (beta0.V23.2007
##############################################################


Mara.V123.2007_beta0 <- cbind(Eles_2007_2021_mara$q2.5$beta0.V23.2007,
                              Eles_2007_2021_mara$q25$beta0.V23.2007, 
                              Eles_2007_2021_mara$mean$beta0.V23.2007, 
                              Eles_2007_2021_mara$q75$beta0.V23.2007, 
                              Eles_2007_2021_mara$q97.5$beta0.V23.2007)



Visits_123.beta0 <- c("Visit.123") 

values_123.beta0 <- data.frame(Visits_123.beta0, Mara.V123.2007_beta0)
colnames(values_123.beta0) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")




########################
##### Box plot
#########################


average_effect_of_elev_2021  <- ggplot() + 
  
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
  
  
  
  
  ggtitle("Elephant (2007)") +
  
  coord_cartesian(ylim = c(3, 7)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = 3.761008, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 4.822472, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Average_effect_of_elev_2007_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Average_effect_of_elev_2007_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)




###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####



#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visit 1 (beta1) ; 2007 survey
#####################################

Mara.V1.2007_beta1 <- cbind(Mara.V1_2007_eles.cov$q2.5$beta1,
                            Mara.V1_2007_eles.cov$q25$beta1, 
                            Mara.V1_2007_eles.cov$mean$beta1, 
                            Mara.V1_2007_eles.cov$q75$beta1, 
                            Mara.V1_2007_eles.cov$q97.5$beta1)



Visit_1.beta1 <- c("Visit.1") 

values_1.beta1 <- data.frame(Visit_1.beta1, Mara.V1.2007_beta1)
colnames(values_1.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")

#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 2 & 3 (beta1) ; 2007 survey
#####################################


Mara.V23.2007_beta1 <- cbind(Mara.V23_2007_eles.cov$q2.5$beta1,
                             Mara.V23_2007_eles.cov$q25$beta1, 
                             Mara.V23_2007_eles.cov$mean$beta1, 
                             Mara.V23_2007_eles.cov$q75$beta1, 
                             Mara.V23_2007_eles.cov$q97.5$beta1)


Visits_23.beta1 <- c("Visit.23") 

values_23.beta1 <- data.frame(Visits_23.beta1, Mara.V23.2007_beta1)
colnames(values_23.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")


#hierarchical distance sampling model
#####################################
###### Linear effect of elevation - Visits 1, 2 & 3 (beta1.21) ; 2007 survey
#####################################

Mara.V123.2007_beta1 <- cbind(Eles_2007_2021_mara$q2.5$beta1.07,
                              Eles_2007_2021_mara$q25$beta1.07, 
                              Eles_2007_2021_mara$mean$beta1.07, 
                              Eles_2007_2021_mara$q75$beta1.07, 
                              Eles_2007_2021_mara$q97.5$beta1.07)



Visits_123.beta1 <- c("Visit.123") 

values_123.beta1 <- data.frame(Visits_123.beta1, Mara.V123.2007_beta1)
colnames(values_123.beta1) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#### Box plots
########################


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
  
  
  
  
  ggtitle("Elephant (2007)") +
  
  coord_cartesian(ylim = c(-3.5, 0)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = -1.537025, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = -0.8285263, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Linear_effect_of_elev_2007_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Linear_effect_of_elev_2007_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)



###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visit 1 (beta2) - 2007 
#####################################

Mara.V1.2007_beta2 <- cbind(Mara.V1_2007_eles.cov$q2.5$beta2,
                            Mara.V1_2007_eles.cov$q25$beta2, 
                            Mara.V1_2007_eles.cov$mean$beta2, 
                            Mara.V1_2007_eles.cov$q75$beta2, 
                            Mara.V1_2007_eles.cov$q97.5$beta2)



Visit_1.beta2 <- c("Visit.1") 

values_1.beta2 <- data.frame(Visit_1.beta2, Mara.V1.2007_beta2)
colnames(values_1.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 2 & 3 (beta2) - 2007 
#####################################

Mara.V23.2007_beta2 <- cbind(Mara.V23_2007_eles.cov$q2.5$beta2,
                             Mara.V23_2007_eles.cov$q25$beta2, 
                             Mara.V23_2007_eles.cov$mean$beta2, 
                             Mara.V23_2007_eles.cov$q75$beta2, 
                             Mara.V23_2007_eles.cov$q97.5$beta2)


Visits_23.beta2 <- c("Visit.23") 

values_23.beta2 <- data.frame(Visits_23.beta2, Mara.V23.2007_beta2)
colnames(values_23.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



#modified hierarchical distance sampling model
#####################################
###### Quadratic effect of elevation - Visits 1, 2 & 3 (beta2.07) - 2007 
#####################################

Mara.V123.2007_beta2 <- cbind(Eles_2007_2021_mara$q2.5$beta2.07,
                              Eles_2007_2021_mara$q25$beta2.07, 
                              Eles_2007_2021_mara$mean$beta2.07, 
                              Eles_2007_2021_mara$q75$beta2.07, 
                              Eles_2007_2021_mara$q97.5$beta2.07)



Visits_123.beta2 <- c("Visit.123") 

values_123.beta2 <- data.frame(Visits_123.beta2, Mara.V123.2007_beta2)
colnames(values_123.beta2) <- c("Visit", "lower", "l25", "mean.cov", "u75", "upper")



######
### Box_plots
####################




Quadratic_effect_of_elev_2007  <- ggplot() + 
  
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
  
  
  
  
  ggtitle("Elephant (2007)") +
  
  coord_cartesian(ylim = c(-1.2, 0.5)) +
  #geom_hline(yintercept = 0, alpha = 0.75, linetype = "solid" ) +
  geom_hline(yintercept = -0.33521, alpha = 0.75, linetype = "dashed" ) +
  geom_hline(yintercept = 0.1630407, alpha = 0.75, linetype = "dashed" ) +
  
  
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



ggsave(file = "Quadratic_effect_of_elev_2007_Mara_eles.svg", bg = NULL, dpi = 400, width = 15, height = 10)
ggsave(file = "Quadratic_effect_of_elev_2007_Mara_eles.jpg", bg = NULL, dpi = 400, width = 15, height = 10)





###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####

  #####Appendix S1: Figure S8: Estimated expected density of elephant dung for the 2021 surveys. 
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


expected_elephant_density <- read.csv("eles_v123_expected_density.csv", header=TRUE)
head(expected_elephant_density)

###############################
### First sixteen transects
################################

### Filter data
e_density_16 <- expected_elephant_density %>% 
                filter(Tr_id <= 16)

#### plot
ggplot(e_density_16 , aes( x=Trasects )) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      #alpha = 0.7,
                      color = `Visit`, fill = `Visit`,
                      shape = `Visit` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(23,22,21)) +
  scale_color_manual(values=c("#2166ac","#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#2166ac","#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(e_density_16$l25),
                         max(e_density_16$u95) + .5)) +
  scale_x_discrete(limits = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16" ))+
  labs(y = expression("Expected dung density"), x = expression( "Transects") ) + #,  fill = "Functional groups (birds)", 
  ggtitle("Elephant (2021): Transects 1 - 16") +
  
  
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


ggsave(file = "Expected_density_elephants_2021_T16.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_elephants_2021_T16.svg", bg = NULL, dpi = 300, width = 15, height = 10)


###############################
### Transects - Seventeen to  twenty seven transects
################################

### Filter data
e_density_17_27  <- expected_elephant_density %>% 
                   filter(Tr_id> 16  & Tr_id < 28 )


#### plot
ggplot(e_density_17_27, aes( x=Trasects )) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      #alpha = 0.7,
                      color = `Visit`, fill = `Visit`,
                      shape = `Visit` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(23,22,21)) +
  scale_color_manual(values=c("#2166ac","#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#2166ac","#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(e_density_17_27$l25),
                         max(e_density_17_27$u95) + .5)) +
  scale_x_discrete(limits = c("T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27"))+
  labs(y = expression("Expected dung density"), x = expression( "Transects") ) + 
  ggtitle("Elephant (2021) -Transects 17 - 27") +
  
  
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

ggsave(file = "Expected_density_elephants_2021_T17-27.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_elephants_2021_T17-27.svg", bg = NULL, dpi = 300, width = 15, height = 10)



#######################################
### Transects - twenty eight to  thirty six
######################################

### Filter data
e_density_28_36 <- expected_elephant_density %>% 
                   filter(Tr_id > 27)
head(e_density_28_36)

#### plot


ggplot(e_density_28_36, aes( x=Trasects )) +
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
  coord_cartesian(ylim=c(min(e_density_28_36$l25),
                         max(e_density_28_36$u95) + .5)) +
  scale_x_discrete(limits = c("T28","T29","T30","T31","T32","T33","T34","T35","T36"))+
  labs(y = expression("Expected dung density"), x = expression( "Transects") ) + 
  ggtitle("Elephant (2021) -Transects 28 - 36") +
  
  
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


ggsave(file = "Expected_density_elephants_2021_T28-36.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

ggsave(file = "Expected_density_elephants_2021_T28-36.svg", bg = NULL, dpi = 300, width = 15, height = 10)






