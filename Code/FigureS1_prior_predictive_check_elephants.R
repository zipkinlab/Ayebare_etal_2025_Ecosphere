

### Author: Samuel Ayebare
## Prior_predictive_check_elephants.r

## This script
# generates 
#  a) Appendix S1: Figure S1 


#-----------------------#
#-Set Working Directory-#
#-----------------------#

library(here)
setwd(here::here("Data"))

#----------------#
#-Load libraries-#
#----------------#

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(extrafont)

## distance simulation ##
##---------------------## ----> up-to 5 meters perpendicular detection for detecting elephant dung
dist.sim <- seq(0, 5, 0.1) 

### Possible values of the scale parameter sigma at intervals of 0.5
sigma <- seq(0,5,0.5)

#Calculate detection probability across distances
distfunc <- matrix(NA, nrow = 11, ncol = length(dist.sim))
for(i in 1:11){
  for(j in 1:length(dist.sim)){
    distfunc[i,j] <- exp(-dist.sim[j]*dist.sim[j]/(2*sigma[i]*sigma[i]))
  }
}

#### Detection probability per vegetation type

Prior.predictive.check.sigma <- ggplot() +  
  geom_line(aes(x = dist.sim, y = distfunc[1,], color = "blue"), size = 1, linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[2,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[3,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[4,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[5,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[6,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[7,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[8,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[9,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[10,], color = "red"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[11,], color = "red"), size = 1) +
  
  annotate("text", x = 5, y = 1, hjust = 0, size = 8, family = "Times New Roman", 
           label = "", fontface= "bold")

Prior.predictive.check.sigma  + labs (x="Distance(m)", y ="Detection probability", color = "Legend\n",fontface= "bold") +
  scale_color_manual(labels = c("Sigma: 0 - 1", "Sigma: 1.5 - 2.5", "Sigma: 3 - 4", "Sigma: 4.5 - 5"), 
                     values=c("blue"="blue", "gray0" = "gray0", 
                               "orange"= "orange","red"= "red" ),name = "Sigma") +

  #ggtitle("Prior predictive distribution\nScale parameter")+
  ggtitle("Scale parameter\nElephants")+
  
  
  theme_few() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Arial", size = 35),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(), axis.line = element_line(colour = "black", linewidth = 1.5),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.x = element_text( hjust = 0.5, vjust = 0.5,size=42, color="black"),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,size=42, color = "black"),
        axis.title.y = element_text(size = 42, angle = 90),
        axis.title.x = element_text(size = 42, angle = 00),
        legend.text=element_text(size=30),
        legend.position = c(0.9,0.9), legend.title=element_text(size=30, color="black"))


ggsave(file = "Prior.predictive.check.sigma_elephants.svg", bg = NULL, dpi = 300, width = 15, height = 10)
ggsave(file = "Prior.predictive.check.sigma_elephants.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

