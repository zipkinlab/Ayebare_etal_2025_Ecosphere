
### Author: Samuel Ayebare
## Prior_predictive_check_chimps.r

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
##---------------------##  ----> up-to 25 meters truncation distance for detecting chimpanzee nests
dist.sim <- seq(0, 25, 1)

### Possible values of the scale parameter sigma at intervals of 1
sigma <- seq(0,25,1)

#Calculate detection probability across distances
distfunc <- matrix(NA, nrow = 26, ncol = length(dist.sim))
for(i in 1:26){
  for(j in 1:length(dist.sim)){
    distfunc[i,j] <- exp(-dist.sim[j]*dist.sim[j]/(2*sigma[i]*sigma[i]))
  }
}

#### Detection probability per distance interval

Prior.predictive.check.sigma <- ggplot() +  
  geom_line(aes(x = dist.sim, y = distfunc[1,], color = "blue"), size = 1, linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[2,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[3,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[4,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[5,], color = "blue"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[6,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[7,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[8,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[9,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[10,], color = "gray0"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[11,], color = "orange"), size = 1, linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[12,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[13,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[14,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[15,], color = "orange"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[16,], color = "purple"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[17,], color = "purple"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[18,], color = "purple"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[19,], color = "purple"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[20,], color = "purple"), size = 1) +
  geom_line(aes(x = dist.sim, y = distfunc[21,], color = "red"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[22,], color = "red"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[23,], color = "red"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[24,], color = "red"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[25,], color = "red"), size = 1,linetype = "dashed") +
  geom_line(aes(x = dist.sim, y = distfunc[26,], color = "red"), size = 1,linetype = "dashed") +
  
  annotate("text", x = 25, y = 1, hjust = 0, size = 8, family = "Times New Roman", 
           label = "", fontface= "bold")

Prior.predictive.check.sigma  + labs (x="Distance(m)", y ="Detection probability", color = "Legend\n",fontface= "bold") +
  scale_color_manual(labels = c("Sigma: 0 - 4", "Sigma: 5 - 9", "Sigma: 10 - 14", "Sigma: 15 - 19",
                                "Sigma: 20 - 25"), 
                     values=c( "gray0" = "gray0", "red"= "red", "blue"="blue",
                               "orange"= "orange",  "purple"= "purple" ),name = "Sigma") +
  
  #ggtitle("Prior predictive distribution\nScale parameter")+
  ggtitle("Scale parameter\nChimpanzees")+
  
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


ggsave(file = "Prior.predictive.check.sigma_chimps.svg", bg = NULL, dpi = 300, width = 15, height = 10)
ggsave(file = "Prior.predictive.check.sigma_chimps.jpg", bg = NULL, dpi = 300, width = 15, height = 10)

