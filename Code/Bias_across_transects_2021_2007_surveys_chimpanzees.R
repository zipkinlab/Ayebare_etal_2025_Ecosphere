
### Author: Samuel Ayebare
## Bias_across_transects_2021_2007_surveys_chimpanzees.r

## This script
# generates 
#  a) Figures 5; 
#  b) Supplementary materials – Appendix S1: Relative bias (Figures S3 & S4)

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

### Import bias estimates for the 2021 & 2007 surveys

### 2021
bias_chimps_hds_mhds_2021 <- read.csv("Bias_chimps_hds_mhds_2021.csv", header=TRUE)
head(bias_chimps_hds_mhds_2021)

bias_chimps_hds_mhds_2007 <- read.csv("Bias_chimps_hds_mhds_2007.csv", header=TRUE)
head(bias_chimps_hds_mhds_2007)


##### First sixteen transects
#### Chimps - 2021


### Filter data
bias_chimps2021_tr1_16 <- bias_chimps_hds_mhds_2021 %>% 
                          filter( Tr_id < 17)

head(bias_chimps2021_tr1_16)

#### plot


ggplot(bias_chimps2021_tr1_16 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      #alpha = 0.7,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2021_tr1_16$l25),
                         max(bias_chimps2021_tr1_16$u95) )) +
  scale_x_discrete(limits = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +  
  
  ggtitle("Chimpanzee nests (2021): Transects 1 - 16") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2021_tr1_16.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2021_tr1_16.svg", bg = NULL, dpi = 500, width = 15, height = 10)





###############################
### Transects - Seventeen to  thirty two
################################

### Filter data

bias_chimps2021_tr17_32 <-  bias_chimps_hds_mhds_2021 %>% 
                           filter(Tr_id > 16  & Tr_id < 33 )

head(bias_chimps2021_tr17_32)


#### plot

ggplot(bias_chimps2021_tr17_32 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2021_tr17_32$l25),
                         max(bias_chimps2021_tr17_32$u95) )) +
  scale_x_discrete(limits = c("T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",
                              "T29","T30","T31","T32"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +    
  
  ggtitle("Chimpanzee nests (2021): Transects 17 - 32") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2021_tr17_32.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2021_tr17_32.svg", bg = NULL, dpi = 500, width = 15, height = 10)







###############################
### Transects -  thirty three to forty six
################################

### Filter data

bias_chimps2021_tr33_46 <-  bias_chimps_hds_mhds_2021 %>% 
                             filter(Tr_id > 32)

head(bias_chimps2021_tr33_46)


 
#### plot

ggplot(bias_chimps2021_tr33_46 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2021_tr33_46$l25),
                         max(bias_chimps2021_tr33_46$u95) )) +
  scale_x_discrete(limits = c("T33","T34","T35","T36","T37","T38","T39","T40","T41",
                              "T42","T43","T44","T45","T46"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +    
  
  ggtitle("Chimpanzee nests (2021): Transects 33 - 46") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2021_tr33_46.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2021_tr33_46.svg", bg = NULL, dpi = 500, width = 15, height = 10)







###########################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------####
####### Chimpanzees - 2007 surveys




##### First sixteen transects
#### Chimps - 2007


### Filter data
bias_chimps2007_tr1_16 <- bias_chimps_hds_mhds_2007 %>% 
  filter( Tr_id < 17)

head(bias_chimps2007_tr1_16)

#### plot


ggplot(bias_chimps2007_tr1_16 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      #alpha = 0.7,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2007_tr1_16$l25),
                         max(bias_chimps2007_tr1_16$u95) )) +
  scale_x_discrete(limits = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +  
  
  ggtitle("Chimpanzee nests (2007): Transects 1 - 16") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2007_tr1_16.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2007_tr1_16.svg", bg = NULL, dpi = 500, width = 15, height = 10)





###############################
### Transects - Seventeen to  thirty two
################################

### Filter data

bias_chimps2007_tr17_32 <-  bias_chimps_hds_mhds_2007 %>% 
  filter(Tr_id > 16  & Tr_id < 33 )

head(bias_chimps2007_tr17_32)


#### plot

ggplot(bias_chimps2007_tr17_32 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2007_tr17_32$l25),
                         max(bias_chimps2007_tr17_32$u95) )) +
  scale_x_discrete(limits = c("T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",
                              "T29","T30","T31","T32"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +    
  
  ggtitle("Chimpanzee nests (2007): Transects 17 - 32") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2007_tr17_32.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2007_tr17_32.svg", bg = NULL, dpi = 500, width = 15, height = 10)







###############################
### Transects -  thirty three to forty six
################################

### Filter data

bias_chimps2007_tr33_46 <-  bias_chimps_hds_mhds_2007 %>% 
  filter(Tr_id > 32)

head(bias_chimps2007_tr33_46)



#### plot

ggplot(bias_chimps2007_tr33_46 , aes( x=Transects )) +
  geom_hline(yintercept=0, color="gray", linetype="longdash", size = 1) +
  geom_pointrange(aes(ymin = l25, ymax = u95, y = mean,
                      color = `Model`, fill = `Model`,
                      shape = `Model` ), 
                  size = 1.2,linewidth = 1,
                  position =  position_dodge(width=0.8)) +
  scale_alpha_manual(values=c(1,1,1,1), guide = "none") +
  scale_shape_manual(values=c(22,21)) +
  scale_color_manual(values=c("#ffb339ff","black")) +
  
  scale_fill_manual(values=c("#ffb339ff","black")) +
  theme_classic() +
  coord_cartesian(ylim=c(min(bias_chimps2007_tr33_46$l25),
                         max(bias_chimps2007_tr33_46$u95) )) +
  scale_x_discrete(limits = c("T33","T34","T35","T36","T37","T38","T39","T40","T41",
                              "T42","T43","T44","T45","T46"))+
  
  ylab(bquote("Bias \n(sign density) ")) +
  xlab("Transects") +    
  
  ggtitle("Chimpanzee nests (2007): Transects 33 - 46") +
  
  
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


ggsave(file = "Bias_estimates_chimps_2007_tr33_46.jpg", bg = NULL, dpi = 500, width = 15, height = 10)

ggsave(file = "Bias_estimates_chimps_2007_tr33_46.svg", bg = NULL, dpi = 500, width = 15, height = 10)







