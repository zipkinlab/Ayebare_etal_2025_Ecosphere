## Author: Neil Gilbert & Samuel Ayebare
##Script:  Figure3_effect_of_elevation.R


## Description : To plot figure3 (i.e., Effect of elevation on the expected density of chimpanzees and elephants for the 
##       2007 and 2021 surveys estimated using the modified approach and the standard approach)


#------------------------------------------------------------------------------------------------##
#------------------------------------------------------------------------------------------------##

#-----------------------#
#-Set Working Directory-#
#-----------------------#

setwd(".../Data")


#----------------#
#-Load libraries-#
#----------------#
library(tidyverse)
library(MCMCvis)
library(MetBrewer)

### Clear working environment
rm(list=ls())

# read in and rename model objects

# CHIMPS

### Standard model: 2007
load("Mara.Kal_2007.V23_chimps_hds.RData")
chimp_std07 <- Mara.Kal_2007.V23.cov

#### Standard model: 2021
load("Mara.Kal_2021.V23_chimps_hds.RData")
chimp_std21 <- Mara.Kal_2021.V23.cov

### Modified model: both years
load("Mara.Kal_2007_2021_chimps_mhds.RData")
chimp_mod <- Mara.Kal_2007_2021_chimps

# ELEPHANTS

### Standard model: 2007
load("Eles_2007_mara_hds.RData")
ele_std07 <- Mara.V23_2007_eles.cov

load("Eles_2021_mara_hds.RData")
ele_std21 <- Mara.V23_2021_eles.cov

### Modified model: both years
load("Eles_2007_2021_mara_mhds.RData")
ele_mod <- Eles_2007_2021_mara

# remove the original model objects with less clear names
rm(list = setdiff(ls(), c("chimp_std07", "chimp_std21", "chimp_mod",
                          "ele_std07", "ele_std21", "ele_mod")))

# covariates

Pred.var <- read.csv("Pred_var.csv", header=TRUE)
chimp_selev <- scale(Pred.var$elev)
ele_selev <- scale(Pred.var$elev)[1:36]
mean(ele_selev)
sd(ele_selev)

# select a subsample of posterior iterations to use to save computation time...15k is a bit excessive
iters_to_select <- sort(sample(1:15000, 2500, replace = FALSE))

# set up sequence of elevation values to predict to
chimp_pelev <- seq(from = min(chimp_selev), to = max(chimp_selev), by = 0.1)
ele_pelev <- seq(from = min(ele_selev), to = max(ele_selev), by = 0.1)

# estimated relationships from the standard model
# use whole posteriors of intercept and elevation coefficients, then take mean / quantiles
# rather than using mean / CI's of parameters to generate relationships
chimp_std_df <- MCMCvis::MCMCpstr(chimp_std07, params = "beta0", type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_std07, params = "beta1", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_std07, params = "beta2", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |>
  tibble::add_column(year = 2007,
                     sp = "Chimpanzee") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_std21, params = "beta0", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(chimp_std21, params = "beta1", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(chimp_std21, params = "beta2", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |>
      tibble::add_column(year = 2021,
                         sp = "Chimpanzee")) |> 
  dplyr::mutate(iter = parse_number(iter)) |> 
  dplyr::filter(iter %in% iters_to_select) |> 
  dplyr::cross_join( tibble(elev = chimp_pelev)) |> 
  dplyr::full_join( tibble(year = c(2007, 2021), 
                           days = c(50, 49))) |> 
  dplyr::mutate( dens = exp(beta0 + beta1 * elev + beta2 * elev * elev) / (1.09 * days) ) |> 
  dplyr::group_by(year, sp, elev) |> 
  dplyr::summarise( mean = mean(dens), 
                    l95 = quantile(dens, c(0.025)), 
                    u95 = quantile(dens, c(0.975))) |> 
  tibble::add_column(model = "Standard") |> 
  dplyr::mutate(elev_unscaled = elev*attr(chimp_selev, "scaled:scale") + attr(chimp_selev, "scaled:center"))

chimp_mod_df <-
  MCMCvis::MCMCpstr(chimp_mod, params = "beta0.V23.2007", type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_mod, params = "beta1.07", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_mod, params = "beta2.07", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |> 
  tibble::add_column(year = 2007,
                     sp = "Chimpanzee") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(chimp_mod, params = "beta0.V23.2021", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(chimp_mod, params = "beta1.21", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(chimp_mod, params = "beta2.21", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |> 
      tibble::add_column(year = 2021,
                         sp = "Chimpanzee")) |> 
  dplyr::mutate(iter = parse_number(iter)) |> 
  dplyr::filter(iter %in% iters_to_select) |> 
  dplyr::cross_join( tibble(elev = chimp_pelev)) |>
  dplyr::full_join( tibble(year = c(2007, 2021), 
                           days = c(50, 49))) |> 
  
  dplyr::mutate(dens = exp(beta0 + beta1 * elev + beta2 * elev * elev) / (1.09 * days)) |> 
  dplyr::group_by(year, sp, elev) |> 
  dplyr::summarise( mean = mean(dens), 
                    l95 = quantile(dens, c(0.025)), 
                    u95 = quantile(dens, c(0.975))) |> 
  tibble::add_column(model = "Modified") |> 
  dplyr::mutate(elev_unscaled = elev*attr(chimp_selev, "scaled:scale") + attr(chimp_selev, "scaled:center"))

ele_std_df <-
  MCMCvis::MCMCpstr(ele_std07, params = "beta0", type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_std07, params = "beta1", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_std07, params = "beta2", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |>
  tibble::add_column(year = 2007,
                     sp = "Elephant") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_std21, params = "beta0", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(ele_std21, params = "beta1", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(ele_std21, params = "beta2", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |>
      tibble::add_column(year = 2021,
                         sp = "Elephant")) |> 
  dplyr::mutate(iter = parse_number(iter)) |> 
  dplyr::filter(iter %in% iters_to_select) |> 
  dplyr::cross_join( tibble(elev = ele_pelev)) |> 
  dplyr::full_join( tibble(year = c(2007, 2021), 
                           days = c(50, 49))) |> 
  dplyr::mutate( dens = exp(beta0 + beta1 * elev + beta2 * elev * elev) / (17 * days) ) |> 
  dplyr::group_by(year, sp, elev) |> 
  dplyr::summarise( mean = mean(dens), 
                    l95 = quantile(dens, c(0.025)), 
                    u95 = quantile(dens, c(0.975))) |> 
  tibble::add_column(model = "Standard") |> 
  dplyr::mutate(elev_unscaled = elev*attr(chimp_selev, "scaled:scale") + attr(chimp_selev, "scaled:center"))

ele_mod_df <-
  MCMCvis::MCMCpstr(ele_mod, params = "beta0.V23.2007", type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_mod, params = "beta1.07", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_mod, params = "beta2.07", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |> 
  tibble::add_column(year = 2007,
                     sp = "Elephant") |> 
  dplyr::full_join(
    MCMCvis::MCMCpstr(ele_mod, params = "beta0.V23.2021", type = "chains")[[1]] |> 
      tibble::as_tibble() |> 
      tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta0") |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(ele_mod, params = "beta1.21", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta1")) |> 
      dplyr::full_join(
        MCMCvis::MCMCpstr(ele_mod, params = "beta2.21", type = "chains")[[1]] |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(1:15000, names_to = "iter", values_to = "beta2")) |> 
      tibble::add_column(year = 2021,
                         sp = "Elephant")) |> 
  dplyr::mutate(iter = parse_number(iter)) |> 
  dplyr::filter(iter %in% iters_to_select) |> 
  dplyr::cross_join( tibble(elev = ele_pelev)) |>
  dplyr::full_join( tibble(year = c(2007, 2021), 
                           days = c(50, 49))) |> 
  
  dplyr::mutate(dens = exp(beta0 + beta1 * elev + beta2 * elev * elev) / (17 * days)) |> 
  dplyr::group_by(year, sp, elev) |> 
  dplyr::summarise( mean = mean(dens), 
                    l95 = quantile(dens, c(0.025)), 
                    u95 = quantile(dens, c(0.975))) |> 
  tibble::add_column(model = "Modified") |> 
  dplyr::mutate(elev_unscaled = elev*attr(chimp_selev, "scaled:scale") + attr(chimp_selev, "scaled:center"))

pal <- c("blue","#D55E00")

full_join(chimp_std_df, 
          chimp_mod_df) |> 
  full_join(ele_std_df) |> 
  full_join(ele_mod_df) |> 
  ggplot(aes(x = elev_unscaled, y = mean, color = model)) + 
  facet_grid(sp~year, scales = "free_y") +
  geom_ribbon(aes(ymin = l95, ymax = u95, fill = model), color = NA, alpha = 0.5) + 
  geom_line(size = 1.5) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  
  labs(x = "Elevation (m)", 
       y = "Density (animals per)" ~ km^2, 
       fill = "Model", 
       color = "Model") + 
  scale_x_continuous(breaks = c(1000, 1250, 1500)) +
  theme_classic() +
  theme( strip.background = element_rect(color = NA), 
         text = element_text(family = "Arial", size = 42),
         axis.line = element_line(color = "black", size = 0.3),
         axis.ticks = element_line(color = "black", size = 0.2), 
         axis.text = element_text(color = "black", size = 10), 
         axis.title = element_text(color = "black", size = 11), 
         strip.text = element_text(color = "black", size = 11), 
         legend.title = element_text(color = "black", size = 11), 
         legend.text = element_text(color = "black", size = 10), 
         legend.position = "bottom")

ggsave(
  "Figure3_density_elevation_relationships.jpg", 
  width = 4,
  height = 4, 
  units = "in", 
  dpi = 300
)



