# [Improving population analysis using indirect count data: a case study of chimpanzees and elephants](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.70150)

### [Samuel Ayebare](https://github.com/samwiry), [Neil A. Gilbert](https://github.com/n-a-gilbert), Andrew J. Plumptre, Simon Nampindo, and [Elise F. Zipkin](https://github.com/ezipkin)

### Ecosphere

### Please contact the first author for questions about the code or data: Samuel Ayebare (ayebares@msu.edu)
__________________________________________________________________________________________________________________________________________

## Abstract:

Estimating spatiotemporal patterns of population density is a primary objective of wildlife monitoring programs. However, estimating density is challenging for species that are elusive and/or occur in habitats with limited visibility. In such situations, indirect measures (e.g., nests, dung) can serve as proxies for counts of individuals. Scientists have developed approaches to estimate population density using these “indirect count” data, although current methods do not adequately account for variation in sign production and spatial patterns of animal density. In this study, we describe a modified hierarchical distance-sampling model that maximizes the information content of indirect count data using Bayesian inference. We apply our model to assess the status of chimpanzee and elephant populations using counts of nests and dung, respectively, that were collected along transects in 2007 and 2021 in western Uganda. Compared to conventional methods, our modeling framework produced more precise estimates of covariate effects on expected animal density by accounting for both long-term and recent variations in animal abundance and enabled the estimation of the number of days that animal signs remained visible. We estimated a 0.98 probability that chimpanzee density in the region had declined by at least 10% and a 0.99 probability that elephant density had increased by 50% from 2007 to 2021. We recommend applying our modified hierarchical distance sampling model in the analysis of indirect count data to account for spatial variation in animal density, assess population change between survey periods, estimate the decay rate of animal signs, and obtain more precise density estimates than achievable with traditional methods.

__________________________________________________________________________________________________________________________________________

## Repository Directory

## Data

This folder contains indirect count data for chimpanzees (i.e, nests) and elephants (i.e., dung) for the 2007 and 2021 survey periods. The datasets were used to estimate the population status and change of chimpanzees and elephants in Maramagambo and Kalinzu Forest Reserves located in western Uganda across two survey periods (i.e., 2007 and 2021).

The files are in a.csv and .Rdata format (i.e., output from R programing software).

## Code

This folder contains all the code required to fit conventional, hierarchical and modified hierarchical distance sampling models.

We fit our models using R (R Core Team 2023) - with programs (Distance in R; Miller et al., 2019b) for maximum likelihood inference and JAGS (Plummer 2003) software with the jagsUI package (Kellner 2021) for Bayesian inference.
