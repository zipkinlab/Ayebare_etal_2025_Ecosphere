# Data - conventional distance sampling model (maximum likelihood inference) - Visits 2 & 3

1.  **`Kalinzu_Mara_combined_R2_2007.csv`**: input file (2007 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu combined (default variance estimator - "R2").

2.  **`Kalinzu_Mara_combined_R2_2021.csv`**: input file (2021 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu combined (default variance estimator - "R2").

3.  **`Kalinzu_Mara_R2_2007.csv`**: input file (2007 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu seperately (default variance estimator - "R2").

4.  **`Kalinzu_Mara_R2_2021.csv`**: input file (2021 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu seperately (default variance estimator - "R2").

5.  **`Elephant_mara_R2_2007.csv`**: input file (2007 survey) to estimate the population density of elephants in Maramagambo (default variance estimator - "R2").

6.  **`Elephant_mara_R2_2021.csv`**: input file (2021 survey) to estimate the population density of elephants in Maramagambo (default variance estimator - "R2").

    -   **Variables**

-   Region.Label: name of protected area
-   Area: size of protected area
-   Sample.Label: unique identification for each line transect
-   object: unique identification for detected signs (i.e., nests or dung)
-   Effort: Length of each transect (km)
-   distance: perpendicular distances in meters
-   dist:     distance categories used in Bayesian inference


7.  **`Kalinzu_Mara_combined_S2_O2_2007.csv`**: input file (2007 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu combined using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

8. **`Kalinzu_Mara_combined_S2_O2_2021.csv`**: input file (2021 survey) to estimate the population density of chimpanzees in Maramagambo and Kalinzu combined using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

9.  **`Mara_S2_O2_2007.csv`**: input file (2007 survey) to estimate the population density of chimpanzees in Maramagambo using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

10. **`Mara_S2_O2_2021.csv`**: input file (2021 survey) to estimate the population density of chimpanzees in Maramagambo using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

11.  **`Kalinzu_S2_O2_2007.csv`**: input file (2007 survey) to estimate the population density of chimpanzees in Kalinzu using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

12.  **`Kalinzu_S2_O2_2021.csv`**: input file (2021 survey) to estimate the population density of chimpanzees in Kalinzu using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

13.  **`Elephant_mara_S2_O2_2007.csv`**: input file (2007 survey) to estimate the population density of elephants in Maramagambo using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").

14. **`Elephant_mara_S2_O2_2021.csv`**: input file (2021 survey) to estimate the population density of elephants in Maramagambo using a conventional distance sampling model (post-stratification based variance estimators- "S2" & "O2").


    -  **Variables**

-   Region.Label: name of protected area.
-   Area: size of protected area.
-   object: unique identification for detected signs (i.e., nests or dung).
-   Sample.Label.original: unique identification for each line transect before post-stratification.
-   distance: perpendicular distances in meters.
-   dist:     distance categories used in Bayesian inference.
-   Effort: Length of each transect (km).
-   Sample.Label.original: unique identification for line transect after post-stratification.
-   grouping: unique identification for each stratum.


# Data -conventional, hierarchical  and modified hierarchical distance sampling models (Bayesian inference)

1.  **`Kalinzu_mara_V1_2007.csv`**: input file (2007 survey) - contains Visit 1 data (distance categories) for chimpanzees (nests). 
2.  **`Kalinzu_mara_V23_2007.csv`**: input file (2007 survey) - contains Visits 2 & 3 data (distance categories) for chimpanzees (nests).
3.  **`Kalinzu_mara_V1_2021.csv`**: input file (2021 survey) - contains Visit 1 data (distance categories) for chimpanzees (nests).
4.  **`Kalinzu_mara_V23_2021.csv`**: input file (2021 survey) - contains Visits 2 & 3 data (distance categories) for chimpanzees (nests). 
5.  **`Ele_V1_2007.csv`**: input file (2007 survey) - contains Visit 1 data (distance categories) for elephants (dung).
6.  **`Ele_V23_2007.csv`**: input file (2007 survey) - contains Visits 2 & 3 data (distance categories) for elephants (dung).
7.  **`Ele_V1_2021.csv`**: input file (2021 survey) - contains Visit 1 data (distance categories) for elephants (dung).
8.  **`Ele_V23_2021.csv`**: input file (2021 survey) - contains Visits 2 & 3 data (distance categories) for elephants (dung).


    -   **Variables**


-   Tr: unique identification for each line transect. 
-   Visit: unique identification for each sampling event.
-   cnest: number of nest observed in each distance category.
-   eldung: number of dung observed in each distance category.
-   dist:  distance categories 
         - chimpanzees: (0 - 5m = dist[1]); (5 - 10m = dist[2]);(10 - 15m = dist[3]); (15 - 20m = dist[4]);(20 - 25m = dist[5]).
         - elephants: (0 - 1m = dist[1]); (1 - 2m = dist[2]);(2 - 3m = dist[3]); (3 - 4m = dist[4]);(4 - 5m = dist[5]).




9.  **`Kalinzu_mara_2007.csv`**: input file (2007 survey) - contains Visits 1, 2, & 3 data (number of nests observed per transect) for chimpanzees.
10.  **`Kalinzu_mara_2021.csv`**: input file (2021 survey)  - contains Visits 1, 2, & 3 data (number of nests observed per transect) for chimpanzees.
11.  **`Ele_2007.csv`**: input file (2007 survey) - contains Visits 1, 2, & 3 data (number of dung observed per transect) for elephants.
12.  **`Ele_2021.csv`**: input file (2021 survey)  - contains Visits 1, 2, & 3 data (number of dung observed per transect) for elephants.

        -   **Variables**

-   Tr: unique identification for each line transect. 
-   L: Length of each transect (km).
-   V1: number signs (ie., nests or dung) observed during Visit 1.
-   V2: number signs (ie., nests or dung) observed during Visit 2.
-   V3: number signs (ie., nests or dung) observed during Visit 3.
-   V23: number signs (ie., nests or dung) observed during Visits 2 & 3 combined.
-   A: area surveyed for each trasect in km squared.


13.  **`Pred_var.csv`**: predictor variables

     -   **Variables**
-   Tr: unique identification for each line transect. 
-   bio1: annual mean temperature (Celicius); https://www.worldclim.org/data/bioclim.html
-   bio2: annual mean precipitation (mm); https://www.worldclim.org/data/bioclim.html
-   elev: elevation (meters).


# Data - model results

1. Mara.Kal_2007.V23_chimps_cds.RData: - model results (2007 survey) for chimpanzees from a conventional distance sampling model (Bayesian inference).
2. Mara.Kal_2021.V23_chimps_cds.RData: - model results (2021 survey) for chimpanzees from a conventional distance sampling model (Bayesian inference).
3. Mara.Kal_2007.V23_chimps_hds.RData: - model results (2007 survey) for chimpanzees from a hierarchical distance sampling model (Bayesian inference).
4. Mara.Kal_2007.V23_chimps_hds.RData: - model results (2021 survey) for chimpanzees from a hierarchical distance sampling model (Bayesian inference).
5. Mara.Kal_2007_2021.V23_chimps_mhds.RData: - model results (2007 & 2021 surveys) for chimpanzees from a modified hierarchical distance sampling model (Bayesian inference).
					## File greater than 25mb Github file size limit - Run this script `MHDS_Maramagambo_Kalinzu_chimpanzees_2007_2021.R` to obtain model results.
6. Eles_2007_mara_cds.RData: - model results (2007 survey) for elephants from a conventional distance sampling model (Bayesian inference).
7. Eles_2021_mara_cds.RData: - model results (2007 survey) for elephants from a conventional distance sampling model (Bayesian inference).
8. Eles_2007_mara_hds.RData: - model results (2007 survey) for elephants from a heirarchical distance sampling model (Bayesian inference).
9. Eles_2007_mara_hds.RData: - model results (2021 survey) for elephants from a heirarchical distance sampling model (Bayesian inference).
10. Eles_2007_2021_mara_mhds.RData: - model results (2007 & 2021 surveys) for elephants from a modified hierarchical distance sampling model (Bayesian inference).
                                     ## File greater than 25mb Github file size limit - Run this script `MHDS_Maramagambo_elephants_2007_2021.R` to obtain model results.

# Data - bias estimates

1.  **`Bias_chimps_hds_mhds_2021.csv`**: Estimated bias of the sign density for chimpanzees (2021) used to plot Appendix S1:Figures S3.
2.  **`Bias_elephants_hds_mhds_2021.csv`**: Estimated bias of the sign density for elephants (2021) used to plot Appendix S1:Figures S3.
3.   **`Bias_chimps_hds_mhds_2007.csv`**: Estimated bias of the sign density for chimpanzees (2007) used to plot Appendix S1:Figures S4.
4.   **`Bias_elephants_hds_mhds_2007.csv`**: Estimated bias of the sign density for elephants (2007) used to plot Appendix S1:Figures S4.

    -   **Variables**
-   Transects: unique identification for each line transect (character).
-   Tr_id: unique identification for each line transect (numeric).
-   Model: unique identifier indicating model membership for either MHDS (i.e., modified hierarchical distance sampling model) and HDS (i.e., hierarchical distance sampling model).
-   sd: standard deviation.
-   l25: lower 95% credible interval.
-   u25: upper 95% credible interval.


# Data - estimating relative bias

1.  **`Bias_hds_2021_chimps_transects.csv`**:  Estimated bias of the sign density for chimpanzees (2021) used to estimate relative bias for a hierarchical distance sampling model.
2.  **`Bias_hds_2021_eles_transects.csv`**:   Estimated bias of the sign density for elephants (2021) used to estimate relative bias for a hierarchical distance sampling model.
3.  **`Bias_hds_2007_chimps_transects.csv`**:  Estimated bias of the sign density for chimpanzees (2007) used to estimate relative bias for a hierarchical distance sampling model.
4.  **`Bias_hds_2007_eles_transects.csv`**:  Estimated bias of the sign density for elephants (2007) used to estimate relative bias for a hierarchical distance sampling model.
5.  **`Bias_mhds_2021_chimps_transects.csv`**:  Estimated bias of the sign density for chimpanzees (2021) used to estimate relative bias for a modified hierarchical distance sampling model.
6.  **`Bias_mhds_2021_eles_transects.csv`**:   Estimated bias of the sign density for elephants (2021) used to estimate relative bias for a modified hierarchical distance sampling model.
7.  **`Bias_mhds_2007_chimps_transects.csv`**:  Estimated bias of the sign density for chimpanzees (2007) used to estimate relative bias for a modified hierarchical distance sampling model.
8.  **`Bias_mhds_2007_eles_transects.csv`**:  Estimated bias of the sign density for elephants (2007) used to estimate relative bias for a modified hierarchical distance sampling model.


    -   **Variables**
-   transects: unique identification for each line transect (character).
-   Model: unique identifier indicating model membership for either MHDS (i.e., modified hierarchical distance sampling model) and HDS (i.e., hierarchical distance sampling model).
-   bias_per_transect: estimated bias per transect (expected number of signs – observed number of signs)
-   observed_dung: number of elephant dung observed at each omitted transect.
-   observed_nests: number of chimpanzee nests observed at each omitted transect.


# # Data - protected area boundaries & elevation raster

1.  **`Kalinzu_FR.**`**:  (4 files) -  boundary for Kalinzu Forest Reserve. 
2.  **`Maramagambo_FR.**`**:  (4 files) -  boundary for Maramagambo Forest Reserve. 
3.  **`Maramagambo_Kalinzu_FRs.**`**:  (4 files) -  boundary for Maramagambo and Kalinzu Forest Reserves combined. 
4.  **`elev_kal.mara.**`**:  (2 files) -  elevation raster for Maramagambo and Kalinzu Forest Reserves combined. 

